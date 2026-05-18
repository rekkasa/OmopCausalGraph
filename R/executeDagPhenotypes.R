#' Execute phenotype cohorts for all required DAG nodes
#'
#' @description
#' Calls `validateDag()`, computes the required node set (exposures + outcomes +
#' chosen adjustment set members), generates cohorts via `CohortGenerator`,
#' and returns a summary table.
#'
#' Hard-errors on:
#' - Validation failures (subject to `allowIncomplete` for binding/adjustment checks).
#' - Any active `PhenotypeLibrary` binding that is still in reference mode.
#' - Any exposure-outcome pair with no valid adjustment set (unless
#'   `allowIncomplete = TRUE`, which emits a prominent warning and proceeds).
#'
#' Requires the `CohortGenerator` and `DatabaseConnector` packages (listed as
#' `Suggests`); errors with install instructions if missing.
#'
#' @param dag An `OmopDag` object with validation passing.
#' @param connectionDetails A `DatabaseConnector` connection details object.
#' @param cdmSchema CDM database schema name.
#' @param resultsSchema Results/cohort database schema name.
#' @param cohortTable Cohort table name.
#' @param allowIncomplete Logical. If `TRUE`, proceed even if adjustment sets are
#'   not recorded or identifiability cannot be confirmed; emits a warning. Default
#'   `FALSE`.
#'
#' @return A `data.frame` with columns `node`, `cohortId`, `binding_type`,
#'   `row_count`.
#'
#' @examples
#' \dontrun{
#' cd <- DatabaseConnector::createConnectionDetails(
#'   dbms = "duckdb", server = Eunomia::getEunomiaConnectionDetails()$server
#' )
#' result <- executeDagPhenotypes(dag, cd, "main", "main", "cohort")
#' }
#'
#' @family execution
#' @export
executeDagPhenotypes <- function(dag,
                                  connectionDetails,
                                  cdmSchema,
                                  resultsSchema,
                                  cohortTable,
                                  allowIncomplete = FALSE) {
  if (!requireNamespace("CohortGenerator",  quietly = TRUE)) {
    stop("Package 'CohortGenerator' required. Install: remotes::install_github('OHDSI/CohortGenerator')", call. = FALSE)
  }
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    stop("Package 'DatabaseConnector' required. Install: remotes::install_github('OHDSI/DatabaseConnector')", call. = FALSE)
  }

  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }

  validation <- validateDag(dag)
  hard_checks <- c("has_exposure", "has_outcome")
  soft_checks <- c("adjustment_sets_recorded", "bindings_present")

  for (chk in hard_checks) {
    if (!validation$checks[[chk]]$pass) {
      stop(sprintf("DAG validation failed [%s]: %s", chk, validation$checks[[chk]]$message), call. = FALSE)
    }
  }
  if (!allowIncomplete) {
    for (chk in soft_checks) {
      if (!is.null(validation$checks[[chk]]) && !validation$checks[[chk]]$pass) {
        stop(sprintf("DAG validation failed [%s]: %s\nPass allowIncomplete = TRUE to override.", chk, validation$checks[[chk]]$message), call. = FALSE)
      }
    }
  }

  roles        <- dag$roles()
  exposures    <- roles$node[roles$role == "exposure"]
  outcomes     <- roles$node[roles$role == "outcome"]
  adj_sets     <- dag$adjustment_sets()
  bindings     <- dag$bindings()

  # Check identifiability per pair
  for (exp in exposures) {
    for (out in outcomes) {
      sets <- tryCatch(
        as.list(dagitty::adjustmentSets(dag$dagitty_graph(), exposure = exp, outcome = out, type = "minimal")),
        error = function(e) list()
      )
      if (length(sets) == 0L) {
        msg <- sprintf(
          "No valid adjustment set exists for '%s -> %s'. The query is unidentifiable.\nPossible cause: unobserved confounder blocks identification.\nUse allowIncomplete = TRUE with prominent bias warning.",
          exp, out
        )
        if (!allowIncomplete) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
      }
    }
  }

  # Compute required nodes
  adj_nodes     <- unlist(lapply(adj_sets, `[[`, "nodes"), use.names = FALSE)
  required_nodes <- unique(c(exposures, outcomes, adj_nodes))

  # Remove demographics-only nodes
  required_nodes <- Filter(function(n) {
    b <- bindings[[n]]
    if (is.null(b)) return(TRUE)
    active_def <- b$alternatives[[b$active]]
    !identical(active_def$type, "demographics")
  }, required_nodes)

  # Check all required nodes have resolved bindings
  unresolved <- character(0)
  for (n in required_nodes) {
    b <- bindings[[n]]
    if (is.null(b)) {
      unresolved <- c(unresolved, sprintf("Node '%s': no binding", n))
      next
    }
    active_def <- b$alternatives[[b$active]]
    if (identical(active_def$type, "PhenotypeLibrary") && !isTRUE(active_def$resolved)) {
      unresolved <- c(unresolved, sprintf(
        "Node '%s': PhenotypeLibrary binding not resolved. Call resolvePhenotypes() first.", n
      ))
    }
  }
  if (length(unresolved) > 0) {
    stop(paste(c("Unresolved bindings:", unresolved), collapse = "\n  "), call. = FALSE)
  }

  # Build cohort definition set
  cohort_defs <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in seq_along(required_nodes)) {
    n          <- required_nodes[i]
    active_def <- bindings[[n]]$alternatives[[bindings[[n]]$active]]
    json_str   <- active_def$json
    cohort_defs <- rbind(cohort_defs, data.frame(
      cohortId          = i,
      cohortName        = n,
      json              = json_str,
      stringsAsFactors  = FALSE
    ))
  }

  # Generate cohorts
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

  cohort_table_names <- CohortGenerator::getCohortTableNames(cohortTable)
  CohortGenerator::createCohortTables(
    connection          = conn,
    cohortDatabaseSchema = resultsSchema,
    cohortTableNames    = cohort_table_names
  )
  CohortGenerator::generateCohortSet(
    connection          = conn,
    cdmDatabaseSchema   = cdmSchema,
    cohortDatabaseSchema = resultsSchema,
    cohortTableNames    = cohort_table_names,
    cohortDefinitionSet = cohort_defs
  )

  # Retrieve row counts
  row_counts <- vapply(seq_along(required_nodes), function(i) {
    sql <- sprintf(
      "SELECT COUNT(*) AS n FROM %s.%s WHERE cohort_definition_id = %d",
      resultsSchema, cohortTable, i
    )
    res <- DatabaseConnector::renderTranslateQuerySql(conn, sql)
    as.integer(res$N[1])
  }, integer(1))

  data.frame(
    node         = required_nodes,
    cohortId     = seq_along(required_nodes),
    binding_type = vapply(required_nodes, function(n) {
      bindings[[n]]$alternatives[[bindings[[n]]$active]]$type
    }, character(1)),
    row_count    = row_counts,
    stringsAsFactors = FALSE
  )
}
