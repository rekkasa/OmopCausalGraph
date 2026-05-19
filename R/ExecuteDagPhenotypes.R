#' Execute phenotype cohorts for all required DAG nodes
#'
#' @description
#' Calls `validateOmopCausalGraph()`, computes the required node set (exposures + outcomes +
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
#' @param omopCausalGraph An `OmopCausalGraph` object with validation passing.
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
#' result <- executeOmopCausalGraphPhenotypes(omopCausalGraph, cd, "main", "main", "cohort")
#' }
#'
#' @family execution
#' @export
executeOmopCausalGraphPhenotypes <- function(omopCausalGraph,
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

  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }

  validation  <- validateOmopCausalGraph(omopCausalGraph)
  hardChecks  <- c("has_exposure", "has_outcome")
  softChecks  <- c("adjustment_sets_recorded", "bindings_present")

  for (checkName in hardChecks) {
    if (!validation$checks[[checkName]]$pass) {
      stop(sprintf("DAG validation failed [%s]: %s", checkName, validation$checks[[checkName]]$message), call. = FALSE)
    }
  }
  if (!allowIncomplete) {
    for (checkName in softChecks) {
      if (!is.null(validation$checks[[checkName]]) && !validation$checks[[checkName]]$pass) {
        stop(sprintf("DAG validation failed [%s]: %s\nPass allowIncomplete = TRUE to override.", checkName, validation$checks[[checkName]]$message), call. = FALSE)
      }
    }
  }

  roles     <- omopCausalGraph$roles()
  exposures <- roles$node[roles$role == "exposure"]
  outcomes  <- roles$node[roles$role == "outcome"]
  adjSets   <- omopCausalGraph$adjustmentSets()
  bindings  <- omopCausalGraph$bindings()

  for (exposure in exposures) {
    for (outcome in outcomes) {
      availableSets <- tryCatch(
        as.list(dagitty::adjustmentSets(omopCausalGraph$dagittyGraph(), exposure = exposure, outcome = outcome, type = "minimal")),
        error = function(omopCausalGraphError) list()
      )
      if (length(availableSets) == 0L) {
        msg <- sprintf(
          "No valid adjustment set exists for '%s -> %s'. The query is unidentifiable.\nPossible cause: unobserved confounder blocks identification.\nUse allowIncomplete = TRUE with prominent bias warning.",
          exposure, outcome
        )
        if (!allowIncomplete) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
      }
    }
  }

  adjNodes       <- unlist(lapply(adjSets, `[[`, "nodes"), use.names = FALSE)
  requiredNodes  <- unique(c(exposures, outcomes, adjNodes))

  requiredNodes <- Filter(function(nodeName) {
    nodeBinding <- bindings[[nodeName]]
    if (is.null(nodeBinding)) return(TRUE)
    activeDef <- nodeBinding$alternatives[[nodeBinding$active]]
    !identical(activeDef$type, "demographics")
  }, requiredNodes)

  unresolvedBindings <- character(0)
  for (nodeName in requiredNodes) {
    nodeBinding <- bindings[[nodeName]]
    if (is.null(nodeBinding)) {
      unresolvedBindings <- c(unresolvedBindings, sprintf("Node '%s': no binding", nodeName))
      next
    }
    activeDef <- nodeBinding$alternatives[[nodeBinding$active]]
    if (identical(activeDef$type, "PhenotypeLibrary") && !isTRUE(activeDef$resolved)) {
      unresolvedBindings <- c(unresolvedBindings, sprintf(
        "Node '%s': PhenotypeLibrary binding not resolved. Call resolvePhenotypes() first.", nodeName
      ))
    }
  }
  if (length(unresolvedBindings) > 0) {
    stop(paste(c("Unresolved bindings:", unresolvedBindings), collapse = "\n  "), call. = FALSE)
  }

  cohortDefs <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in seq_along(requiredNodes)) {
    nodeName  <- requiredNodes[i]
    activeDef <- bindings[[nodeName]]$alternatives[[bindings[[nodeName]]$active]]
    jsonStr   <- activeDef$json
    cohortDefs <- rbind(cohortDefs, data.frame(
      cohortId         = i,
      cohortName       = nodeName,
      json             = jsonStr,
      stringsAsFactors = FALSE
    ))
  }

  dbConnection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(dbConnection), add = TRUE)

  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)
  CohortGenerator::createCohortTables(
    connection           = dbConnection,
    cohortDatabaseSchema = resultsSchema,
    cohortTableNames     = cohortTableNames
  )
  CohortGenerator::generateCohortSet(
    connection           = dbConnection,
    cdmDatabaseSchema    = cdmSchema,
    cohortDatabaseSchema = resultsSchema,
    cohortTableNames     = cohortTableNames,
    cohortDefinitionSet  = cohortDefs
  )

  rowCounts <- vapply(seq_along(requiredNodes), function(i) {
    sql <- sprintf(
      "SELECT COUNT(*) AS n FROM %s.%s WHERE cohort_definition_id = %d",
      resultsSchema, cohortTable, i
    )
    result <- DatabaseConnector::renderTranslateQuerySql(dbConnection, sql)
    as.integer(result$N[1])
  }, integer(1))

  data.frame(
    node         = requiredNodes,
    cohortId     = seq_along(requiredNodes),
    binding_type = vapply(requiredNodes, function(nodeName) {
      bindings[[nodeName]]$alternatives[[bindings[[nodeName]]$active]]$type
    }, character(1)),
    row_count    = rowCounts,
    stringsAsFactors = FALSE
  )
}
