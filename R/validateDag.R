#' Validate an OmopDag before execution
#'
#' @description
#' Runs a battery of named checks and returns a structured result. Never
#' throws — callers decide how to react to failures. Called internally by
#' `executeDagPhenotypes()`.
#'
#' Checks performed:
#' - `has_exposure`: at least one node with role `exposure`.
#' - `has_outcome`: at least one node with role `outcome`.
#' - `bindings_present`: every execution-required node (exposures, outcomes,
#'   chosen adjustment set members) has an active binding that is not
#'   `demographics`-only.
#' - `adjustment_sets_recorded`: every exposure-outcome pair has a chosen
#'   adjustment set entry.
#' - `no_adjusted_collider`: warns when any `adjusted` node is a collider.
#'
#' @param dag An `OmopDag` object.
#'
#' @return A list with:
#'   - `checks`: named list, each element `list(pass = logical, message = character)`.
#'   - `overall`: `TRUE` if all checks pass.
#'
#' @examples
#' dag <- emptyOmopDag("Example")
#' dag <- addNode(dag, "Exposure", 1L, "Drug")
#' dag <- addNode(dag, "Outcome",  2L, "Condition")
#' dag <- setExposure(dag, "Exposure")
#' dag <- setOutcome(dag,  "Outcome")
#' result <- validateDag(dag)
#' result$overall
#'
#' @family validation
#' @export
validateDag <- function(dag) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }

  roles <- dag$roles()
  checks <- list()

  # has_exposure
  exposures <- roles$node[roles$role == "exposure"]
  checks$has_exposure <- list(
    pass    = length(exposures) >= 1L,
    message = if (length(exposures) >= 1L) "OK" else "No exposure node set. Use setExposure()."
  )

  # has_outcome
  outcomes <- roles$node[roles$role == "outcome"]
  checks$has_outcome <- list(
    pass    = length(outcomes) >= 1L,
    message = if (length(outcomes) >= 1L) "OK" else "No outcome node set. Use setOutcome()."
  )

  # adjustment_sets_recorded
  adj_sets <- dag$adjustment_sets()
  if (length(exposures) > 0 && length(outcomes) > 0) {
    pairs_needed <- as.vector(outer(exposures, outcomes,
                                    FUN = function(e, o) paste0(e, "__", o)))
    missing_pairs <- setdiff(pairs_needed, names(adj_sets))
    checks$adjustment_sets_recorded <- list(
      pass    = length(missing_pairs) == 0L,
      message = if (length(missing_pairs) == 0L) "OK" else sprintf(
        "No adjustment set chosen for pair(s): %s. Use setAdjustmentSet().",
        paste(missing_pairs, collapse = ", ")
      )
    )
  } else {
    checks$adjustment_sets_recorded <- list(
      pass    = FALSE,
      message = "Cannot check adjustment sets: no exposure or outcome defined."
    )
  }

  # bindings_present
  required_nodes <- unique(c(
    exposures, outcomes,
    unlist(lapply(adj_sets, `[[`, "nodes"), use.names = FALSE)
  ))
  bindings <- dag$bindings()
  unbound <- character(0)
  demo_only <- character(0)
  for (n in required_nodes) {
    b <- bindings[[n]]
    if (is.null(b)) {
      unbound <- c(unbound, n)
    } else {
      active_def <- b$alternatives[[b$active]]
      if (!is.null(active_def) && identical(active_def$type, "demographics")) {
        demo_only <- c(demo_only, n)
      }
    }
  }
  bp_pass <- length(unbound) == 0L && length(demo_only) == 0L
  bp_msg  <- if (bp_pass) "OK" else {
    msgs <- character(0)
    if (length(unbound)   > 0) msgs <- c(msgs, paste("No binding:", paste(unbound,   collapse = ", ")))
    if (length(demo_only) > 0) msgs <- c(msgs, paste("Demographics-only (no cohort):", paste(demo_only, collapse = ", ")))
    paste(msgs, collapse = "; ")
  }
  checks$bindings_present <- list(pass = bp_pass, message = bp_msg)

  # no_adjusted_collider (warning-class check)
  adjusted_nodes <- roles$node[roles$role == "adjusted"]
  collider_warn  <- character(0)
  if (length(adjusted_nodes) > 0 && length(exposures) > 0 && length(outcomes) > 0) {
    g <- tryCatch(dagitty::ancestralGraph(dag$dagitty_graph()), error = function(e) NULL)
    if (!is.null(g)) {
      for (n in adjusted_nodes) {
        parents <- dagitty::parents(dag$dagitty_graph(), n)
        if (length(parents) >= 2L) collider_warn <- c(collider_warn, n)
      }
    }
  }
  checks$no_adjusted_collider <- list(
    pass    = length(collider_warn) == 0L,
    message = if (length(collider_warn) == 0L) "OK" else sprintf(
      "Potentially adjusted collider(s): %s. Conditioning on a collider can open a backdoor path.",
      paste(collider_warn, collapse = ", ")
    )
  )

  overall <- all(vapply(checks, `[[`, logical(1), "pass"))
  list(checks = checks, overall = overall)
}
