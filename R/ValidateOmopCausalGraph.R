#' Validate an OmopCausalGraph before execution
#'
#' @description
#' Runs a battery of named checks and returns a structured result. Never
#' throws — callers decide how to react to failures. Called internally by
#' `executeOmopCausalGraphPhenotypes()`.
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
#' @param omopCausalGraph An `OmopCausalGraph` object.
#'
#' @return A list with:
#'   - `checks`: named list, each element `list(pass = logical, message = character)`.
#'   - `overall`: `TRUE` if all checks pass.
#'
#' @examples
#' omopCausalGraph <- emptyOmopCausalGraph("Example")
#' omopCausalGraph <- addNode(omopCausalGraph, "Exposure", 1L, "Drug")
#' omopCausalGraph <- addNode(omopCausalGraph, "Outcome",  2L, "Condition")
#' omopCausalGraph <- setExposure(omopCausalGraph, "Exposure")
#' omopCausalGraph <- setOutcome(omopCausalGraph,  "Outcome")
#' result <- validateOmopCausalGraph(omopCausalGraph)
#' result$overall
#'
#' @family validation
#' @export
validateOmopCausalGraph <- function(omopCausalGraph) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }

  roles  <- omopCausalGraph$roles()
  checks <- list()

  exposures <- roles$node[roles$role == "exposure"]
  checks$has_exposure <- list(
    pass    = length(exposures) >= 1L,
    message = if (length(exposures) >= 1L) "OK" else "No exposure node set. Use setExposure()."
  )

  outcomes <- roles$node[roles$role == "outcome"]
  checks$has_outcome <- list(
    pass    = length(outcomes) >= 1L,
    message = if (length(outcomes) >= 1L) "OK" else "No outcome node set. Use setOutcome()."
  )

  adjSets <- omopCausalGraph$adjustmentSets()
  if (length(exposures) > 0 && length(outcomes) > 0) {
    pairsNeeded  <- as.vector(outer(exposures, outcomes,
                                    FUN = function(exposure, outcome) paste0(exposure, "__", outcome)))
    missingPairs <- setdiff(pairsNeeded, names(adjSets))
    checks$adjustment_sets_recorded <- list(
      pass    = length(missingPairs) == 0L,
      message = if (length(missingPairs) == 0L) "OK" else sprintf(
        "No adjustment set chosen for pair(s): %s. Use setAdjustmentSet().",
        paste(missingPairs, collapse = ", ")
      )
    )
  } else {
    checks$adjustment_sets_recorded <- list(
      pass    = FALSE,
      message = "Cannot check adjustment sets: no exposure or outcome defined."
    )
  }

  requiredNodes <- unique(c(
    exposures, outcomes,
    unlist(lapply(adjSets, `[[`, "nodes"), use.names = FALSE)
  ))
  bindings        <- omopCausalGraph$bindings()
  unboundNodes    <- character(0)
  demographicsOnly <- character(0)
  for (nodeName in requiredNodes) {
    nodeBinding <- bindings[[nodeName]]
    if (is.null(nodeBinding)) {
      unboundNodes <- c(unboundNodes, nodeName)
    } else {
      activeDef <- nodeBinding$alternatives[[nodeBinding$active]]
      if (!is.null(activeDef) && identical(activeDef$type, "demographics")) {
        demographicsOnly <- c(demographicsOnly, nodeName)
      }
    }
  }
  bindingsPass <- length(unboundNodes) == 0L && length(demographicsOnly) == 0L
  bindingsMsg  <- if (bindingsPass) "OK" else {
    msgs <- character(0)
    if (length(unboundNodes)    > 0) msgs <- c(msgs, paste("No binding:", paste(unboundNodes,    collapse = ", ")))
    if (length(demographicsOnly) > 0) msgs <- c(msgs, paste("Demographics-only (no cohort):", paste(demographicsOnly, collapse = ", ")))
    paste(msgs, collapse = "; ")
  }
  checks$bindings_present <- list(pass = bindingsPass, message = bindingsMsg)

  adjustedNodes    <- roles$node[roles$role == "adjusted"]
  colliderWarnings <- character(0)
  if (length(adjustedNodes) > 0 && length(exposures) > 0 && length(outcomes) > 0) {
    ancestralGraph <- tryCatch(dagitty::ancestralGraph(omopCausalGraph$dagittyGraph()), error = function(omopCausalGraphError) NULL)
    if (!is.null(ancestralGraph)) {
      for (nodeName in adjustedNodes) {
        nodeParents <- dagitty::parents(omopCausalGraph$dagittyGraph(), nodeName)
        if (length(nodeParents) >= 2L) colliderWarnings <- c(colliderWarnings, nodeName)
      }
    }
  }
  checks$no_adjusted_collider <- list(
    pass    = length(colliderWarnings) == 0L,
    message = if (length(colliderWarnings) == 0L) "OK" else sprintf(
      "Potentially adjusted collider(s): %s. Conditioning on a collider can open a backdoor path.",
      paste(colliderWarnings, collapse = ", ")
    )
  )

  overall <- all(vapply(checks, `[[`, logical(1), "pass"))
  list(checks = checks, overall = overall)
}
