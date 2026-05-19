#' Record the chosen minimal adjustment set for an exposure-outcome pair
#'
#' @description
#' Recomputes the available minimal adjustment sets for the given pair and
#' records the analyst's choice by index. Must be called for every
#' exposure-outcome pair before `executeOmopCausalGraphPhenotypes()`.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#' @param exposure Name of the exposure node.
#' @param outcome Name of the outcome node.
#' @param index Integer index into the list returned by
#'   `getMinimalAdjustmentSet()` for this pair. Use `0L` to record an empty
#'   set (valid when exposure and outcome are d-separated).
#'
#' @return `omopCausalGraph`, invisibly.
#'
#' @examples
#' omopCausalGraph <- emptyOmopCausalGraph("Example")
#' omopCausalGraph <- addNode(omopCausalGraph, "Confounder", 1L, "Condition")
#' omopCausalGraph <- addNode(omopCausalGraph, "Exposure",   2L, "Drug")
#' omopCausalGraph <- addNode(omopCausalGraph, "Outcome",    3L, "Condition")
#' omopCausalGraph <- addCausal(omopCausalGraph, "Confounder", c("Exposure", "Outcome"))
#' omopCausalGraph <- addCausal(omopCausalGraph, "Exposure",  "Outcome")
#' omopCausalGraph <- setExposure(omopCausalGraph, "Exposure")
#' omopCausalGraph <- setOutcome(omopCausalGraph,  "Outcome")
#' omopCausalGraph <- setAdjustmentSet(omopCausalGraph, "Exposure", "Outcome", index = 1L)
#'
#' @family analytics
#' @export
setAdjustmentSet <- function(omopCausalGraph, exposure, outcome, index) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }

  roles     <- omopCausalGraph$roles()
  exposures <- roles$node[roles$role == "exposure"]
  outcomes  <- roles$node[roles$role == "outcome"]

  if (!exposure %in% exposures) {
    stop(sprintf("'%s' is not an exposure node. Use setExposure() first.", exposure), call. = FALSE)
  }
  if (!outcome %in% outcomes) {
    stop(sprintf("'%s' is not an outcome node. Use setOutcome() first.", outcome), call. = FALSE)
  }

  index <- as.integer(index)

  availableSets <- tryCatch(
    as.list(dagitty::adjustmentSets(omopCausalGraph$dagittyGraph(),
                                    exposure = exposure,
                                    outcome  = outcome,
                                    type     = "minimal")),
    error = function(omopCausalGraphError) list()
  )

  if (index == 0L) {
    chosenNodes <- character(0)
  } else {
    if (length(availableSets) == 0L) {
      stop(sprintf(
        "No valid adjustment sets exist for '%s -> %s'. The query is unidentifiable.",
        exposure, outcome
      ), call. = FALSE)
    }
    if (index < 1L || index > length(availableSets)) {
      stop(sprintf(
        "index %d is out of range; %d set(s) available for '%s -> %s'.",
        index, length(availableSets), exposure, outcome
      ), call. = FALSE)
    }
    chosenNodes <- as.character(availableSets[[index]])
  }

  key <- paste0(exposure, "__", outcome)
  omopCausalGraph$setAdjustmentSetImpl(key, index, chosenNodes)
  invisible(omopCausalGraph)
}
