#' Record the chosen minimal adjustment set for an exposure-outcome pair
#'
#' @description
#' Recomputes the available minimal adjustment sets for the given pair and
#' records the analyst's choice by index. Must be called for every
#' exposure-outcome pair before `executeDagPhenotypes()`.
#'
#' @param dag An `OmopDag` object.
#' @param exposure Name of the exposure node.
#' @param outcome Name of the outcome node.
#' @param index Integer index into the list returned by
#'   `getMinimalAdjustmentSet()` for this pair. Use `0L` to record an empty
#'   set (valid when exposure and outcome are d-separated).
#'
#' @return `dag`, invisibly.
#'
#' @examples
#' dag <- emptyOmopDag("Example")
#' dag <- addNode(dag, "Confounder", 1L, "Condition")
#' dag <- addNode(dag, "Exposure",   2L, "Drug")
#' dag <- addNode(dag, "Outcome",    3L, "Condition")
#' dag <- addCausal(dag, "Confounder", c("Exposure", "Outcome"))
#' dag <- addCausal(dag, "Exposure",  "Outcome")
#' dag <- setExposure(dag, "Exposure")
#' dag <- setOutcome(dag,  "Outcome")
#' dag <- setAdjustmentSet(dag, "Exposure", "Outcome", index = 1L)
#'
#' @family analytics
#' @export
setAdjustmentSet <- function(dag, exposure, outcome, index) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }

  roles     <- dag$roles()
  exposures <- roles$node[roles$role == "exposure"]
  outcomes  <- roles$node[roles$role == "outcome"]

  if (!exposure %in% exposures) {
    stop(sprintf("'%s' is not an exposure node. Use setExposure() first.", exposure), call. = FALSE)
  }
  if (!outcome %in% outcomes) {
    stop(sprintf("'%s' is not an outcome node. Use setOutcome() first.", outcome), call. = FALSE)
  }

  index <- as.integer(index)

  sets <- tryCatch(
    as.list(dagitty::adjustmentSets(dag$dagitty_graph(),
                                     exposure = exposure,
                                     outcome  = outcome,
                                     type     = "minimal")),
    error = function(e) list()
  )

  if (index == 0L) {
    chosen_nodes <- character(0)
  } else {
    if (length(sets) == 0L) {
      stop(sprintf(
        "No valid adjustment sets exist for '%s -> %s'. The query is unidentifiable.",
        exposure, outcome
      ), call. = FALSE)
    }
    if (index < 1L || index > length(sets)) {
      stop(sprintf(
        "index %d is out of range; %d set(s) available for '%s -> %s'.",
        index, length(sets), exposure, outcome
      ), call. = FALSE)
    }
    chosen_nodes <- as.character(sets[[index]])
  }

  key <- paste0(exposure, "__", outcome)
  dag$set_adjustment_set_impl(key, index, chosen_nodes)
  invisible(dag)
}
