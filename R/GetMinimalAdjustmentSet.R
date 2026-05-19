#' Get minimal adjustment sets for all exposure-outcome pairs
#'
#' @description
#' Queries `dagitty::adjustmentSets()` for every exposure-outcome pair defined
#' in the DAG and returns all valid minimal sets. Does not mutate `omopCausalGraph` and does
#' not auto-select; use `setAdjustmentSet()` to record the analyst's choice.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#'
#' @return A named list of class `omop_adjustment_sets`, keyed by
#'   `"<exposure>__<outcome>"`. Each element is a list of valid sets (character
#'   vectors). Empty list means no valid set exists (unidentifiable).
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
#' sets <- getMinimalAdjustmentSet(omopCausalGraph)
#' print(sets)
#'
#' @family analytics
#' @export
getMinimalAdjustmentSet <- function(omopCausalGraph) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }
  roles     <- omopCausalGraph$roles()
  exposures <- roles$node[roles$role == "exposure"]
  outcomes  <- roles$node[roles$role == "outcome"]
  dagittyGraph  <- omopCausalGraph$dagittyGraph()

  result <- list()
  for (exposure in exposures) {
    for (outcome in outcomes) {
      key          <- paste0(exposure, "__", outcome)
      availableSets <- tryCatch(
        as.list(dagitty::adjustmentSets(dagittyGraph, exposure = exposure, outcome = outcome,
                                        type = "minimal")),
        error = function(omopCausalGraphError) list()
      )
      result[[key]] <- lapply(availableSets, as.character)
    }
  }

  structure(result, class = c("omop_adjustment_sets", "list"))
}

#' @export
print.omop_adjustment_sets <- function(x, ...) {
  if (length(x) == 0L) {
    cat("No exposure-outcome pairs found.\n")
    return(invisible(x))
  }
  for (key in names(x)) {
    pair         <- strsplit(key, "__", fixed = TRUE)[[1]]
    availableSets <- x[[key]]
    cat(sprintf("  %s -> %s: ", pair[1], pair[2]))
    if (length(availableSets) == 0L) {
      cat("NO VALID ADJUSTMENT SET (unidentifiable)\n")
    } else {
      setStrings <- vapply(availableSets, function(nodeSet) {
        if (length(nodeSet) == 0L) "{}" else paste0("{", paste(nodeSet, collapse = ", "), "}")
      }, character(1))
      cat(paste(setStrings, collapse = "  |  "), "\n")
    }
  }
  invisible(x)
}
