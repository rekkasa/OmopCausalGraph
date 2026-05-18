#' Get minimal adjustment sets for all exposure-outcome pairs
#'
#' @description
#' Queries `dagitty::adjustmentSets()` for every exposure-outcome pair defined
#' in the DAG and returns all valid minimal sets. Does not mutate `dag` and does
#' not auto-select; use `setAdjustmentSet()` to record the analyst's choice.
#'
#' @param dag An `OmopDag` object.
#'
#' @return A named list of class `omop_adjustment_sets`, keyed by
#'   `"<exposure>__<outcome>"`. Each element is a list of valid sets (character
#'   vectors). Empty list means no valid set exists (unidentifiable).
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
#' sets <- getMinimalAdjustmentSet(dag)
#' print(sets)
#'
#' @family analytics
#' @export
getMinimalAdjustmentSet <- function(dag) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }
  roles     <- dag$roles()
  exposures <- roles$node[roles$role == "exposure"]
  outcomes  <- roles$node[roles$role == "outcome"]
  g         <- dag$dagitty_graph()

  result <- list()
  for (exp in exposures) {
    for (out in outcomes) {
      key  <- paste0(exp, "__", out)
      sets <- tryCatch(
        as.list(dagitty::adjustmentSets(g, exposure = exp, outcome = out,
                                         type = "minimal")),
        error = function(e) list()
      )
      result[[key]] <- lapply(sets, as.character)
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
    pair  <- strsplit(key, "__", fixed = TRUE)[[1]]
    sets  <- x[[key]]
    cat(sprintf("  %s -> %s: ", pair[1], pair[2]))
    if (length(sets) == 0L) {
      cat("NO VALID ADJUSTMENT SET (unidentifiable)\n")
    } else {
      set_strs <- vapply(sets, function(s) {
        if (length(s) == 0L) "{}" else paste0("{", paste(s, collapse = ", "), "}")
      }, character(1))
      cat(paste(set_strs, collapse = "  |  "), "\n")
    }
  }
  invisible(x)
}
