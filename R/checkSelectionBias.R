#' Check for selection bias in an OmopDag
#'
#' @description
#' Identifies nodes with the `selected` role and, for each exposure-outcome pair,
#' enumerates backdoor paths opened by conditioning on those nodes.
#'
#' @param dag An `OmopDag` object.
#'
#' @return A `data.frame` with columns `selected_node`, `exposure`, `outcome`,
#'   `opened_backdoor_path`. Returns an empty data frame if no `selected` nodes
#'   exist.
#'
#' @examples
#' dag <- emptyOmopDag("Example")
#' dag <- addNode(dag, "Exposure", 1L, "Drug")
#' dag <- addNode(dag, "Outcome",  2L, "Condition")
#' dag <- addNode(dag, "Selected", 3L, "Observation")
#' dag <- addCausal(dag, "Exposure", "Selected")
#' dag <- addCausal(dag, "Outcome",  "Selected")
#' dag <- setExposure(dag, "Exposure")
#' dag <- setOutcome(dag,  "Outcome")
#' dag <- setSelected(dag, "Selected")
#' checkSelectionBias(dag)
#'
#' @family validation
#' @export
checkSelectionBias <- function(dag) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }

  empty_result <- data.frame(
    selected_node        = character(0),
    exposure             = character(0),
    outcome              = character(0),
    opened_backdoor_path = character(0),
    stringsAsFactors     = FALSE
  )

  roles         <- dag$roles()
  selected_nodes <- roles$node[roles$role == "selected"]
  if (length(selected_nodes) == 0L) return(empty_result)

  exposures <- roles$node[roles$role == "exposure"]
  outcomes  <- roles$node[roles$role == "outcome"]
  if (length(exposures) == 0L || length(outcomes) == 0L) return(empty_result)

  g    <- dag$dagitty_graph()
  rows <- list()

  for (sel in selected_nodes) {
    # Build a modified graph conditioning on the selected node
    g_cond <- tryCatch(
      dagitty::adjustmentSets(g, exposure = exposures[1], outcome = outcomes[1],
                              adjust.for = sel),
      error = function(e) NULL
    )

    for (exp in exposures) {
      for (out in outcomes) {
        all_paths <- tryCatch(
          dagitty::paths(g, from = exp, to = out, directed = FALSE),
          error = function(e) list(paths = character(0))
        )
        path_strs <- if (length(all_paths$paths) > 0)
          as.character(all_paths$paths)
        else
          character(0)

        for (p in path_strs) {
          if (grepl(sel, p, fixed = TRUE)) {
            rows[[length(rows) + 1L]] <- data.frame(
              selected_node        = sel,
              exposure             = exp,
              outcome              = out,
              opened_backdoor_path = p,
              stringsAsFactors     = FALSE
            )
          }
        }
      }
    }
  }

  if (length(rows) == 0L) empty_result else do.call(rbind, rows)
}
