#' Check for selection bias in an OmopCausalGraph
#'
#' @description
#' Identifies nodes with the `selected` role and, for each exposure-outcome pair,
#' enumerates backdoor paths opened by conditioning on those nodes.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#'
#' @return A `data.frame` with columns `selected_node`, `exposure`, `outcome`,
#'   `opened_backdoor_path`. Returns an empty data frame if no `selected` nodes
#'   exist.
#'
#' @examples
#' omopCausalGraph <- emptyOmopCausalGraph("Example")
#' omopCausalGraph <- addNode(omopCausalGraph, "Exposure", 1L, "Drug")
#' omopCausalGraph <- addNode(omopCausalGraph, "Outcome",  2L, "Condition")
#' omopCausalGraph <- addNode(omopCausalGraph, "Selected", 3L, "Observation")
#' omopCausalGraph <- addCausal(omopCausalGraph, "Exposure", "Selected")
#' omopCausalGraph <- addCausal(omopCausalGraph, "Outcome",  "Selected")
#' omopCausalGraph <- setExposure(omopCausalGraph, "Exposure")
#' omopCausalGraph <- setOutcome(omopCausalGraph,  "Outcome")
#' omopCausalGraph <- setSelected(omopCausalGraph, "Selected")
#' checkSelectionBias(omopCausalGraph)
#'
#' @family validation
#' @export
checkSelectionBias <- function(omopCausalGraph) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }

  emptyResult <- data.frame(
    selected_node        = character(0),
    exposure             = character(0),
    outcome              = character(0),
    opened_backdoor_path = character(0),
    stringsAsFactors     = FALSE
  )

  roles         <- omopCausalGraph$roles()
  selectedNodes <- roles$node[roles$role == "selected"]
  if (length(selectedNodes) == 0L) return(emptyResult)

  exposures <- roles$node[roles$role == "exposure"]
  outcomes  <- roles$node[roles$role == "outcome"]
  if (length(exposures) == 0L || length(outcomes) == 0L) return(emptyResult)

  dagittyGraph <- omopCausalGraph$dagittyGraph()
  rows     <- list()

  for (selectedNode in selectedNodes) {
    conditionedGraph <- tryCatch(
      dagitty::adjustmentSets(dagittyGraph, exposure = exposures[1], outcome = outcomes[1],
                              adjust.for = selectedNode),
      error = function(omopCausalGraphError) NULL
    )

    for (exposure in exposures) {
      for (outcome in outcomes) {
        allPaths <- tryCatch(
          dagitty::paths(dagittyGraph, from = exposure, to = outcome, directed = FALSE),
          error = function(omopCausalGraphError) list(paths = character(0))
        )
        pathStrings <- if (length(allPaths$paths) > 0)
          as.character(allPaths$paths)
        else
          character(0)

        for (pathString in pathStrings) {
          if (grepl(selectedNode, pathString, fixed = TRUE)) {
            rows[[length(rows) + 1L]] <- data.frame(
              selected_node        = selectedNode,
              exposure             = exposure,
              outcome              = outcome,
              opened_backdoor_path = pathString,
              stringsAsFactors     = FALSE
            )
          }
        }
      }
    }
  }

  if (length(rows) == 0L) emptyResult else do.call(rbind, rows)
}
