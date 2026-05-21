buildSugiyamaLayout <- function(omopCausalGraph) {
  edges    <- omopCausalGraph$edges()[, c("cause", "effect")]
  vertices <- omopCausalGraph$constructs()[, "name", drop = FALSE]

  graph <- igraph::graph_from_data_frame(
    d        = edges,
    vertices = vertices,
    directed = TRUE
  )

  layout <- ggraph::create_layout(graph, layout = "sugiyama")

  # Rotate top-to-bottom Sugiyama to left-to-right: negate y becomes x,
  # original x becomes y. Sources (high y in default view) map to small x (left).
  origX    <- layout$x
  layout$x <- -layout$y
  layout$y <- origX

  layout
}

#' Plot an OmopCausalGraph
#'
#' @description
#' Renders an `OmopCausalGraph` as a `ggplot2` object coloured by causal role,
#' using a Sugiyama hierarchical layout (via `ggraph` and `igraph`) oriented
#' left-to-right so causal flow reads naturally from exposure to outcome.
#'
#' Graphs with more than 10 nodes automatically switch to a label-only
#' rendering mode: nodes are invisible (`nodeSize = 0`) and labels are drawn
#' with `ggrepel::geom_label_repel()` to prevent overlap. In this mode
#' `labelSize` scales inversely with node count (floor `2.0`), and nodes with
#' an active non-demographics binding are shown in **bold** label text. Role
#' colour is applied as label background fill, consistent with the small-graph
#' style. Explicit user-supplied values for `nodeSize` and `labelSize` override
#' the automatic large-graph defaults.
#'
#' Default colour scheme (by role): exposure `#1f77b4`, outcome `#d62728`,
#' unobserved `#7f7f7f`, adjusted `#2ca02c`, selected `#ff7f0e`, plain
#' `#cccccc`. Pass a named character vector to `nodeColors` to override
#' per-role defaults.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#' @param nodeColors Named character vector of hex colours keyed by role name
#'   (`"exposure"`, `"outcome"`, `"unobserved"`, `"adjusted"`, `"selected"`,
#'   `"plain"`). Overrides defaults for the specified roles.
#' @param nodeSize Numeric. Point size. Defaults to `10` for small graphs
#'   (`<= 10` nodes) and `0` for large graphs (`> 10` nodes). Explicit values
#'   override the automatic default in both modes.
#' @param labelSize Numeric. Label text size (ggplot units). Defaults to `3.5`
#'   for small graphs and `max(2.0, 3.5 - (n - 10) / 10)` for large graphs.
#'   Explicit values override the automatic default in both modes.
#' @param edgeColor Character. Edge colour, default `"#444444"`.
#'
#' @return A `ggplot2` object. Never prints; supports `+` extensibility.
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
#' p <- plotOmopCausalGraph(omopCausalGraph)
#'
#' @family analytics
#' @export
plotOmopCausalGraph <- function(omopCausalGraph,
                    nodeColors = NULL,
                    nodeSize   = NULL,
                    labelSize  = NULL,
                    edgeColor  = "#444444") {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }

  defaultColors <- c(
    exposure   = "#1f77b4",
    outcome    = "#d62728",
    unobserved = "#7f7f7f",
    adjusted   = "#2ca02c",
    selected   = "#ff7f0e",
    plain      = "#cccccc"
  )
  if (!is.null(nodeColors)) {
    defaultColors[names(nodeColors)] <- nodeColors
  }

  roles    <- omopCausalGraph$roles()
  bindings <- omopCausalGraph$bindings()

  rolePriority <- c("exposure", "outcome", "unobserved", "adjusted", "selected")
  nodeRoleMap  <- stats::setNames(rep("plain", nrow(omopCausalGraph$constructs())),
                                  omopCausalGraph$constructs()$name)
  for (priority in rev(rolePriority)) {
    nodesWithRole <- roles$node[roles$role == priority]
    nodeRoleMap[nodesWithRole] <- priority
  }

  hasBinding <- function(nodeName) {
    nodeBinding <- bindings[[nodeName]]
    if (is.null(nodeBinding)) return(FALSE)
    activeDef <- nodeBinding$alternatives[[nodeBinding$active]]
    !is.null(activeDef) && !identical(activeDef$type, "demographics")
  }

  layoutData <- buildSugiyamaLayout(omopCausalGraph)

  layoutData$role       <- nodeRoleMap[layoutData$name]
  layoutData$fillColor  <- defaultColors[layoutData$role]
  layoutData$bound      <- vapply(layoutData$name, hasBinding, logical(1))
  layoutData$nodeStroke <- ifelse(layoutData$bound, 1.5, 0.5)

  nConstructs  <- nrow(omopCausalGraph$constructs())
  isLargeGraph <- nConstructs > 10L

  if (is.null(nodeSize)) {
    nodeSize <- if (isLargeGraph) 0 else 10
  }
  if (is.null(labelSize)) {
    labelSize <- if (isLargeGraph) max(2.0, 3.5 - (nConstructs - 10) / 10) else 3.5
  }

  arrowSpec <- grid::arrow(length = grid::unit(0.2, "cm"), type = "closed")

  if (isLargeGraph) {
    ggraph::ggraph(layoutData) +
      ggraph::geom_edge_link(colour = edgeColor, arrow = arrowSpec,
                             end_cap = ggraph::circle(3, "mm")) +
      ggrepel::geom_label_repel(
        ggplot2::aes(x = x, y = y, label = name, fill = role,
                     fontface = ifelse(bound, "bold", "plain")),
        colour            = "black",
        size              = labelSize,
        max.overlaps      = Inf,
        min.segment.length = 0,
        data              = as.data.frame(layoutData)
      ) +
      ggplot2::scale_fill_manual(values = defaultColors, na.value = defaultColors["plain"]) +
      ggraph::theme_graph() +
      ggplot2::labs(fill = "Causal role")
  } else {
    ggraph::ggraph(layoutData) +
      ggraph::geom_edge_link(colour = edgeColor, arrow = arrowSpec,
                             end_cap = ggraph::circle(nodeSize / 2 + 1, "mm")) +
      ggraph::geom_node_point(
        ggplot2::aes(fill = role, stroke = I(nodeStroke)),
        shape  = 21L,
        size   = nodeSize,
        colour = "black"
      ) +
      ggplot2::scale_fill_manual(values = defaultColors, na.value = defaultColors["plain"]) +
      ggraph::geom_node_label(
        ggplot2::aes(label = name, fill = role),
        colour = "black",
        size   = labelSize
      ) +
      ggraph::theme_graph() +
      ggplot2::labs(fill = "Causal role")
  }
}
