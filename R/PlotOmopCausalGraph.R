#' Plot an OmopCausalGraph
#'
#' @description
#' Converts the DAG to a `ggdag` tidy data frame and returns a `ggplot2` object
#' coloured by causal role. Nodes with an active (non-demographics) binding are
#' drawn as filled circles (shape 21); nodes without a binding are drawn as
#' hollow circles (shape 1).
#'
#' Default colour scheme (by role): exposure `#1f77b4`, outcome `#d62728`,
#' unobserved `#7f7f7f`, adjusted `#2ca02c`, selected `#ff7f0e`, plain `#cccccc`.
#' Pass a named character vector to `nodeColors` to override per-role defaults.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#' @param nodeColors Named character vector of hex colours keyed by role name
#'   (`"exposure"`, `"outcome"`, `"unobserved"`, `"adjusted"`, `"selected"`,
#'   `"plain"`). Overrides defaults for the specified roles.
#' @param nodeSize Numeric. Point size, default `10`.
#' @param labelSize Numeric. Label text size (ggplot units), default `3.5`.
#' @param edgeColor Character. Edge colour, default `"#444444"`.
#' @param ... Additional arguments passed to `ggdag::ggdag()`.
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
                    nodeSize   = 10,
                    labelSize  = 3.5,
                    edgeColor  = "#444444",
                    ...) {
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

  tidyOmopCausalGraph <- ggdag::tidy_dagitty(omopCausalGraph$dagittyGraph())

  exposureNode <- roles$node[roles$role == "exposure"]
  outcomeNode  <- roles$node[roles$role == "outcome"]

  if (length(exposureNode) == 1L && length(outcomeNode) == 1L) {
    dat <- tidyOmopCausalGraph$data
    nodePos <- unique(dat[!is.na(dat$name), c("name", "x", "y")])

    ex <- nodePos$x[nodePos$name == exposureNode]
    ey <- nodePos$y[nodePos$name == exposureNode]
    ox <- nodePos$x[nodePos$name == outcomeNode]
    oy <- nodePos$y[nodePos$name == outcomeNode]

    if (length(ex) == 1L && length(ox) == 1L) {
      angle <- -atan2(oy - ey, ox - ex)
      cosA  <- cos(angle)
      sinA  <- sin(angle)

      rotX <- function(x, y) cosA * (x - ex) - sinA * (y - ey) + ex
      rotY <- function(x, y) sinA * (x - ex) + cosA * (y - ey) + ey

      tidyOmopCausalGraph$data$x    <- rotX(dat$x,    dat$y)
      tidyOmopCausalGraph$data$y    <- rotY(dat$x,    dat$y)
      tidyOmopCausalGraph$data$xend <- rotX(dat$xend, dat$yend)
      tidyOmopCausalGraph$data$yend <- rotY(dat$xend, dat$yend)
    }
  }

  tidyOmopCausalGraph$data$role       <- nodeRoleMap[tidyOmopCausalGraph$data$name]
  tidyOmopCausalGraph$data$fill_color <- defaultColors[tidyOmopCausalGraph$data$role]
  tidyOmopCausalGraph$data$node_shape <- ifelse(
    vapply(tidyOmopCausalGraph$data$name, hasBinding, logical(1)), 21L, 1L
  )

  ggplot2::ggplot(tidyOmopCausalGraph, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggdag::geom_dag_edges(edge_colour = edgeColor) +
    ggplot2::geom_point(
      ggplot2::aes(fill = role, shape = I(node_shape)),
      size   = nodeSize,
      colour = "black"
    ) +
    ggplot2::scale_fill_manual(values = defaultColors, na.value = defaultColors["plain"]) +
    ggdag::geom_dag_label(size = labelSize) +
    ggdag::theme_dag() +
    ggplot2::labs(fill = "Causal role")
}
