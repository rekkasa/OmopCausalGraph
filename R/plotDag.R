#' Plot an OmopDag
#'
#' @description
#' Converts the DAG to a `ggdag` tidy data frame and returns a `ggplot2` object
#' coloured by causal role. Nodes with an active (non-demographics) binding are
#' drawn as filled circles (shape 21); nodes without a binding are drawn as
#' hollow circles (shape 1).
#'
#' Default colour scheme (by role): exposure `#1f77b4`, outcome `#d62728`,
#' unobserved `#7f7f7f`, adjusted `#2ca02c`, selected `#ff7f0e`, plain `#cccccc`.
#' Pass a named character vector to `node_colors` to override per-role defaults.
#'
#' @param dag An `OmopDag` object.
#' @param node_colors Named character vector of hex colours keyed by role name
#'   (`"exposure"`, `"outcome"`, `"unobserved"`, `"adjusted"`, `"selected"`,
#'   `"plain"`). Overrides defaults for the specified roles.
#' @param node_size Numeric. Point size, default `10`.
#' @param label_size Numeric. Label text size (ggplot units), default `3.5`.
#' @param edge_color Character. Edge colour, default `"#444444"`.
#' @param ... Additional arguments passed to `ggdag::ggdag()`.
#'
#' @return A `ggplot2` object. Never prints; supports `+` extensibility.
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
#' p <- plotDag(dag)
#'
#' @family analytics
#' @export
plotDag <- function(dag,
                    node_colors = NULL,
                    node_size   = 10,
                    label_size  = 3.5,
                    edge_color  = "#444444",
                    ...) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }

  default_colors <- c(
    exposure   = "#1f77b4",
    outcome    = "#d62728",
    unobserved = "#7f7f7f",
    adjusted   = "#2ca02c",
    selected   = "#ff7f0e",
    plain      = "#cccccc"
  )
  if (!is.null(node_colors)) {
    default_colors[names(node_colors)] <- node_colors
  }

  roles    <- dag$roles()
  bindings <- dag$bindings()

  # Primary role per node (first role wins for colour)
  role_priority <- c("exposure", "outcome", "unobserved", "adjusted", "selected")
  node_role_map <- stats::setNames(rep("plain", nrow(dag$constructs())),
                                   dag$constructs()$name)
  for (pri in rev(role_priority)) {
    nodes_with_role <- roles$node[roles$role == pri]
    node_role_map[nodes_with_role] <- pri
  }

  # Binding status per node
  has_binding <- function(n) {
    b <- bindings[[n]]
    if (is.null(b)) return(FALSE)
    active_def <- b$alternatives[[b$active]]
    !is.null(active_def) && !identical(active_def$type, "demographics")
  }

  tidy_dag <- ggdag::tidy_dagitty(dag$dagitty_graph())
  tidy_dag$data$role        <- node_role_map[tidy_dag$data$name]
  tidy_dag$data$fill_color  <- default_colors[tidy_dag$data$role]
  tidy_dag$data$node_shape  <- ifelse(
    vapply(tidy_dag$data$name, has_binding, logical(1)), 21L, 1L
  )

  ggplot2::ggplot(tidy_dag, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggdag::geom_dag_edges(edge_colour = edge_color) +
    ggplot2::geom_point(
      ggplot2::aes(fill = role, shape = I(node_shape)),
      size   = node_size,
      colour = "black"
    ) +
    ggplot2::scale_fill_manual(values = default_colors, na.value = default_colors["plain"]) +
    ggdag::geom_dag_label(size = label_size) +
    ggdag::theme_dag() +
    ggplot2::labs(fill = "Causal role")
}
