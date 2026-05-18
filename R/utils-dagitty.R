#' @noRd
rebuild_dagitty <- function(nodes_df, edges_df, roles_df) {
  parts <- character(0)

  # Node declarations with roles
  for (i in seq_len(nrow(nodes_df))) {
    n <- nodes_df$name[i]
    node_roles <- roles_df$role[roles_df$node == n]
    attrs <- character(0)

    if ("exposure" %in% node_roles) attrs <- c(attrs, "exposure")
    if ("outcome" %in% node_roles) attrs <- c(attrs, "outcome")
    if ("unobserved" %in% node_roles) attrs <- c(attrs, "latent")
    if ("adjusted" %in% node_roles) attrs <- c(attrs, "adjusted")
    if ("selected" %in% node_roles) attrs <- c(attrs, "selected")

    if (length(attrs) > 0) {
      parts <- c(parts, sprintf("%s [%s]", n, paste(attrs, collapse = ", ")))
    } else {
      parts <- c(parts, n)
    }
  }

  # Edge declarations
  if (nrow(edges_df) > 0) {
    for (i in seq_len(nrow(edges_df))) {
      parts <- c(parts, sprintf("%s -> %s", edges_df$cause[i], edges_df$effect[i]))
    }
  }

  dsl <- sprintf("dag { %s }", paste(parts, collapse = "; "))
  dagitty::dagitty(dsl)
}
