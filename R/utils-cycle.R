#' @noRd
detect_cycle <- function(edges_df, new_cause, new_effect) {
  all_edges <- rbind(
    edges_df[, c("cause", "effect"), drop = FALSE],
    data.frame(cause = new_cause, effect = new_effect, stringsAsFactors = FALSE)
  )

  # Build adjacency list
  adj <- split(all_edges$effect, all_edges$cause)

  # Iterative DFS from new_effect to detect path back to new_cause
  stack <- list(list(node = new_effect, path = new_effect))
  visited <- character(0)

  while (length(stack) > 0) {
    frame <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    node <- frame$node
    path <- frame$path

    if (node == new_cause) {
      return(c(new_cause, strsplit(path, " -> ", fixed = TRUE)[[1]]))
    }

    if (node %in% visited) next
    visited <- c(visited, node)

    neighbors <- adj[[node]]
    for (nb in neighbors) {
      stack <- c(stack, list(list(node = nb, path = paste0(path, " -> ", nb))))
    }
  }

  NULL
}
