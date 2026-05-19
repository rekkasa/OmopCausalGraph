#' @noRd
detectCycle <- function(edgesData, newCause, newEffect) {
  allEdges <- rbind(
    edgesData[, c("cause", "effect"), drop = FALSE],
    data.frame(cause = newCause, effect = newEffect, stringsAsFactors = FALSE)
  )

  adjacency <- split(allEdges$effect, allEdges$cause)

  stack   <- list(list(node = newEffect, path = newEffect))
  visited <- character(0)

  while (length(stack) > 0) {
    frame <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    currentNode <- frame$node
    currentPath <- frame$path

    if (currentNode == newCause) {
      return(c(newCause, strsplit(currentPath, " -> ", fixed = TRUE)[[1]]))
    }

    if (currentNode %in% visited) next
    visited <- c(visited, currentNode)

    neighbors <- adjacency[[currentNode]]
    for (neighbor in neighbors) {
      stack <- c(stack, list(list(node = neighbor, path = paste0(currentPath, " -> ", neighbor))))
    }
  }

  NULL
}

#' @noRd
rebuildDagitty <- function(nodesData, edgesData, rolesData) {
  omopCausalGraphParts <- character(0)

  for (i in seq_len(nrow(nodesData))) {
    nodeName  <- nodesData$name[i]
    nodeRoles <- rolesData$role[rolesData$node == nodeName]
    roleAttrs <- character(0)

    if ("exposure"   %in% nodeRoles) roleAttrs <- c(roleAttrs, "exposure")
    if ("outcome"    %in% nodeRoles) roleAttrs <- c(roleAttrs, "outcome")
    if ("unobserved" %in% nodeRoles) roleAttrs <- c(roleAttrs, "latent")
    if ("adjusted"   %in% nodeRoles) roleAttrs <- c(roleAttrs, "adjusted")
    if ("selected"   %in% nodeRoles) roleAttrs <- c(roleAttrs, "selected")

    if (length(roleAttrs) > 0) {
      omopCausalGraphParts <- c(omopCausalGraphParts, sprintf("%s [%s]", nodeName, paste(roleAttrs, collapse = ", ")))
    } else {
      omopCausalGraphParts <- c(omopCausalGraphParts, nodeName)
    }
  }

  if (nrow(edgesData) > 0) {
    for (i in seq_len(nrow(edgesData))) {
      omopCausalGraphParts <- c(omopCausalGraphParts, sprintf("%s -> %s", edgesData$cause[i], edgesData$effect[i]))
    }
  }

  omopCausalGraphDsl <- sprintf("dag { %s }", paste(omopCausalGraphParts, collapse = "; "))
  dagitty::dagitty(omopCausalGraphDsl)
}
