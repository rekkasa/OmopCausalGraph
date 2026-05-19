#' Set causal roles on DAG nodes
#'
#' @description
#' Five functions that assign `dagitty`-compatible causal roles to nodes.
#' Each accepts a character vector of node names. The role is stored internally
#' and reflected in the `dagitty` graph immediately.
#'
#' Disallowed combinations:
#' - `exposure` + `adjusted` on the same node.
#' - `outcome` + `adjusted` on the same node.
#' - `exposure` + `outcome` on the same node.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#' @param node Character vector of node name(s).
#'
#' @return `omopCausalGraph`, invisibly.
#'
#' @examples
#' omopCausalGraph <- emptyOmopCausalGraph("Example")
#' omopCausalGraph <- addNode(omopCausalGraph, "Exposure", 1L, "Drug")
#' omopCausalGraph <- addNode(omopCausalGraph, "Outcome",  2L, "Condition")
#' omopCausalGraph <- setExposure(omopCausalGraph, "Exposure")
#' omopCausalGraph <- setOutcome(omopCausalGraph,  "Outcome")
#'
#' @name setRoles
#' @family roles
NULL

.setRole <- function(omopCausalGraph, node, role) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }
  nodeNames    <- omopCausalGraph$constructs()$name
  missingNodes <- setdiff(node, nodeNames)
  if (length(missingNodes) > 0) {
    stop(sprintf(
      "Node(s) not found in DAG: %s.",
      paste(missingNodes, collapse = ", ")
    ), call. = FALSE)
  }
  for (nodeName in node) {
    omopCausalGraph$setRoleImpl(nodeName, role)
  }
  invisible(omopCausalGraph)
}

#' @rdname setRoles
#' @export
setExposure <- function(omopCausalGraph, node) .setRole(omopCausalGraph, node, "exposure")

#' @rdname setRoles
#' @export
setOutcome <- function(omopCausalGraph, node) .setRole(omopCausalGraph, node, "outcome")

#' @rdname setRoles
#' @export
setUnobserved <- function(omopCausalGraph, node) .setRole(omopCausalGraph, node, "unobserved")

#' @rdname setRoles
#' @export
setAdjusted <- function(omopCausalGraph, node) .setRole(omopCausalGraph, node, "adjusted")

#' @rdname setRoles
#' @export
setSelected <- function(omopCausalGraph, node) .setRole(omopCausalGraph, node, "selected")
