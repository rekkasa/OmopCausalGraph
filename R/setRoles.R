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
#' @param dag An `OmopDag` object.
#' @param node Character vector of node name(s).
#'
#' @return `dag`, invisibly.
#'
#' @examples
#' dag <- emptyOmopDag("Example")
#' dag <- addNode(dag, "Exposure", 1L, "Drug")
#' dag <- addNode(dag, "Outcome",  2L, "Condition")
#' dag <- setExposure(dag, "Exposure")
#' dag <- setOutcome(dag,  "Outcome")
#'
#' @name setRoles
#' @family roles
NULL

.set_role <- function(dag, node, role) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }
  node_names <- dag$constructs()$name
  bad <- setdiff(node, node_names)
  if (length(bad) > 0) {
    stop(sprintf(
      "Node(s) not found in DAG: %s.",
      paste(bad, collapse = ", ")
    ), call. = FALSE)
  }
  for (n in node) {
    dag$set_role_impl(n, role)
  }
  invisible(dag)
}

#' @rdname setRoles
#' @export
setExposure <- function(dag, node) .set_role(dag, node, "exposure")

#' @rdname setRoles
#' @export
setOutcome <- function(dag, node) .set_role(dag, node, "outcome")

#' @rdname setRoles
#' @export
setUnobserved <- function(dag, node) .set_role(dag, node, "unobserved")

#' @rdname setRoles
#' @export
setAdjusted <- function(dag, node) .set_role(dag, node, "adjusted")

#' @rdname setRoles
#' @export
setSelected <- function(dag, node) .set_role(dag, node, "selected")
