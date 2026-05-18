#' Switch the active phenotype binding for a node
#'
#' @description
#' Changes which named binding alternative is active for a node. The active
#' binding is the one used by `executeDagPhenotypes()` and serialization.
#'
#' @param dag An `OmopDag` object.
#' @param node Node name (character scalar).
#' @param alias Alias key to activate.
#'
#' @return `dag`, invisibly.
#'
#' @examples
#' dag <- emptyOmopDag("Example")
#' dag <- addNode(dag, "Exposure", 1L, "Drug")
#' json1 <- '{"ConceptSets":[],"PrimaryCriteria":{"CriteriaList":[]}}'
#' json2 <- '{"ConceptSets":[],"PrimaryCriteria":{"CriteriaList":[]}}'
#' dag <- bindPhenotype(dag, "Exposure", type = "atlasJson", definition = json1)
#' dag <- bindPhenotype(dag, "Exposure", type = "atlasJson", definition = json2, alias = "v2")
#' dag <- setActiveBinding(dag, "Exposure", alias = "v2")
#'
#' @family bindings
#' @export
setActiveBinding <- function(dag, node, alias) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }
  if (!node %in% dag$constructs()$name) {
    stop(sprintf("Node '%s' does not exist in the DAG.", node), call. = FALSE)
  }
  bindings <- dag$bindings()
  if (is.null(bindings[[node]])) {
    stop(sprintf("Node '%s' has no bindings.", node), call. = FALSE)
  }
  if (!alias %in% names(bindings[[node]]$alternatives)) {
    stop(sprintf(
      "Alias '%s' not found for node '%s'. Available: %s.",
      alias, node, paste(names(bindings[[node]]$alternatives), collapse = ", ")
    ), call. = FALSE)
  }
  dag$set_active_binding_impl(node, alias)
  invisible(dag)
}
