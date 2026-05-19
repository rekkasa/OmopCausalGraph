#' Switch the active phenotype binding for a node
#'
#' @description
#' Changes which named binding alternative is active for a node. The active
#' binding is the one used by `executeOmopCausalGraphPhenotypes()` and serialization.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#' @param node Node name (character scalar).
#' @param alias Alias key to activate.
#'
#' @return `omopCausalGraph`, invisibly.
#'
#' @examples
#' omopCausalGraph <- emptyOmopCausalGraph("Example")
#' omopCausalGraph <- addNode(omopCausalGraph, "Exposure", 1L, "Drug")
#' json1 <- '{"ConceptSets":[],"PrimaryCriteria":{"CriteriaList":[]}}'
#' json2 <- '{"ConceptSets":[],"PrimaryCriteria":{"CriteriaList":[]}}'
#' omopCausalGraph <- bindPhenotype(omopCausalGraph, "Exposure", type = "atlasJson", definition = json1)
#' omopCausalGraph <- bindPhenotype(omopCausalGraph, "Exposure", type = "atlasJson", definition = json2, alias = "v2")
#' omopCausalGraph <- setActiveBinding(omopCausalGraph, "Exposure", alias = "v2")
#'
#' @family bindings
#' @export
setActiveBinding <- function(omopCausalGraph, node, alias) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }
  if (!node %in% omopCausalGraph$constructs()$name) {
    stop(sprintf("Node '%s' does not exist in the DAG.", node), call. = FALSE)
  }
  bindings <- omopCausalGraph$bindings()
  if (is.null(bindings[[node]])) {
    stop(sprintf("Node '%s' has no bindings.", node), call. = FALSE)
  }
  if (!alias %in% names(bindings[[node]]$alternatives)) {
    stop(sprintf(
      "Alias '%s' not found for node '%s'. Available: %s.",
      alias, node, paste(names(bindings[[node]]$alternatives), collapse = ", ")
    ), call. = FALSE)
  }
  omopCausalGraph$setActiveBindingImpl(node, alias)
  invisible(omopCausalGraph)
}
