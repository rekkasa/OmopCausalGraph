#' Resolve PhenotypeLibrary reference bindings
#'
#' @description
#' Fetches all `PhenotypeLibrary` bindings that are still in reference mode
#' (i.e., `resolved == FALSE`) from GitHub at the recorded commit hash and
#' converts them to embedded JSON. Intended to run on an internet-connected
#' machine before entering a locked hospital environment.
#'
#' URL pattern:
#' `https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/<commitHash>/inst/Cohorts/<phenotypeId>.json`
#'
#' On partial failure, errors for all failing bindings are aggregated and
#' reported together after attempting all bindings.
#'
#' @param dag An `OmopDag` object.
#'
#' @return `dag`, invisibly.
#'
#' @examples
#' \dontrun{
#' dag <- resolvePhenotypes(dag)
#' }
#'
#' @family bindings
#' @export
resolvePhenotypes <- function(dag) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }

  bindings <- dag$bindings()
  errors   <- character(0)

  for (node in names(bindings)) {
    node_bindings <- bindings[[node]]$alternatives
    for (alias in names(node_bindings)) {
      bl <- node_bindings[[alias]]
      if (!identical(bl$type, "PhenotypeLibrary") || isTRUE(bl$resolved)) next

      resolved_bl <- tryCatch(
        .resolve_one_binding(bl),
        error = function(e) {
          errors <<- c(errors, sprintf(
            "Node '%s' alias '%s': %s", node, alias, conditionMessage(e)
          ))
          NULL
        }
      )

      if (!is.null(resolved_bl)) {
        dag$bind_phenotype_impl(node, alias, resolved_bl)
      }
    }
  }

  if (length(errors) > 0) {
    stop(paste(c("Failed to resolve phenotypes:", errors), collapse = "\n  "), call. = FALSE)
  }

  invisible(dag)
}
