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
#' @param omopCausalGraph An `OmopCausalGraph` object.
#'
#' @return `omopCausalGraph`, invisibly.
#'
#' @examples
#' \dontrun{
#' omopCausalGraph <- resolvePhenotypes(omopCausalGraph)
#' }
#'
#' @family bindings
#' @export
resolvePhenotypes <- function(omopCausalGraph) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }

  bindings <- omopCausalGraph$bindings()
  errors   <- character(0)

  for (nodeName in names(bindings)) {
    nodeBindings <- bindings[[nodeName]]$alternatives
    for (alias in names(nodeBindings)) {
      bindingData <- nodeBindings[[alias]]
      if (!identical(bindingData$type, "PhenotypeLibrary") || isTRUE(bindingData$resolved)) next

      resolvedBinding <- tryCatch(
        .resolveOneBinding(bindingData),
        error = function(fetchError) {
          errors <<- c(errors, sprintf(
            "Node '%s' alias '%s': %s", nodeName, alias, conditionMessage(fetchError)
          ))
          NULL
        }
      )

      if (!is.null(resolvedBinding)) {
        omopCausalGraph$bindPhenotypeImpl(nodeName, alias, resolvedBinding)
      }
    }
  }

  if (length(errors) > 0) {
    stop(paste(c("Failed to resolve phenotypes:", errors), collapse = "\n  "), call. = FALSE)
  }

  invisible(omopCausalGraph)
}
