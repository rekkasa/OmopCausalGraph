#' Bulk-add directed edges from a CSV file
#'
#' @description
#' Reads a CSV and adds all rows as directed edges. Uses all-or-nothing
#' semantics: validates every row (including cycle detection) before committing.
#'
#' Required columns: `cause`, `effect`. Optional column: `evidence`.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#' @param path Path to the CSV file.
#'
#' @return `omopCausalGraph`, invisibly.
#'
#' @examples
#' \dontrun{
#' omopCausalGraph <- emptyOmopCausalGraph("Example")
#' omopCausalGraph <- addNodesFromCsv(omopCausalGraph, "nodes.csv")
#' omopCausalGraph <- addEdgesFromCsv(omopCausalGraph, "edges.csv")
#' }
#'
#' @family construction
#' @export
addEdgesFromCsv <- function(omopCausalGraph, path) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("File not found: '%s'.", path), call. = FALSE)
  }
  csvData      <- utils::read.csv(path, stringsAsFactors = FALSE)
  requiredCols <- c("cause", "effect")
  missingCols  <- setdiff(requiredCols, names(csvData))
  if (length(missingCols) > 0) {
    stop(sprintf(
      "CSV is missing required columns: %s.",
      paste(missingCols, collapse = ", ")
    ), call. = FALSE)
  }
  if (!"evidence" %in% names(csvData)) csvData$evidence <- NA_character_

  nodeNames    <- omopCausalGraph$constructs()$name
  errors       <- character(0)
  workingEdges <- omopCausalGraph$edges()

  for (i in seq_len(nrow(csvData))) {
    row <- csvData[i, ]
    if (!row$cause %in% nodeNames) {
      errors <- c(errors, sprintf("Row %d: cause node '%s' not found.", i, row$cause))
      next
    }
    if (!row$effect %in% nodeNames) {
      errors <- c(errors, sprintf("Row %d: effect node '%s' not found.", i, row$effect))
      next
    }
    if (nrow(workingEdges) > 0 &&
        any(workingEdges$cause == row$cause & workingEdges$effect == row$effect)) {
      errors <- c(errors, sprintf("Row %d: edge '%s -> %s' already exists.", i, row$cause, row$effect))
      next
    }
    cyclePath <- detectCycle(workingEdges, row$cause, row$effect)
    if (!is.null(cyclePath)) {
      errors <- c(errors, sprintf(
        "Row %d: edge '%s -> %s' creates a cycle: %s.",
        i, row$cause, row$effect, paste(cyclePath, collapse = " -> ")
      ))
      next
    }
    workingEdges <- rbind(
      workingEdges,
      data.frame(cause = row$cause, effect = row$effect,
                 evidence = as.character(row$evidence),
                 stringsAsFactors = FALSE)
    )
  }

  if (length(errors) > 0) {
    stop(paste(c("Errors in CSV:", errors), collapse = "\n  "), call. = FALSE)
  }

  for (i in seq_len(nrow(csvData))) {
    row      <- csvData[i, ]
    evidence <- if (is.na(row$evidence) || nchar(row$evidence) == 0) NA_character_ else row$evidence
    omopCausalGraph$addEdgeImpl(row$cause, row$effect, evidence)
  }
  invisible(omopCausalGraph)
}
