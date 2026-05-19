#' Bulk-add construct nodes from a CSV file
#'
#' @description
#' Reads a CSV file and adds all rows as nodes to the DAG. Uses all-or-nothing
#' semantics: validates every row first, then adds all nodes if all pass.
#'
#' Required columns: `name`, `conceptId`. Optional column: `domain`.
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
#' }
#'
#' @family construction
#' @export
addNodesFromCsv <- function(omopCausalGraph, path) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("File not found: '%s'.", path), call. = FALSE)
  }
  csvData      <- utils::read.csv(path, stringsAsFactors = FALSE)
  requiredCols <- c("name", "conceptId")
  missingCols  <- setdiff(requiredCols, names(csvData))
  if (length(missingCols) > 0) {
    stop(sprintf(
      "CSV is missing required columns: %s.",
      paste(missingCols, collapse = ", ")
    ), call. = FALSE)
  }
  if (!"domain" %in% names(csvData)) csvData$domain <- NA_character_

  errors <- character(0)
  for (i in seq_len(nrow(csvData))) {
    row       <- csvData[i, ]
    conceptId <- suppressWarnings(as.integer(row$conceptId))
    if (is.na(conceptId) || conceptId <= 0L) {
      errors <- c(errors, sprintf("Row %d: invalid conceptId '%s'.", i, row$conceptId))
    }
    if (!is.na(row$domain) && nchar(row$domain) > 0 && !row$domain %in% omop_domains) {
      errors <- c(errors, sprintf("Row %d: invalid domain '%s'.", i, row$domain))
    }
    if (row$name %in% omopCausalGraph$constructs()$name) {
      errors <- c(errors, sprintf("Row %d: node '%s' already exists.", i, row$name))
    }
  }
  if (length(errors) > 0) {
    stop(paste(c("Errors in CSV:", errors), collapse = "\n  "), call. = FALSE)
  }

  for (i in seq_len(nrow(csvData))) {
    row    <- csvData[i, ]
    domain <- if (is.na(row$domain) || nchar(row$domain) == 0) NA_character_ else row$domain
    omopCausalGraph$addNodeImpl(row$name, as.integer(row$conceptId), domain)
  }
  invisible(omopCausalGraph)
}
