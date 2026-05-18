#' Bulk-add construct nodes from a CSV file
#'
#' @description
#' Reads a CSV file and adds all rows as nodes to the DAG. Uses all-or-nothing
#' semantics: validates every row first, then adds all nodes if all pass.
#'
#' Required columns: `name`, `conceptId`. Optional column: `domain`.
#'
#' @param dag An `OmopDag` object.
#' @param path Path to the CSV file.
#'
#' @return `dag`, invisibly.
#'
#' @examples
#' \dontrun{
#' dag <- emptyOmopDag("Example")
#' dag <- addNodesFromCsv(dag, "nodes.csv")
#' }
#'
#' @family construction
#' @export
addNodesFromCsv <- function(dag, path) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("File not found: '%s'.", path), call. = FALSE)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  required_cols <- c("name", "conceptId")
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "CSV is missing required columns: %s.",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }
  if (!"domain" %in% names(df)) df$domain <- NA_character_

  errors <- character(0)
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    cid <- suppressWarnings(as.integer(row$conceptId))
    if (is.na(cid) || cid <= 0L) {
      errors <- c(errors, sprintf("Row %d: invalid conceptId '%s'.", i, row$conceptId))
    }
    if (!is.na(row$domain) && nchar(row$domain) > 0 && !row$domain %in% omop_domains) {
      errors <- c(errors, sprintf("Row %d: invalid domain '%s'.", i, row$domain))
    }
    if (row$name %in% dag$constructs()$name) {
      errors <- c(errors, sprintf("Row %d: node '%s' already exists.", i, row$name))
    }
  }
  if (length(errors) > 0) {
    stop(paste(c("Errors in CSV:", errors), collapse = "\n  "), call. = FALSE)
  }

  for (i in seq_len(nrow(df))) {
    row    <- df[i, ]
    domain <- if (is.na(row$domain) || nchar(row$domain) == 0) NA_character_ else row$domain
    dag$add_node_impl(row$name, as.integer(row$conceptId), domain)
  }
  invisible(dag)
}
