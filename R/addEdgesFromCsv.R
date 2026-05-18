#' Bulk-add directed edges from a CSV file
#'
#' @description
#' Reads a CSV and adds all rows as directed edges. Uses all-or-nothing
#' semantics: validates every row (including cycle detection) before committing.
#'
#' Required columns: `cause`, `effect`. Optional column: `evidence`.
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
#' dag <- addEdgesFromCsv(dag, "edges.csv")
#' }
#'
#' @family construction
#' @export
addEdgesFromCsv <- function(dag, path) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("File not found: '%s'.", path), call. = FALSE)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  required_cols <- c("cause", "effect")
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "CSV is missing required columns: %s.",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }
  if (!"evidence" %in% names(df)) df$evidence <- NA_character_

  node_names <- dag$constructs()$name
  errors     <- character(0)

  # Validate using a working copy (simulates addCausal but accumulates errors)
  working_edges <- dag$edges()
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    if (!row$cause %in% node_names) {
      errors <- c(errors, sprintf("Row %d: cause node '%s' not found.", i, row$cause))
      next
    }
    if (!row$effect %in% node_names) {
      errors <- c(errors, sprintf("Row %d: effect node '%s' not found.", i, row$effect))
      next
    }
    if (nrow(working_edges) > 0 &&
        any(working_edges$cause == row$cause & working_edges$effect == row$effect)) {
      errors <- c(errors, sprintf("Row %d: edge '%s -> %s' already exists.", i, row$cause, row$effect))
      next
    }
    cycle_path <- detect_cycle(working_edges, row$cause, row$effect)
    if (!is.null(cycle_path)) {
      errors <- c(errors, sprintf(
        "Row %d: edge '%s -> %s' creates a cycle: %s.",
        i, row$cause, row$effect, paste(cycle_path, collapse = " -> ")
      ))
      next
    }
    # Tentatively add to working copy for subsequent cycle checks
    working_edges <- rbind(
      working_edges,
      data.frame(cause = row$cause, effect = row$effect,
                 evidence = as.character(row$evidence),
                 stringsAsFactors = FALSE)
    )
  }

  if (length(errors) > 0) {
    stop(paste(c("Errors in CSV:", errors), collapse = "\n  "), call. = FALSE)
  }

  for (i in seq_len(nrow(df))) {
    row      <- df[i, ]
    evidence <- if (is.na(row$evidence) || nchar(row$evidence) == 0) NA_character_ else row$evidence
    dag$add_edge_impl(row$cause, row$effect, evidence)
  }
  invisible(dag)
}
