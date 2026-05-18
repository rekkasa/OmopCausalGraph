#' Add a construct node to an OmopDag
#'
#' @description
#' Adds a single construct node anchored to an OMOP Standard Concept ID.
#' The `domain` is validated against the bundled `omop_domains` internal vector;
#' pass `NA` to leave it unspecified.
#'
#' @param dag An `OmopDag` object.
#' @param name Node name (character scalar, must be unique within the DAG).
#' @param conceptId OMOP Standard Concept ID (positive integer or coercible).
#' @param domain OMOP domain string or `NA`. Must be a member of the internal
#'   `omop_domains` vector if supplied.
#'
#' @return `dag`, invisibly.
#'
#' @examples
#' dag <- emptyOmopDag("Example")
#' dag <- addNode(dag, "Hypertension", conceptId = 320128L, domain = "Condition")
#'
#' @family construction
#' @export
addNode <- function(dag, name, conceptId, domain = NA_character_) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || nchar(trimws(name)) == 0L) {
    stop("'name' must be a single non-empty character string.", call. = FALSE)
  }
  conceptId <- suppressWarnings(as.integer(conceptId))
  if (length(conceptId) != 1L || is.na(conceptId) || conceptId <= 0L) {
    stop("'conceptId' must be a single positive integer.", call. = FALSE)
  }
  if (!is.na(domain)) {
    if (!domain %in% omop_domains) {
      stop(sprintf(
        "'domain' value '%s' is not a valid OMOP domain.\nAllowed values: %s",
        domain, paste(sort(omop_domains), collapse = ", ")
      ), call. = FALSE)
    }
  }
  existing <- dag$constructs()$name
  if (name %in% existing) {
    stop(sprintf("Node '%s' already exists in the DAG.", name), call. = FALSE)
  }

  dag$add_node_impl(name, conceptId, domain)
  invisible(dag)
}
