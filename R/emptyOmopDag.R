#' Create an empty OmopDag
#'
#' @description
#' Thin constructor wrapper around `OmopDag$new()`. Returns an empty DAG ready
#' for incremental construction via `addNode()`, `addCausal()`, etc.
#'
#' @param name Study name (character scalar).
#' @param version Semver string, default `"0.1.0"`.
#' @param description Optional study description.
#' @param author Optional author name.
#' @param orcid Optional ORCID iD.
#'
#' @return An `OmopDag` R6 object.
#'
#' @examples
#' dag <- emptyOmopDag("My Study", author = "Smith J")
#' dag
#'
#' @family construction
#' @export
emptyOmopDag <- function(name,
                         version     = "0.1.0",
                         description = NA_character_,
                         author      = NA_character_,
                         orcid       = NA_character_) {
  OmopDag$new(
    name        = name,
    version     = version,
    description = description,
    author      = author,
    orcid       = orcid
  )
}
