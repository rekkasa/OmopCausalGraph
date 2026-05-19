#' Create an empty OmopCausalGraph
#'
#' @description
#' Thin constructor wrapper around `OmopCausalGraph$new()`. Returns an empty DAG
#' ready for incremental construction via `addNode()`, `addCausal()`, etc.
#'
#' @param name Study name (character scalar).
#' @param version Semver string, default `"0.1.0"`.
#' @param description Optional study description.
#' @param author Optional author name.
#' @param orcid Optional ORCID iD.
#'
#' @return An `OmopCausalGraph` R6 object.
#'
#' @examples
#' omopCausalGraph <- emptyOmopCausalGraph("My Study", author = "Smith J")
#' omopCausalGraph
#'
#' @family construction
#' @export
emptyOmopCausalGraph <- function(name,
                                 version     = "0.1.0",
                                 description = NA_character_,
                                 author      = NA_character_,
                                 orcid       = NA_character_) {
  OmopCausalGraph$new(
    name        = name,
    version     = version,
    description = description,
    author      = author,
    orcid       = orcid
  )
}
