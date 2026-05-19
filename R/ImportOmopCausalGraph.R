#' Import an OmopCausalGraph from a JSON-LD lockfile
#'
#' @description
#' Reconstructs an `OmopCausalGraph` from a file created by
#' `exportOmopCausalGraph()`. Hard-errors on major version mismatch between the
#' lockfile and the running package; warns on minor/patch mismatch and proceeds.
#'
#' @param path Path to the JSON-LD lockfile.
#'
#' @return A rehydrated `OmopCausalGraph` object.
#'
#' @examples
#' \dontrun{
#' omopCausalGraph <- importOmopCausalGraph("study.json")
#' }
#'
#' @family serialization
#' @export
importOmopCausalGraph <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: '%s'.", path), call. = FALSE)
  }

  doc <- jsonlite::read_json(path, simplifyVector = FALSE)

  fileVersion <- doc$version
  pkgVersion  <- as.character(utils::packageVersion("OmopCausalGraph"))

  .versionMajor <- function(v) as.integer(strsplit(as.character(v), ".", fixed = TRUE)[[1]][1])

  if (!is.null(fileVersion)) {
    fileMajor <- .versionMajor(fileVersion)
    pkgMajor  <- .versionMajor(pkgVersion)
    if (fileMajor != pkgMajor) {
      stop(sprintf(
        "Major version mismatch: lockfile is v%s, running package is v%s.\nManual migration not yet automated; deferred post-1.0.",
        fileVersion, pkgVersion
      ), call. = FALSE)
    }
    if (fileVersion != pkgVersion) {
      warning(sprintf(
        "Version mismatch: lockfile is v%s, running package is v%s. Proceeding.",
        fileVersion, pkgVersion
      ), call. = FALSE)
    }
  }

  studyMeta <- doc$metadata
  omopCausalGraph <- OmopCausalGraph$new(
    name        = studyMeta$name,
    version     = studyMeta$version %||% "0.1.0",
    description = studyMeta$description %||% NA_character_,
    author      = studyMeta$author %||% NA_character_,
    orcid       = studyMeta$orcid %||% NA_character_
  )

  for (constructNode in doc$constructs) {
    omopCausalGraph$addNodeImpl(
      name      = constructNode$name,
      conceptId = as.integer(constructNode$conceptId),
      domain    = constructNode$domain %||% NA_character_
    )
  }

  for (edge in doc$edges) {
    omopCausalGraph$addEdgeImpl(
      cause    = edge$cause,
      effect   = edge$effect,
      evidence = edge$evidence %||% NA_character_
    )
  }

  for (roleEntry in doc$roles) {
    for (role in roleEntry$roles) {
      omopCausalGraph$setRoleImpl(roleEntry$node, role)
    }
  }

  for (bindingEntry in doc$bindings) {
    nodeName <- bindingEntry$node
    for (alternative in bindingEntry$alternatives) {
      bindingData <- list(type = alternative$type)
      if (!is.null(alternative$json))        bindingData$json        <- alternative$json
      if (!is.null(alternative$sha256))      bindingData$sha256      <- alternative$sha256
      if (!is.null(alternative$phenotypeId)) bindingData$phenotypeId <- as.integer(alternative$phenotypeId)
      if (!is.null(alternative$commitHash))  bindingData$commitHash  <- alternative$commitHash
      if (!is.null(alternative$repo))        bindingData$repo        <- alternative$repo
      if (!is.null(alternative$resolved))    bindingData$resolved    <- isTRUE(alternative$resolved)
      if (!is.null(alternative$column))      bindingData$column      <- alternative$column
      omopCausalGraph$bindPhenotypeImpl(nodeName, alternative$alias, bindingData)
    }
    activeAlt <- Filter(function(alt) isTRUE(alt$active), bindingEntry$alternatives)
    if (length(activeAlt) > 0) {
      omopCausalGraph$setActiveBindingImpl(nodeName, activeAlt[[1]]$alias)
    }
  }

  for (adjustmentEntry in doc$adjustmentSets) {
    key <- paste0(adjustmentEntry$exposure, "__", adjustmentEntry$outcome)
    omopCausalGraph$setAdjustmentSetImpl(key, as.integer(adjustmentEntry$index),
                             as.character(unlist(adjustmentEntry$nodes)))
  }

  omopCausalGraph
}

# Null-coalescing helper (not exported)
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
