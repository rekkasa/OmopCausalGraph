#' Import an OmopDag from a JSON-LD lockfile
#'
#' @description
#' Reconstructs an `OmopDag` from a file created by `exportOmopDag()`.
#' Hard-errors on major version mismatch between the lockfile and the running
#' package; warns on minor/patch mismatch and proceeds.
#'
#' @param path Path to the JSON-LD lockfile.
#'
#' @return A rehydrated `OmopDag` object.
#'
#' @examples
#' \dontrun{
#' dag <- importOmopDag("study.json")
#' }
#'
#' @family serialization
#' @export
importOmopDag <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: '%s'.", path), call. = FALSE)
  }

  doc <- jsonlite::read_json(path, simplifyVector = FALSE)

  # Version check
  file_version <- doc$version
  pkg_version  <- as.character(utils::packageVersion("OmopCausalGraph"))

  .version_major <- function(v) as.integer(strsplit(as.character(v), ".", fixed = TRUE)[[1]][1])
  .version_minor <- function(v) as.integer(strsplit(as.character(v), ".", fixed = TRUE)[[1]][2])

  if (!is.null(file_version)) {
    fmaj <- .version_major(file_version)
    pmaj <- .version_major(pkg_version)
    if (fmaj != pmaj) {
      stop(sprintf(
        "Major version mismatch: lockfile is v%s, running package is v%s.\nManual migration not yet automated; deferred post-1.0.",
        file_version, pkg_version
      ), call. = FALSE)
    }
    if (file_version != pkg_version) {
      warning(sprintf(
        "Version mismatch: lockfile is v%s, running package is v%s. Proceeding.",
        file_version, pkg_version
      ), call. = FALSE)
    }
  }

  meta <- doc$metadata
  dag  <- OmopDag$new(
    name        = meta$name,
    version     = meta$version %||% "0.1.0",
    description = meta$description %||% NA_character_,
    author      = meta$author %||% NA_character_,
    orcid       = meta$orcid %||% NA_character_
  )

  # Restore constructs
  for (cn in doc$constructs) {
    dag$add_node_impl(
      name      = cn$name,
      conceptId = as.integer(cn$conceptId),
      domain    = cn$domain %||% NA_character_
    )
  }

  # Restore edges
  for (e in doc$edges) {
    dag$add_edge_impl(
      cause    = e$cause,
      effect   = e$effect,
      evidence = e$evidence %||% NA_character_
    )
  }

  # Restore roles
  for (r in doc$roles) {
    for (role in r$roles) {
      dag$set_role_impl(r$node, role)
    }
  }

  # Restore bindings
  for (b in doc$bindings) {
    node <- b$node
    for (alt in b$alternatives) {
      bl <- list(type = alt$type)
      if (!is.null(alt$json))        bl$json        <- alt$json
      if (!is.null(alt$sha256))      bl$sha256      <- alt$sha256
      if (!is.null(alt$phenotypeId)) bl$phenotypeId <- as.integer(alt$phenotypeId)
      if (!is.null(alt$commitHash))  bl$commitHash  <- alt$commitHash
      if (!is.null(alt$repo))        bl$repo        <- alt$repo
      if (!is.null(alt$resolved))    bl$resolved    <- isTRUE(alt$resolved)
      if (!is.null(alt$column))      bl$column      <- alt$column
      dag$bind_phenotype_impl(node, alt$alias, bl)
    }
    active_alias <- Filter(function(a) isTRUE(a$active), b$alternatives)
    if (length(active_alias) > 0) {
      dag$set_active_binding_impl(node, active_alias[[1]]$alias)
    }
  }

  # Restore adjustment sets
  for (a in doc$adjustmentSets) {
    key <- paste0(a$exposure, "__", a$outcome)
    dag$set_adjustment_set_impl(key, as.integer(a$index), as.character(unlist(a$nodes)))
  }

  dag
}

# Null-coalescing helper (not exported)
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
