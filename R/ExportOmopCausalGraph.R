#' Export an OmopCausalGraph to a JSON-LD file
#'
#' @description
#' Serializes the DAG to a JSON-LD lockfile. OMOP concept IDs are expressed as
#' Athena IRIs (`https://athena.ohdsi.org/search-terms/terms/{conceptId}`).
#' The `@context` URL is documentary and bundled in
#' `inst/jsonld/context-v1.json`; it need not resolve at version 0.1.0.
#'
#' `PhenotypeLibrary` bindings in reference mode emit only `phenotypeId` +
#' `commitHash` + `repo`. Resolved `PhenotypeLibrary`, `atlasJson`, and `capr`
#' bindings embed `json` + `sha256`.
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#' @param path Output file path.
#'
#' @return `path`, invisibly.
#'
#' @examples
#' \dontrun{
#' exportOmopCausalGraph <- emptyOmopCausalGraph("Study")
#' exportOmopCausalGraph(omopCausalGraph, "study.json")
#' }
#'
#' @family serialization
#' @export
exportOmopCausalGraph <- function(omopCausalGraph, path) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }

  studyMeta  <- omopCausalGraph$metadata()
  constructs <- omopCausalGraph$constructs()
  edges      <- omopCausalGraph$edges()
  roles      <- omopCausalGraph$roles()
  bindings   <- omopCausalGraph$bindings()
  adjSets    <- omopCausalGraph$adjustmentSets()

  constructsLd <- lapply(seq_len(nrow(constructs)), function(i) {
    list(
      `@id`     = sprintf("https://athena.ohdsi.org/search-terms/terms/%d", constructs$conceptId[i]),
      name      = constructs$name[i],
      conceptId = constructs$conceptId[i],
      domain    = constructs$domain[i]
    )
  })

  edgesLd <- lapply(seq_len(nrow(edges)), function(i) {
    list(cause = edges$cause[i], effect = edges$effect[i],
         evidence = edges$evidence[i])
  })

  rolesLd <- lapply(unique(roles$node), function(nodeName) {
    list(node = nodeName, roles = roles$role[roles$node == nodeName])
  })

  bindingsLd <- lapply(names(bindings), function(nodeName) {
    nodeBinding <- bindings[[nodeName]]
    alts <- lapply(names(nodeBinding$alternatives), function(alias) {
      activeDef <- nodeBinding$alternatives[[alias]]
      rec <- list(
        alias  = alias,
        type   = activeDef$type,
        active = identical(alias, nodeBinding$active)
      )
      if (activeDef$type %in% c("atlasJson", "capr") ||
          (activeDef$type == "PhenotypeLibrary" && isTRUE(activeDef$resolved))) {
        rec$json   <- activeDef$json
        rec$sha256 <- activeDef$sha256
      }
      if (activeDef$type == "PhenotypeLibrary") {
        rec$phenotypeId <- activeDef$phenotypeId
        rec$commitHash  <- activeDef$commitHash
        rec$repo        <- activeDef$repo
        rec$resolved    <- activeDef$resolved
      }
      if (activeDef$type == "demographics") {
        rec$column <- activeDef$column
      }
      rec
    })
    list(node = nodeName, alternatives = alts)
  })

  adjSetsLd <- lapply(names(adjSets), function(key) {
    pair <- strsplit(key, "__", fixed = TRUE)[[1]]
    list(
      exposure = pair[1],
      outcome  = pair[2],
      index    = adjSets[[key]]$index,
      nodes    = adjSets[[key]]$nodes
    )
  })

  pkgVersion <- as.character(utils::packageVersion("OmopCausalGraph"))

  doc <- list(
    `@context` = "https://omopcausalgraph.org/context/v1",
    `@type`    = "OmopCausalGraph",
    version    = pkgVersion,
    metadata   = studyMeta,
    constructs = constructsLd,
    edges      = edgesLd,
    roles      = rolesLd,
    bindings   = bindingsLd,
    adjustmentSets = adjSetsLd
  )

  jsonlite::write_json(doc, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}
