#' Export an OmopDag to a JSON-LD file
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
#' @param dag An `OmopDag` object.
#' @param path Output file path.
#'
#' @return `path`, invisibly.
#'
#' @examples
#' \dontrun{
#' dag <- emptyOmopDag("Study")
#' exportOmopDag(dag, "study.json")
#' }
#'
#' @family serialization
#' @export
exportOmopDag <- function(dag, path) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }

  meta      <- dag$metadata()
  constructs <- dag$constructs()
  edges     <- dag$edges()
  roles     <- dag$roles()
  bindings  <- dag$bindings()
  adj_sets  <- dag$adjustment_sets()

  constructs_ld <- lapply(seq_len(nrow(constructs)), function(i) {
    list(
      `@id`     = sprintf("https://athena.ohdsi.org/search-terms/terms/%d", constructs$conceptId[i]),
      name      = constructs$name[i],
      conceptId = constructs$conceptId[i],
      domain    = constructs$domain[i]
    )
  })

  edges_ld <- lapply(seq_len(nrow(edges)), function(i) {
    list(cause = edges$cause[i], effect = edges$effect[i],
         evidence = edges$evidence[i])
  })

  roles_ld <- lapply(unique(roles$node), function(n) {
    list(node = n, roles = roles$role[roles$node == n])
  })

  bindings_ld <- lapply(names(bindings), function(n) {
    b     <- bindings[[n]]
    alts  <- lapply(names(b$alternatives), function(a) {
      def <- b$alternatives[[a]]
      rec <- list(alias = a, type = def$type, active = identical(a, b$active))
      if (def$type %in% c("atlasJson", "capr") ||
          (def$type == "PhenotypeLibrary" && isTRUE(def$resolved))) {
        rec$json   <- def$json
        rec$sha256 <- def$sha256
      }
      if (def$type == "PhenotypeLibrary") {
        rec$phenotypeId <- def$phenotypeId
        rec$commitHash  <- def$commitHash
        rec$repo        <- def$repo
        rec$resolved    <- def$resolved
      }
      if (def$type == "demographics") {
        rec$column <- def$column
      }
      rec
    })
    list(node = n, alternatives = alts)
  })

  adj_sets_ld <- lapply(names(adj_sets), function(key) {
    pair <- strsplit(key, "__", fixed = TRUE)[[1]]
    list(
      exposure = pair[1],
      outcome  = pair[2],
      index    = adj_sets[[key]]$index,
      nodes    = adj_sets[[key]]$nodes
    )
  })

  pkg_version <- as.character(utils::packageVersion("OmopCausalGraph"))

  doc <- list(
    `@context` = "https://omopcausalgraph.org/context/v1",
    `@type`    = "OmopDag",
    version    = pkg_version,
    metadata   = meta,
    constructs = constructs_ld,
    edges      = edges_ld,
    roles      = roles_ld,
    bindings   = bindings_ld,
    adjustmentSets = adj_sets_ld
  )

  jsonlite::write_json(doc, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}
