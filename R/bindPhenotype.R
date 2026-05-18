#' Bind a phenotype definition to a DAG node
#'
#' @description
#' Attaches an executable phenotype definition to a node. Supported types:
#'
#' - `"atlasJson"`: ATLAS cohort JSON string; always embedded with SHA-256.
#' - `"capr"`: A `Capr` cohort R object; compiled to JSON at bind time
#'   (requires the `Capr` package).
#' - `"PhenotypeLibrary"`: stores `phenotypeId` + `commitHash` + repo by
#'   default. Pass `bundle = TRUE` to immediately fetch and embed.
#' - `"demographics"`: metadata-only; stores an OMOP `PERSON` table column.
#'   No cohort is generated for this binding type.
#'
#' If `alias` is `NULL`, the binding is stored as `"default"` and marked
#' active. If `alias` is supplied, it is registered as a named alternative;
#' the active binding is unchanged unless no binding existed before.
#'
#' @param dag An `OmopDag` object.
#' @param node Node name (character scalar).
#' @param type Binding type; one of `"atlasJson"`, `"capr"`,
#'   `"PhenotypeLibrary"`, `"demographics"`.
#' @param definition Type-specific definition:
#'   - `"atlasJson"`: character JSON string.
#'   - `"capr"`: a `Cohort` object from the `Capr` package.
#'   - `"PhenotypeLibrary"`: a list with `phenotypeId` (integer) and
#'     `commitHash` (40-character SHA string).
#'   - `"demographics"`: character column name from the OMOP `PERSON` table.
#' @param alias Optional alias name. If `NULL`, stored as `"default"`.
#' @param bundle Logical. For `"PhenotypeLibrary"` only: if `TRUE`, immediately
#'   fetch and embed the definition from GitHub.
#'
#' @return `dag`, invisibly.
#'
#' @examples
#' dag <- emptyOmopDag("Example")
#' dag <- addNode(dag, "Exposure", 1L, "Drug")
#' atlas_json <- '{"ConceptSets":[],"PrimaryCriteria":{"CriteriaList":[]}}'
#' dag <- bindPhenotype(dag, "Exposure", type = "atlasJson", definition = atlas_json)
#'
#' @family bindings
#' @export
bindPhenotype <- function(dag, node, type, definition, alias = NULL, bundle = FALSE) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }
  valid_types <- c("atlasJson", "capr", "PhenotypeLibrary", "demographics")
  if (!type %in% valid_types) {
    stop(sprintf(
      "'type' must be one of: %s.", paste(valid_types, collapse = ", ")
    ), call. = FALSE)
  }
  if (!node %in% dag$constructs()$name) {
    stop(sprintf("Node '%s' does not exist in the DAG.", node), call. = FALSE)
  }

  alias_key <- if (is.null(alias)) "default" else alias

  binding_list <- switch(type,

    atlasJson = {
      if (!is.character(definition) || length(definition) != 1L) {
        stop("For type 'atlasJson', 'definition' must be a single JSON string.", call. = FALSE)
      }
      tryCatch(jsonlite::fromJSON(definition), error = function(e) {
        stop(sprintf("'definition' is not valid JSON: %s", conditionMessage(e)), call. = FALSE)
      })
      list(
        type   = "atlasJson",
        json   = definition,
        sha256 = as.character(openssl::sha256(chartr("", "", definition)))
      )
    },

    capr = {
      if (!requireNamespace("Capr", quietly = TRUE)) {
        stop("Package 'Capr' is required for type 'capr'. Install with: remotes::install_github('OHDSI/Capr')", call. = FALSE)
      }
      if (!inherits(definition, "Cohort")) {
        stop("For type 'capr', 'definition' must be a Capr 'Cohort' object.", call. = FALSE)
      }
      compiled_json <- jsonlite::toJSON(Capr::toCirce(definition), auto_unbox = TRUE)
      list(
        type   = "capr",
        json   = as.character(compiled_json),
        sha256 = as.character(openssl::sha256(as.character(compiled_json)))
      )
    },

    PhenotypeLibrary = {
      if (!is.list(definition) ||
          !all(c("phenotypeId", "commitHash") %in% names(definition))) {
        stop(
          "For type 'PhenotypeLibrary', 'definition' must be a list with 'phenotypeId' and 'commitHash'.",
          call. = FALSE
        )
      }
      ph_id <- as.integer(definition$phenotypeId)
      if (is.na(ph_id) || ph_id <= 0L) {
        stop("'phenotypeId' must be a positive integer.", call. = FALSE)
      }
      commit <- as.character(definition$commitHash)
      if (nchar(commit) != 40L) {
        stop("'commitHash' must be a 40-character SHA string.", call. = FALSE)
      }
      bl <- list(
        type        = "PhenotypeLibrary",
        phenotypeId = ph_id,
        commitHash  = commit,
        repo        = "OHDSI/PhenotypeLibrary",
        resolved    = FALSE,
        json        = NULL,
        sha256      = NULL
      )
      if (isTRUE(bundle)) bl <- .resolve_one_binding(bl)
      bl
    },

    demographics = {
      valid_cols <- c(
        "gender_concept_id", "year_of_birth", "race_concept_id",
        "ethnicity_concept_id", "location_id", "care_site_id",
        "person_source_value", "gender_source_value",
        "race_source_value", "ethnicity_source_value"
      )
      if (!definition %in% valid_cols) {
        stop(sprintf(
          "'definition' for type 'demographics' must be one of: %s.",
          paste(valid_cols, collapse = ", ")
        ), call. = FALSE)
      }
      list(type = "demographics", column = definition)
    }
  )

  dag$bind_phenotype_impl(node, alias_key, binding_list)
  invisible(dag)
}

# Internal helper used by bindPhenotype(bundle=TRUE) and resolvePhenotypes()
#' @noRd
.resolve_one_binding <- function(bl) {
  url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/inst/Cohorts/%d.json",
    bl$repo, bl$commitHash, bl$phenotypeId
  )
  resp <- tryCatch(
    readLines(url, warn = FALSE),
    error = function(e) stop(sprintf(
      "Failed to fetch PhenotypeLibrary definition (phenotypeId=%d, commitHash=%s): %s",
      bl$phenotypeId, bl$commitHash, conditionMessage(e)
    ), call. = FALSE)
  )
  json_str <- paste(resp, collapse = "\n")
  bl$json     <- json_str
  bl$sha256   <- as.character(openssl::sha256(json_str))
  bl$resolved <- TRUE
  bl
}
