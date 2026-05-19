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
#' @param omopCausalGraph An `OmopCausalGraph` object.
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
#' @return `omopCausalGraph`, invisibly.
#'
#' @examples
#' omopCausalGraph <- emptyOmopCausalGraph("Example")
#' omopCausalGraph <- addNode(omopCausalGraph, "Exposure", 1L, "Drug")
#' atlas_json <- '{"ConceptSets":[],"PrimaryCriteria":{"CriteriaList":[]}}'
#' omopCausalGraph <- bindPhenotype(omopCausalGraph, "Exposure", type = "atlasJson", definition = atlas_json)
#'
#' @family bindings
#' @export
bindPhenotype <- function(omopCausalGraph, node, type, definition, alias = NULL, bundle = FALSE) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }
  validTypes <- c("atlasJson", "capr", "PhenotypeLibrary", "demographics")
  if (!type %in% validTypes) {
    stop(sprintf(
      "'type' must be one of: %s.", paste(validTypes, collapse = ", ")
    ), call. = FALSE)
  }
  if (!node %in% omopCausalGraph$constructs()$name) {
    stop(sprintf("Node '%s' does not exist in the OMOPCAUSALGRAPH.", node), call. = FALSE)
  }

  aliasKey <- if (is.null(alias)) "default" else alias

  bindingData <- switch(type,

    atlasJson = {
      if (!is.character(definition) || length(definition) != 1L) {
        stop("For type 'atlasJson', 'definition' must be a single JSON string.", call. = FALSE)
      }
      tryCatch(jsonlite::fromJSON(definition), error = function(jsonError) {
        stop(sprintf("'definition' is not valid JSON: %s", conditionMessage(jsonError)), call. = FALSE)
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
      compiledJson <- jsonlite::toJSON(Capr::toCirce(definition), auto_unbox = TRUE)
      list(
        type   = "capr",
        json   = as.character(compiledJson),
        sha256 = as.character(openssl::sha256(as.character(compiledJson)))
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
      phenotypeId <- as.integer(definition$phenotypeId)
      if (is.na(phenotypeId) || phenotypeId <= 0L) {
        stop("'phenotypeId' must be a positive integer.", call. = FALSE)
      }
      commitHash <- as.character(definition$commitHash)
      if (nchar(commitHash) != 40L) {
        stop("'commitHash' must be a 40-character SHA string.", call. = FALSE)
      }
      bindingList <- list(
        type        = "PhenotypeLibrary",
        phenotypeId = phenotypeId,
        commitHash  = commitHash,
        repo        = "OHDSI/PhenotypeLibrary",
        resolved    = FALSE,
        json        = NULL,
        sha256      = NULL
      )
      if (isTRUE(bundle)) bindingList <- .resolveOneBinding(bindingList)
      bindingList
    },

    demographics = {
      validColumns <- c(
        "gender_concept_id", "year_of_birth", "race_concept_id",
        "ethnicity_concept_id", "location_id", "care_site_id",
        "person_source_value", "gender_source_value",
        "race_source_value", "ethnicity_source_value"
      )
      if (!definition %in% validColumns) {
        stop(sprintf(
          "'definition' for type 'demographics' must be one of: %s.",
          paste(validColumns, collapse = ", ")
        ), call. = FALSE)
      }
      list(type = "demographics", column = definition)
    }
  )

  omopCausalGraph$bindPhenotypeImpl(node, aliasKey, bindingData)
  invisible(omopCausalGraph)
}

# Internal helper used by bindPhenotype(bundle=TRUE) and resolvePhenotypes()
#' @noRd
.resolveOneBinding <- function(bindingData) {
  url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/inst/Cohorts/%d.json",
    bindingData$repo, bindingData$commitHash, bindingData$phenotypeId
  )
  responseLines <- tryCatch(
    readLines(url, warn = FALSE),
    error = function(fetchError) stop(sprintf(
      "Failed to fetch PhenotypeLibrary definition (phenotypeId=%d, commitHash=%s): %s",
      bindingData$phenotypeId, bindingData$commitHash, conditionMessage(fetchError)
    ), call. = FALSE)
  )
  jsonStr             <- paste(responseLines, collapse = "\n")
  bindingData$json     <- jsonStr
  bindingData$sha256   <- as.character(openssl::sha256(jsonStr))
  bindingData$resolved <- TRUE
  bindingData
}
