#' Create an OmopCausalGraph from a dagitty graph
#'
#' @description
#' Converts a `dagitty` graph (or a raw dagitty DSL string) into an
#' `OmopCausalGraph`, preserving node names, directed edges, and any causal
#' roles already encoded in the dagitty object (exposure, outcome,
#' latent → unobserved, adjusted, selected).
#'
#' When a character string is supplied, two sanitizers run automatically before
#' parsing:
#' \itemize{
#'   \item Position metadata (`bb=`, `[pos="..."]`) is stripped — it is not
#'     used by `OmopCausalGraph` and some versions of the dagitty R package do
#'     not accept the combined `[role,pos="..."]` syntax produced by
#'     dagitty.io.
#'   \item The string `--` inside quoted node names is replaced with a single
#'     `-`, because `--` is a reserved undirected-edge operator in the dagitty
#'     DSL and confuses the parser even inside quoted strings.
#' }
#'
#' OMOP-specific fields (`conceptId`, `domain`) are set to `NA` for every node.
#' Non-directed edges (`<->`, `--`) are silently dropped.
#'
#' @param dagittyGraph A `dagitty` graph object **or** a length-1 character
#'   string containing a dagitty DSL definition (e.g. copy-pasted from
#'   dagitty.io). If a string is provided the sanitizers are applied before
#'   parsing.
#' @param name Study name (character scalar).
#' @param version Semver string, default `"0.1.0"`.
#' @param description Optional study description.
#' @param author Optional author name.
#' @param orcid Optional ORCID iD.
#'
#' @return An `OmopCausalGraph` object.
#'
#' @examples
#' g <- dagitty::dagitty("dag { X [exposure]; Y [outcome]; Z -> X; Z -> Y; X -> Y }")
#' omopCausalGraph <- fromDagitty(g, name = "My Study")
#' omopCausalGraph
#'
#' # Raw string pasted from dagitty.io also works:
#' dagStr <- 'dag { bb="0,0,1,1"
#'   X [exposure,pos="0.2,0.5"]
#'   Y [outcome,pos="0.8,0.5"]
#'   Z [pos="0.5,0.1"]
#'   Z -> X; Z -> Y; X -> Y }'
#' omopCausalGraph <- fromDagitty(dagStr, name = "My Study")
#'
#' @family construction
#' @export
fromDagitty <- function(dagittyGraph,
                        name,
                        version     = "0.1.0",
                        description = NA_character_,
                        author      = NA_character_,
                        orcid       = NA_character_) {
  if (is.character(dagittyGraph) && length(dagittyGraph) == 1L) {
    dagittyGraph <- .stripDagittyPositions(dagittyGraph)
    dagittyGraph <- .sanitizeDagittyEdgeTokens(dagittyGraph)
    dagittyGraph <- dagitty::dagitty(dagittyGraph)
  }
  if (!inherits(dagittyGraph, "dagitty")) {
    stop(
      "'dagittyGraph' must be a dagitty object or a dagitty DSL character string.",
      call. = FALSE
    )
  }
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("'name' must be a single non-empty character string.", call. = FALSE)
  }

  omopCausalGraph <- OmopCausalGraph$new(
    name        = name,
    version     = version,
    description = description,
    author      = author,
    orcid       = orcid
  )

  for (nodeName in names(dagittyGraph)) {
    omopCausalGraph$addNodeImpl(nodeName, NA_integer_, NA_character_)
  }

  edgesData     <- dagitty::edges(dagittyGraph)
  directedEdges <- edgesData[edgesData$e == "->", , drop = FALSE]
  for (i in seq_len(nrow(directedEdges))) {
    omopCausalGraph$addEdgeImpl(directedEdges$v[i], directedEdges$w[i], NA_character_)
  }

  roleMap <- list(
    exposure   = dagitty::exposures(dagittyGraph),
    outcome    = dagitty::outcomes(dagittyGraph),
    unobserved = dagitty::latents(dagittyGraph),
    adjusted   = dagitty::adjustedNodes(dagittyGraph),
    selected   = tryCatch(dagitty::selectedNodes(dagittyGraph), error = function(e) character(0))
  )

  for (role in names(roleMap)) {
    for (nodeName in roleMap[[role]]) {
      omopCausalGraph$setRoleImpl(nodeName, role)
    }
  }

  omopCausalGraph
}

# Strip bb= and pos= attributes from a dagitty DSL string.
# The combined [role,pos="..."] syntax from dagitty.io is not accepted by all
# versions of the dagitty R package; standalone [pos="..."] is irrelevant to
# OmopCausalGraph regardless.
#' @noRd
.stripDagittyPositions <- function(s) {
  s <- gsub('bb="[^"]*"[ \t]*\n?', "", s)
  s <- gsub(
    '\\[(exposure|outcome|latent|adjusted|selected),pos="[^"]*"\\]',
    "[\\1]", s
  )
  s <- gsub('\\[pos="[^"]*"\\]', "", s)
  s
}

# Replace reserved dagitty tokens inside quoted node names.
# The dagitty lexer tokenises these before the quoted-string rule runs, so they
# break the parser even inside "...":
#   --   undirected-edge operator
#   /    treated as a special lexer token in some parser versions
#' @noRd
.sanitizeDagittyEdgeTokens <- function(s) {
  m <- gregexpr('"[^"]*"', s)
  quoted <- regmatches(s, m)[[1]]
  if (length(quoted) > 0L) {
    fixed <- gsub("--", "-", quoted, fixed = TRUE)
    fixed <- gsub("/",  "-", fixed,  fixed = TRUE)
    regmatches(s, m) <- list(fixed)
  }
  s
}
