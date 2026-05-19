#' Add directed causal edge(s) to an OmopCausalGraph
#'
#' @description
#' Adds one or more directed edges (`cause -> effect`) to the DAG. Before
#' inserting each edge, performs a depth-first cycle check (implemented in
#' package code, not delegated to `dagitty`).
#'
#' Vectorisation rules:
#' - Scalar `cause` + scalar `effect`: one edge.
#' - Equal-length vectors: pairwise edges.
#' - One side length-1, other side length-n: broadcast (one-to-many / many-to-one).
#' - Mismatched multi-element vectors: hard error (no Cartesian product).
#'
#' @param omopCausalGraph An `OmopCausalGraph` object.
#' @param cause Name(s) of the cause node(s).
#' @param effect Name(s) of the effect node(s).
#' @param evidence Optional free-text evidence note(s). Recycled to match the
#'   number of edges if length-1 or `NA`.
#'
#' @return `omopCausalGraph`, invisibly.
#'
#' @examples
#' omopCausalGraph <- emptyOmopCausalGraph("Example")
#' omopCausalGraph <- addNode(omopCausalGraph, "Confounder", 1L, "Condition")
#' omopCausalGraph <- addNode(omopCausalGraph, "Exposure",   2L, "Drug")
#' omopCausalGraph <- addNode(omopCausalGraph, "Outcome",    3L, "Condition")
#' omopCausalGraph <- addCausal(omopCausalGraph, "Confounder", c("Exposure", "Outcome"))
#' omopCausalGraph <- addCausal(omopCausalGraph, "Exposure",  "Outcome")
#'
#' @family construction
#' @export
addCausal <- function(omopCausalGraph, cause, effect, evidence = NA_character_) {
  if (!inherits(omopCausalGraph, "OmopCausalGraph")) {
    stop("'omopCausalGraph' must be an OmopCausalGraph object.", call. = FALSE)
  }

  nCause  <- length(cause)
  nEffect <- length(effect)

  if (nCause == 1L && nEffect == 1L) {
    nEdges <- 1L
  } else if (nCause == nEffect) {
    nEdges <- nCause
  } else if (nCause == 1L) {
    cause  <- rep(cause, nEffect)
    nEdges <- nEffect
  } else if (nEffect == 1L) {
    effect <- rep(effect, nCause)
    nEdges <- nCause
  } else {
    stop(sprintf(
      "'cause' (length %d) and 'effect' (length %d) must be equal-length or one must be length-1.",
      nCause, nEffect
    ), call. = FALSE)
  }

  if (length(evidence) == 1L) evidence <- rep(evidence, nEdges)
  if (length(evidence) != nEdges) {
    stop("'evidence' must be length-1 or the same length as the number of edges.", call. = FALSE)
  }

  nodeNames <- omopCausalGraph$constructs()$name

  for (i in seq_len(nEdges)) {
    causeNode    <- cause[i]
    effectNode   <- effect[i]
    evidenceNote <- evidence[i]

    if (!causeNode %in% nodeNames) {
      stop(sprintf("Cause node '%s' does not exist in the DAG.", causeNode), call. = FALSE)
    }
    if (!effectNode %in% nodeNames) {
      stop(sprintf("Effect node '%s' does not exist in the DAG.", effectNode), call. = FALSE)
    }

    existingEdges <- omopCausalGraph$edges()
    if (nrow(existingEdges) > 0 &&
        any(existingEdges$cause == causeNode & existingEdges$effect == effectNode)) {
      stop(sprintf("Edge '%s -> %s' already exists.", causeNode, effectNode), call. = FALSE)
    }

    cyclePath <- detectCycle(omopCausalGraph$edges(), causeNode, effectNode)
    if (!is.null(cyclePath)) {
      stop(sprintf(
        "Adding '%s -> %s' would create a cycle: %s.",
        causeNode, effectNode, paste(cyclePath, collapse = " -> ")
      ), call. = FALSE)
    }

    omopCausalGraph$addEdgeImpl(causeNode, effectNode, evidenceNote)
  }

  invisible(omopCausalGraph)
}
