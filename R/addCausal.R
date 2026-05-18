#' Add directed causal edge(s) to an OmopDag
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
#' @param dag An `OmopDag` object.
#' @param cause Name(s) of the cause node(s).
#' @param effect Name(s) of the effect node(s).
#' @param evidence Optional free-text evidence note(s). Recycled to match the
#'   number of edges if length-1 or `NA`.
#'
#' @return `dag`, invisibly.
#'
#' @examples
#' dag <- emptyOmopDag("Example")
#' dag <- addNode(dag, "Confounder", 1L, "Condition")
#' dag <- addNode(dag, "Exposure",   2L, "Drug")
#' dag <- addNode(dag, "Outcome",    3L, "Condition")
#' dag <- addCausal(dag, "Confounder", c("Exposure", "Outcome"))
#' dag <- addCausal(dag, "Exposure",  "Outcome")
#'
#' @family construction
#' @export
addCausal <- function(dag, cause, effect, evidence = NA_character_) {
  if (!inherits(dag, "OmopDag")) {
    stop("'dag' must be an OmopDag object.", call. = FALSE)
  }

  n_cause  <- length(cause)
  n_effect <- length(effect)

  if (n_cause == 1L && n_effect == 1L) {
    n <- 1L
  } else if (n_cause == n_effect) {
    n <- n_cause
  } else if (n_cause == 1L) {
    cause <- rep(cause, n_effect)
    n     <- n_effect
  } else if (n_effect == 1L) {
    effect <- rep(effect, n_cause)
    n      <- n_cause
  } else {
    stop(sprintf(
      "'cause' (length %d) and 'effect' (length %d) must be equal-length or one must be length-1.",
      n_cause, n_effect
    ), call. = FALSE)
  }

  if (length(evidence) == 1L) evidence <- rep(evidence, n)
  if (length(evidence) != n) {
    stop("'evidence' must be length-1 or the same length as the number of edges.", call. = FALSE)
  }

  node_names <- dag$constructs()$name

  for (i in seq_len(n)) {
    c_i <- cause[i]
    e_i <- effect[i]
    ev_i <- evidence[i]

    if (!c_i %in% node_names) {
      stop(sprintf("Cause node '%s' does not exist in the DAG.", c_i), call. = FALSE)
    }
    if (!e_i %in% node_names) {
      stop(sprintf("Effect node '%s' does not exist in the DAG.", e_i), call. = FALSE)
    }

    existing_edges <- dag$edges()
    if (nrow(existing_edges) > 0 &&
        any(existing_edges$cause == c_i & existing_edges$effect == e_i)) {
      stop(sprintf("Edge '%s -> %s' already exists.", c_i, e_i), call. = FALSE)
    }

    cycle_path <- detect_cycle(dag$edges(), c_i, e_i)
    if (!is.null(cycle_path)) {
      stop(sprintf(
        "Adding '%s -> %s' would create a cycle: %s.",
        c_i, e_i, paste(cycle_path, collapse = " -> ")
      ), call. = FALSE)
    }

    dag$add_edge_impl(c_i, e_i, ev_i)
  }

  invisible(dag)
}
