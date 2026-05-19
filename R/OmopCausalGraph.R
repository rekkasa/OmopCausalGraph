#' OmopCausalGraph R6 Class
#'
#' @title OmopCausalGraph: OMOP-anchored Causal DAG Container
#'
#' @description
#' Central study container holding a `dagitty` graph, a construct dictionary
#' anchored to OMOP Standard Concept IDs, a phenotype binding registry, and
#' study metadata. All mutating methods return `invisible(self)` to enable
#' pipe-friendly incremental construction.
#'
#' @field name Study name, mirrors `metadata$name`.
#'
#' @section Methods:
#' See exported constructor `emptyOmopCausalGraph()` and the construction, role,
#' validation, analytics, binding, execution, and serialization families.
#'
#' @export
OmopCausalGraph <- R6::R6Class(
  "OmopCausalGraph",

  private = list(
    dagittyGraph_    = NULL,
    constructs_      = NULL,
    edges_           = NULL,
    bindings_        = NULL,
    metadata_        = NULL,
    roles_           = NULL,
    adjustmentSets_  = NULL,

    # Role conflict rules: pairs that cannot coexist on the same node
    roleConflicts_ = list(
      c("exposure",  "adjusted"),
      c("outcome",   "adjusted"),
      c("exposure",  "outcome")
    )
  ),

  public = list(

    name = NULL,

    #' @description Create a new OmopCausalGraph.
    #' @param name Study name (character scalar).
    #' @param version Semver string, default `"0.1.0"`.
    #' @param description Optional description.
    #' @param author Optional author name.
    #' @param orcid Optional ORCID iD.
    initialize = function(name,
                          version     = "0.1.0",
                          description = NA_character_,
                          author      = NA_character_,
                          orcid       = NA_character_) {
      if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
        stop("'name' must be a single non-empty character string.", call. = FALSE)
      }
      self$name <- name
      private$metadata_ <- list(
        name        = name,
        version     = version,
        description = description,
        created     = Sys.time(),
        author      = author,
        orcid       = orcid
      )
      private$constructs_     <- data.frame(
        name      = character(0),
        conceptId = integer(0),
        domain    = character(0),
        stringsAsFactors = FALSE
      )
      private$edges_ <- data.frame(
        cause    = character(0),
        effect   = character(0),
        evidence = character(0),
        stringsAsFactors = FALSE
      )
      private$bindings_       <- list()
      private$roles_          <- data.frame(
        node = character(0),
        role = character(0),
        stringsAsFactors = FALSE
      )
      private$adjustmentSets_ <- list()
      private$dagittyGraph_   <- dagitty::dagitty("dag { }")

      invisible(self)
    },

    #' @description Print a compact summary of the DAG.
    print = function() {
      studyMeta  <- private$metadata_
      nNodes     <- nrow(private$constructs_)
      nEdges     <- nrow(private$edges_)
      exposures  <- private$roles_$node[private$roles_$role == "exposure"]
      outcomes   <- private$roles_$node[private$roles_$role == "outcome"]
      boundNodes <- names(private$bindings_)
      unresolved <- sum(vapply(private$bindings_, function(nodeBinding) {
        activeAlias <- nodeBinding$active
        activeDef   <- nodeBinding$alternatives[[activeAlias]]
        identical(activeDef$type, "PhenotypeLibrary") && isFALSE(activeDef$resolved)
      }, logical(1)))

      cat(sprintf(
        "OmopCausalGraph: %s (v%s)\n  Nodes: %d  Edges: %d\n  Exposures: %s\n  Outcomes:  %s\n  Bound nodes: %d  Unresolved PhenotypeLibrary: %d\n",
        studyMeta$name, studyMeta$version,
        nNodes, nEdges,
        if (length(exposures)) paste(exposures, collapse = ", ") else "<none>",
        if (length(outcomes))  paste(outcomes,  collapse = ", ") else "<none>",
        length(boundNodes),
        unresolved
      ))
      invisible(self)
    },

    # ---- Accessors --------------------------------------------------------

    #' @description Return the `dagitty` graph object.
    dagittyGraph = function() private$dagittyGraph_,

    #' @description Return the constructs data frame.
    constructs = function() private$constructs_,

    #' @description Return the edges data frame.
    edges = function() private$edges_,

    #' @description Return the bindings list.
    bindings = function() private$bindings_,

    #' @description Return the metadata list.
    metadata = function() private$metadata_,

    #' @description Return the roles data frame.
    roles = function() private$roles_,

    #' @description Return the chosen adjustment sets list.
    adjustmentSets = function() private$adjustmentSets_,

    # ---- Mutation implementations -----------------------------------------

    #' @description Add a node to the DAG.
    #' @param name Node name.
    #' @param conceptId OMOP concept ID.
    #' @param domain OMOP domain string or NA.
    addNodeImpl = function(name, conceptId, domain) {
      private$constructs_ <- rbind(
        private$constructs_,
        data.frame(name = name, conceptId = conceptId, domain = domain,
                   stringsAsFactors = FALSE)
      )
      private$dagittyGraph_ <- rebuildDagitty(
        private$constructs_, private$edges_, private$roles_
      )
      invisible(self)
    },

    #' @description Add a directed edge to the DAG.
    #' @param cause Cause node name.
    #' @param effect Effect node name.
    #' @param evidence Free-text evidence note or NA.
    addEdgeImpl = function(cause, effect, evidence) {
      private$edges_ <- rbind(
        private$edges_,
        data.frame(cause = cause, effect = effect,
                   evidence = if (is.na(evidence)) NA_character_ else evidence,
                   stringsAsFactors = FALSE)
      )
      private$dagittyGraph_ <- rebuildDagitty(
        private$constructs_, private$edges_, private$roles_
      )
      invisible(self)
    },

    #' @description Set a causal role for a node.
    #' @param node Node name.
    #' @param role Role string.
    setRoleImpl = function(node, role) {
      existingRoles <- private$roles_$role[private$roles_$node == node]
      for (conflictPair in private$roleConflicts_) {
        if (role %in% conflictPair) {
          conflictingRole <- setdiff(conflictPair, role)
          if (conflictingRole %in% existingRoles) {
            stop(sprintf(
              "Node '%s' already has role '%s'; cannot also assign '%s'.",
              node, conflictingRole, role
            ), call. = FALSE)
          }
        }
      }
      if (!any(private$roles_$node == node & private$roles_$role == role)) {
        private$roles_ <- rbind(
          private$roles_,
          data.frame(node = node, role = role, stringsAsFactors = FALSE)
        )
      }
      private$dagittyGraph_ <- rebuildDagitty(
        private$constructs_, private$edges_, private$roles_
      )
      invisible(self)
    },

    #' @description Register a phenotype binding for a node.
    #' @param node Node name.
    #' @param alias Alias key for this binding.
    #' @param bindingData Named list describing the binding.
    bindPhenotypeImpl = function(node, alias, bindingData) {
      if (is.null(private$bindings_[[node]])) {
        private$bindings_[[node]] <- list(active = alias, alternatives = list())
      }
      private$bindings_[[node]]$alternatives[[alias]] <- bindingData
      if (is.null(private$bindings_[[node]]$active) ||
          length(private$bindings_[[node]]$alternatives) == 1L) {
        private$bindings_[[node]]$active <- alias
      }
      invisible(self)
    },

    #' @description Switch the active binding alias for a node.
    #' @param node Node name.
    #' @param alias Alias to activate.
    setActiveBindingImpl = function(node, alias) {
      private$bindings_[[node]]$active <- alias
      invisible(self)
    },

    #' @description Store a chosen adjustment set for an exposure-outcome pair.
    #' @param key Character key `"<exposure>__<outcome>"`.
    #' @param index Integer index into the available sets.
    #' @param nodes Character vector of the chosen set.
    setAdjustmentSetImpl = function(key, index, nodes) {
      private$adjustmentSets_[[key]] <- list(index = index, nodes = nodes)
      invisible(self)
    }
  )
)
