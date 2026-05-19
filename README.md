
<!-- README.md is generated from README.Rmd. Run `devtools::build_readme()` to regenerate. -->

# OmopCausalGraph

<!-- badges: start -->

[![License: Apache
2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![R-CMD-check](https://github.com/OHDSI/OmopCausalGraph/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/OmopCausalGraph/actions)
<!-- badges: end -->

`OmopCausalGraph` bridges causal directed acyclic graphs (DAGs) with
OMOP Common Data Model (CDM) concept identifiers and executable
phenotype definitions. The central abstraction is an `OmopCausalGraph`
R6 class that holds a `dagitty` graph, a construct dictionary anchored
to OMOP Standard Concept IDs, a phenotype binding registry, and study
metadata in a single mutable object.

## Installation

``` r
# Using renv (recommended):
renv::install("OHDSI/OmopCausalGraph")

# Or with pak:
pak::pak("OHDSI/OmopCausalGraph")
```

## Quick start

``` r
library(OmopCausalGraph)

# 1. Create the DAG
dag <- emptyOmopCausalGraph("NSAID GI Bleed Study", author = "Smith J")

dag <- addNode(dag, "Age",        conceptId = 4216316L, domain = "Observation")
dag <- addNode(dag, "Celecoxib",  conceptId = 1118084L, domain = "Drug")
dag <- addNode(dag, "GI_Bleed",   conceptId = 192671L,  domain = "Condition")

dag <- addCausal(dag, "Age",       c("Celecoxib", "GI_Bleed"))
dag <- addCausal(dag, "Celecoxib", "GI_Bleed")

# 2. Set causal roles
dag <- setExposure(dag, "Celecoxib")
dag <- setOutcome(dag,  "GI_Bleed")

# 3. Compute and record the adjustment set
sets <- getMinimalAdjustmentSet(dag)
print(sets)
dag <- setAdjustmentSet(dag, "Celecoxib", "GI_Bleed", index = 1L)

# 4. Bind phenotype definitions
atlasJson <- readLines("inst/atlas/celecoxib.json", warn = FALSE) |> paste(collapse = "\n")
dag <- bindPhenotype(dag, "Celecoxib", type = "atlasJson", definition = atlasJson)
dag <- bindPhenotype(dag, "GI_Bleed",  type = "atlasJson", definition = atlasJson)
dag <- bindPhenotype(dag, "Age",       type = "demographics", definition = "year_of_birth")

# PhenotypeLibrary reference (fetch later with resolvePhenotypes())
dag <- bindPhenotype(dag, "Celecoxib",
  type       = "PhenotypeLibrary",
  definition = list(phenotypeId = 1L, commitHash = "abc123..."),
  alias      = "pl_ref"
)

# 5. Visualise
p <- plotDag(dag)
print(p)

# 6. Export as JSON-LD lockfile
exportOmopCausalGraph(dag, "nsaid_gi_bleed_v1.json")

# 7. Re-import in another session
dag2 <- importOmopCausalGraph("nsaid_gi_bleed_v1.json")
```

## Execute cohorts in an OMOP CDM

``` r
# Resolve any PhenotypeLibrary references before entering a locked environment
dag <- resolvePhenotypes(dag)

cd <- DatabaseConnector::createConnectionDetails(
  dbms     = "postgresql",
  server   = Sys.getenv("CDM_SERVER"),
  user     = Sys.getenv("CDM_USER"),
  password = Sys.getenv("CDM_PASSWORD")
)

result <- executeDagPhenotypes(
  dag,
  connectionDetails = cd,
  cdmSchema         = "cdm_v54",
  resultsSchema     = "results",
  cohortTable       = "cohort"
)
print(result)
```

## License

Apache 2.0
