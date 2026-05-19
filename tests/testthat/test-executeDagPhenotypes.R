skip_if_not_installed("Eunomia")
skip_if_not_installed("CohortGenerator")
skip_if_not_installed("DatabaseConnector")
skip_on_cran()

test_that("executeOmopCausalGraphPhenotypes returns summary with row counts", {
  cd <- Eunomia::getEunomiaConnectionDetails()

  # Celecoxib (1118084) -> GI bleed (192671) with age confounder
  celecoxib_json <- '{
    "ConceptSets": [{"id": 0, "name": "Celecoxib",
      "expression": {"items": [{"concept": {"CONCEPT_ID": 1118084}}]}}],
    "PrimaryCriteria": {"CriteriaList": [{"DrugExposure": {"CodesetId": 0}}],
      "ObservationWindow": {"PriorDays": 0, "PostDays": 0}},
    "QualifiedLimit": {"Type": "First"},
    "ExpressionLimit": {"Type": "First"},
    "InclusionRules": [], "EndStrategy": null,
    "CensoringCriteria": [], "CollapseSettings": {"CollapseType": "ERA", "EraPad": 0},
    "CensorWindow": {}, "cdmVersionRange": ">=5.0.0"
  }'

  gi_bleed_json <- '{
    "ConceptSets": [{"id": 0, "name": "GI bleed",
      "expression": {"items": [{"concept": {"CONCEPT_ID": 192671}}]}}],
    "PrimaryCriteria": {"CriteriaList": [{"ConditionOccurrence": {"CodesetId": 0}}],
      "ObservationWindow": {"PriorDays": 0, "PostDays": 0}},
    "QualifiedLimit": {"Type": "First"},
    "ExpressionLimit": {"Type": "First"},
    "InclusionRules": [], "EndStrategy": null,
    "CensoringCriteria": [], "CollapseSettings": {"CollapseType": "ERA", "EraPad": 0},
    "CensorWindow": {}, "cdmVersionRange": ">=5.0.0"
  }'

  diclofenac_json <- '{
    "ConceptSets": [{"id": 0, "name": "Diclofenac",
      "expression": {"items": [{"concept": {"CONCEPT_ID": 1124300}}]}}],
    "PrimaryCriteria": {"CriteriaList": [{"DrugExposure": {"CodesetId": 0}}],
      "ObservationWindow": {"PriorDays": 0, "PostDays": 0}},
    "QualifiedLimit": {"Type": "First"},
    "ExpressionLimit": {"Type": "First"},
    "InclusionRules": [], "EndStrategy": null,
    "CensoringCriteria": [], "CollapseSettings": {"CollapseType": "ERA", "EraPad": 0},
    "CensorWindow": {}, "cdmVersionRange": ">=5.0.0"
  }'

  omopCausalGraph <- emptyOmopCausalGraph("EunomiaTest")
  omopCausalGraph <- addNode(omopCausalGraph, "Confounder", 4216316L, "Condition")
  omopCausalGraph <- addNode(omopCausalGraph, "Exposure",   1118084L,  "Drug")
  omopCausalGraph <- addNode(omopCausalGraph, "Outcome",    192671L,   "Condition")
  omopCausalGraph <- addCausal(omopCausalGraph, "Confounder", c("Exposure", "Outcome"))
  omopCausalGraph <- addCausal(omopCausalGraph, "Exposure",  "Outcome")
  omopCausalGraph <- setExposure(omopCausalGraph, "Exposure")
  omopCausalGraph <- setOutcome(omopCausalGraph,  "Outcome")
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Exposure",   type = "atlasJson", definition = celecoxib_json)
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Outcome",    type = "atlasJson", definition = gi_bleed_json)
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Confounder", type = "atlasJson", definition = diclofenac_json)
  omopCausalGraph <- setAdjustmentSet(omopCausalGraph, "Exposure", "Outcome", index = 1L)

  result <- executeOmopCausalGraphPhenotypes(
    omopCausalGraph,
    connectionDetails = cd,
    cdmSchema         = "main",
    resultsSchema     = "main",
    cohortTable       = "cohort"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
  expect_true(all(c("node", "cohortId", "binding_type", "row_count") %in% names(result)))
  expect_true(all(!is.na(result$row_count)))
})

test_that("executeOmopCausalGraphPhenotypes hard-errors on unresolved PhenotypeLibrary binding", {
  omopCausalGraph <- make_test_omopCausalGraph()
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Exposure", type = "PhenotypeLibrary",
                        definition = list(phenotypeId = 1L,
                                         commitHash = paste(rep("a", 40), collapse = "")))
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Outcome",    type = "atlasJson", definition = '{"x":1}')
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Confounder", type = "atlasJson", definition = '{"x":1}')
  omopCausalGraph <- setAdjustmentSet(omopCausalGraph, "Exposure", "Outcome", index = 1L)

  cd <- Eunomia::getEunomiaConnectionDetails()
  expect_error(
    executeOmopCausalGraphPhenotypes(omopCausalGraph, cd, "main", "main", "cohort"),
    "Unresolved bindings"
  )
})

test_that("executeOmopCausalGraphPhenotypes hard-errors on unidentifiable DAG", {
  cd  <- Eunomia::getEunomiaConnectionDetails()
  omopCausalGraph <- emptyOmopCausalGraph("UnidentTest")
  omopCausalGraph <- addNode(omopCausalGraph, "U", 99L)
  omopCausalGraph <- addNode(omopCausalGraph, "Exposure", 1L, "Drug")
  omopCausalGraph <- addNode(omopCausalGraph, "Outcome",  2L, "Condition")
  omopCausalGraph <- addCausal(omopCausalGraph, "U", "Exposure")
  omopCausalGraph <- addCausal(omopCausalGraph, "U", "Outcome")
  omopCausalGraph <- addCausal(omopCausalGraph, "Exposure", "Outcome")
  omopCausalGraph <- setExposure(omopCausalGraph,   "Exposure")
  omopCausalGraph <- setOutcome(omopCausalGraph,    "Outcome")
  omopCausalGraph <- setUnobserved(omopCausalGraph, "U")
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Exposure", type = "atlasJson", definition = '{"x":1}')
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Outcome",  type = "atlasJson", definition = '{"x":1}')
  omopCausalGraph <- setAdjustmentSet(omopCausalGraph, "Exposure", "Outcome", index = 0L)

  expect_error(
    executeOmopCausalGraphPhenotypes(dag, cd, "main", "main", "cohort"),
    "unidentifiable|adjustment set"
  )

  expect_warning(
    executeOmopCausalGraphPhenotypes(dag, cd, "main", "main", "cohort", allowIncomplete = TRUE),
    "unidentifiable|bias"
  )
})
