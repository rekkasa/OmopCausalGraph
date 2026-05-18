skip_if_not_installed("Eunomia")
skip_if_not_installed("CohortGenerator")
skip_if_not_installed("DatabaseConnector")
skip_on_cran()

test_that("executeDagPhenotypes returns summary with row counts", {
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

  dag <- emptyOmopDag("EunomiaTest")
  dag <- addNode(dag, "Confounder", 4216316L, "Condition")
  dag <- addNode(dag, "Exposure",   1118084L,  "Drug")
  dag <- addNode(dag, "Outcome",    192671L,   "Condition")
  dag <- addCausal(dag, "Confounder", c("Exposure", "Outcome"))
  dag <- addCausal(dag, "Exposure",  "Outcome")
  dag <- setExposure(dag, "Exposure")
  dag <- setOutcome(dag,  "Outcome")
  dag <- bindPhenotype(dag, "Exposure",   type = "atlasJson", definition = celecoxib_json)
  dag <- bindPhenotype(dag, "Outcome",    type = "atlasJson", definition = gi_bleed_json)
  dag <- bindPhenotype(dag, "Confounder", type = "atlasJson", definition = diclofenac_json)
  dag <- setAdjustmentSet(dag, "Exposure", "Outcome", index = 1L)

  result <- executeDagPhenotypes(
    dag,
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

test_that("executeDagPhenotypes hard-errors on unresolved PhenotypeLibrary binding", {
  dag <- make_test_dag()
  dag <- bindPhenotype(dag, "Exposure", type = "PhenotypeLibrary",
                        definition = list(phenotypeId = 1L,
                                         commitHash = paste(rep("a", 40), collapse = "")))
  dag <- bindPhenotype(dag, "Outcome",    type = "atlasJson", definition = '{"x":1}')
  dag <- bindPhenotype(dag, "Confounder", type = "atlasJson", definition = '{"x":1}')
  dag <- setAdjustmentSet(dag, "Exposure", "Outcome", index = 1L)

  cd <- Eunomia::getEunomiaConnectionDetails()
  expect_error(
    executeDagPhenotypes(dag, cd, "main", "main", "cohort"),
    "Unresolved bindings"
  )
})

test_that("executeDagPhenotypes hard-errors on unidentifiable DAG", {
  cd  <- Eunomia::getEunomiaConnectionDetails()
  dag <- emptyOmopDag("UnidentTest")
  dag <- addNode(dag, "U",        99L)
  dag <- addNode(dag, "Exposure", 1L, "Drug")
  dag <- addNode(dag, "Outcome",  2L, "Condition")
  dag <- addCausal(dag, "U", "Exposure")
  dag <- addCausal(dag, "U", "Outcome")
  dag <- addCausal(dag, "Exposure", "Outcome")
  dag <- setExposure(dag,   "Exposure")
  dag <- setOutcome(dag,    "Outcome")
  dag <- setUnobserved(dag, "U")
  dag <- bindPhenotype(dag, "Exposure", type = "atlasJson", definition = '{"x":1}')
  dag <- bindPhenotype(dag, "Outcome",  type = "atlasJson", definition = '{"x":1}')
  dag <- setAdjustmentSet(dag, "Exposure", "Outcome", index = 0L)

  expect_error(
    executeDagPhenotypes(dag, cd, "main", "main", "cohort"),
    "unidentifiable|adjustment set"
  )

  expect_warning(
    executeDagPhenotypes(dag, cd, "main", "main", "cohort", allowIncomplete = TRUE),
    "unidentifiable|bias"
  )
})
