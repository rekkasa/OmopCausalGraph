test_that("validateDag passes for complete fixture with bindings and adjustment set", {
  dag <- make_test_dag()
  json <- '{"ConceptSets":[]}'
  dag <- bindPhenotype(dag, "Exposure",   type = "atlasJson", definition = json)
  dag <- bindPhenotype(dag, "Outcome",    type = "atlasJson", definition = json)
  dag <- bindPhenotype(dag, "Confounder", type = "atlasJson", definition = json)
  dag <- setAdjustmentSet(dag, "Exposure", "Outcome", index = 1L)
  result <- validateDag(dag)
  expect_true(result$checks$has_exposure$pass)
  expect_true(result$checks$has_outcome$pass)
})

test_that("validateDag fails when no exposure", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "Outcome", 1L)
  dag <- setOutcome(dag, "Outcome")
  result <- validateDag(dag)
  expect_false(result$checks$has_exposure$pass)
  expect_false(result$overall)
})

test_that("validateDag fails when no outcome", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "Exposure", 1L)
  dag <- setExposure(dag, "Exposure")
  result <- validateDag(dag)
  expect_false(result$checks$has_outcome$pass)
  expect_false(result$overall)
})

test_that("validateDag fails when adjustment set not recorded", {
  dag <- make_test_dag()
  result <- validateDag(dag)
  expect_false(result$checks$adjustment_sets_recorded$pass)
})
