test_that("validateOmopCausalGraph passes for complete fixture with bindings and adjustment set", {
  omopCausalGraph <- makeTestOmopCausalGraph()
  json <- '{"ConceptSets":[]}'
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Exposure",   type = "atlasJson", definition = json)
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Outcome",    type = "atlasJson", definition = json)
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "Confounder", type = "atlasJson", definition = json)
  omopCausalGraph <- setAdjustmentSet(omopCausalGraph, "Exposure", "Outcome", index = 1L)
  result <- validateOmopCausalGraph(omopCausalGraph)
  expect_true(result$checks$has_exposure$pass)
  expect_true(result$checks$has_outcome$pass)
})

test_that("validateDag fails when no exposure", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "Outcome", 1L)
  omopCausalGraph <- setOutcome(omopCausalGraph, "Outcome")
  result <- validateOmopCausalGraph(omopCausalGraph)
  expect_false(result$checks$has_exposure$pass)
  expect_false(result$overall)
})

test_that("validateOmopCausalGraph fails when no outcome", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "Exposure", 1L)
  omopCausalGraph <- setExposure(omopCausalGraph, "Exposure")
  result <- validateOmopCausalGraph(omopCausalGraph)
  expect_false(result$checks$has_outcome$pass)
  expect_false(result$overall)
})

test_that("validateOmopCausalGraph fails when adjustment set not recorded", {
  omopCausalGraph <- makeTestOmopCausalGraph()
  result <- validateOmopCausalGraph(omopCausalGraph)
  expect_false(result$checks$adjustment_sets_recorded$pass)
})
