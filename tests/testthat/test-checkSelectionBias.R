test_that("checkSelectionBias returns empty data.frame when no selected nodes", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "E", 1L)
  omopCausalGraph <- addNode(omopCausalGraph, "O", 2L)
  omopCausalGraph <- setExposure(omopCausalGraph, "E")
  omopCausalGraph <- setOutcome(omopCausalGraph,  "O")
  result <- checkSelectionBias(omopCausalGraph)
  expect_equal(nrow(result), 0L)
  expect_true(is.data.frame(result))
})

test_that("checkSelectionBias returns rows for collider-selected node", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "E", 1L, "Drug")
  omopCausalGraph <- addNode(omopCausalGraph, "O", 2L, "Condition")
  omopCausalGraph <- addNode(omopCausalGraph, "S", 3L, "Observation")
  omopCausalGraph <- addCausal(omopCausalGraph, "E", "O")
  omopCausalGraph <- addCausal(omopCausalGraph, "E", "S")
  omopCausalGraph <- addCausal(omopCausalGraph, "O", "S")
  omopCausalGraph <- setExposure(omopCausalGraph, "E")
  omopCausalGraph <- setOutcome(omopCausalGraph,  "O")
  omopCausalGraph <- setSelected(omopCausalGraph, "S")
  result <- checkSelectionBias(omopCausalGraph)
  expect_true(is.data.frame(result))
})

test_that("checkSelectionBias errors on non-OmopCausalGraph", {
  expect_error(checkSelectionBias(list()), "OmopCausalGraph")
})
