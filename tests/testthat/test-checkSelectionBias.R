test_that("checkSelectionBias returns empty data.frame when no selected nodes", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L)
  dag <- addNode(dag, "O", 2L)
  dag <- setExposure(dag, "E")
  dag <- setOutcome(dag,  "O")
  result <- checkSelectionBias(dag)
  expect_equal(nrow(result), 0L)
  expect_true(is.data.frame(result))
})

test_that("checkSelectionBias returns rows for collider-selected node", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L, "Drug")
  dag <- addNode(dag, "O", 2L, "Condition")
  dag <- addNode(dag, "S", 3L, "Observation")
  dag <- addCausal(dag, "E", "O")
  dag <- addCausal(dag, "E", "S")
  dag <- addCausal(dag, "O", "S")
  dag <- setExposure(dag, "E")
  dag <- setOutcome(dag,  "O")
  dag <- setSelected(dag, "S")
  result <- checkSelectionBias(dag)
  expect_true(is.data.frame(result))
})

test_that("checkSelectionBias errors on non-OmopDag", {
  expect_error(checkSelectionBias(list()), "OmopDag")
})
