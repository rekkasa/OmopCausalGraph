test_that("setAdjustmentSet records the chosen set", {
  dag <- make_test_dag()
  dag <- setAdjustmentSet(dag, "Exposure", "Outcome", index = 1L)
  adj <- dag$adjustment_sets()
  expect_true("Exposure__Outcome" %in% names(adj))
  expect_equal(adj$Exposure__Outcome$index, 1L)
  expect_true("Confounder" %in% adj$Exposure__Outcome$nodes)
})

test_that("setAdjustmentSet errors on out-of-range index", {
  dag <- make_test_dag()
  expect_error(setAdjustmentSet(dag, "Exposure", "Outcome", index = 999L), "out of range")
})

test_that("setAdjustmentSet errors when exposure not set", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "A", 1L)
  dag <- addNode(dag, "B", 2L)
  expect_error(setAdjustmentSet(dag, "A", "B", 1L), "not an exposure")
})
