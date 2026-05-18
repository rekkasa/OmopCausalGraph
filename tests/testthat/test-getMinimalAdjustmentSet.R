test_that("getMinimalAdjustmentSet returns Confounder for fixture", {
  dag  <- make_test_dag()
  sets <- getMinimalAdjustmentSet(dag)
  key  <- "Exposure__Outcome"
  expect_true(key %in% names(sets))
  expect_true(length(sets[[key]]) >= 1L)
  # Confounder should be in at least one set
  all_nodes <- unlist(sets[[key]], use.names = FALSE)
  expect_true("Confounder" %in% all_nodes)
})

test_that("getMinimalAdjustmentSet print() does not error", {
  dag  <- make_test_dag()
  sets <- getMinimalAdjustmentSet(dag)
  expect_no_error(print(sets))
})

test_that("getMinimalAdjustmentSet returns empty list when no exposure/outcome", {
  dag  <- emptyOmopDag("Test")
  sets <- getMinimalAdjustmentSet(dag)
  expect_equal(length(sets), 0L)
})
