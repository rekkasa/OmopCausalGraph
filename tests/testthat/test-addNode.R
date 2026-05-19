test_that("addNode happy path adds the node", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "Hypertension", 320128L, "Condition")
  expect_equal(nrow(omopCausalGraph$constructs()), 1L)
  expect_equal(omopCausalGraph$constructs()$name,      "Hypertension")
  expect_equal(omopCausalGraph$constructs()$conceptId, 320128L)
  expect_equal(omopCausalGraph$constructs()$domain,    "Condition")
})

test_that("addNode accepts NA domain", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  expect_no_error(addNode(omopCausalGraph, "X", 1L))
})

test_that("addNode errors on duplicate node name", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "X", 1L)
  expect_error(addNode(omopCausalGraph, "X", 2L), "already exists")
})

test_that("addNode errors on invalid domain", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  expect_error(addNode(omopCausalGraph, "X", 1L, domain = "NotADomain"), "valid OMOP domain")
})

test_that("addNode errors on non-positive conceptId", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  expect_error(addNode(omopCausalGraph, "X", -1L),  "positive integer")
  expect_error(addNode(omopCausalGraph, "X", 0L),   "positive integer")
  expect_error(addNode(omopCausalGraph, "X", "abc"), "positive integer")
})

test_that("addNode errors on non-OmopOmopCausalGraph input", {
  expect_error(addNode(list(), "X", 1L), "OmopCausalGraph")
})

test_that("pipe chaining works", {
  omopCausalGraph <- emptyOmopCausalGraph("Test") |>
    addNode("A", 1L) |>
    addNode("B", 2L)
  expect_equal(nrow(omopCausalGraph$constructs()), 2L)
})
