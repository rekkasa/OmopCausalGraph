make_nodes <- function() {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "A", 1L, "Condition")
  omopCausalGraph <- addNode(omopCausalGraph, "B", 2L, "Condition")
  omopCausalGraph <- addNode(omopCausalGraph, "C", 3L, "Condition")
  omopCausalGraph
}

test_that("addCausal scalar pair adds one edge", {
  omopCausalGraph <- addCausal(make_nodes(), "A", "B")
  expect_equal(nrow(omopCausalGraph$edges()), 1L)
})

test_that("addCausal broadcast one-to-many", {
  omopCausalGraph <- addCausal(make_nodes(), "A", c("B", "C"))
  expect_equal(nrow(omopCausalGraph$edges()), 2L)
})

test_that("addCausal broadcast many-to-one", {
  omopCausalGraph <- addCausal(make_nodes(), c("A", "B"), "C")
  expect_equal(nrow(omopCausalGraph$edges()), 2L)
})

test_that("addCausal pairwise equal-length vectors", {
  omopCausalGraph <- addCausal(make_nodes(), c("A", "B"), c("B", "C"))
  expect_equal(nrow(omopCausalGraph$edges()), 2L)
})

test_that("addCausal errors on mismatched multi-vectors", {
  expect_error(addCausal(make_nodes(), c("A", "B"), c("B", "C", "A")), "equal-length")
})

test_that("addCausal rejects cycle and names the path", {
  omopCausalGraph <- addCausal(make_nodes(), "A", "B")
  omopCausalGraph <- addCausal(omopCausalGraph,          "B", "C")
  expect_error(addCausal(omopCausalGraph, "C", "A"), "cycle")
})

test_that("addCausal rejects duplicate edge", {
  omopCausalGraph <- addCausal(make_nodes(), "A", "B")
  expect_error(addCausal(omopCausalGraph, "A", "B"), "already exists")
})

test_that("addCausal errors on unknown nodes", {
  omopCausalGraph <- make_nodes()
  expect_error(addCausal(omopCausalGraph, "X", "A"), "does not exist")
  expect_error(addCausal(omopCausalGraph, "A", "X"), "does not exist")
})
