make_nodes <- function() {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "A", 1L, "Condition")
  dag <- addNode(dag, "B", 2L, "Condition")
  dag <- addNode(dag, "C", 3L, "Condition")
  dag
}

test_that("addCausal scalar pair adds one edge", {
  dag <- addCausal(make_nodes(), "A", "B")
  expect_equal(nrow(dag$edges()), 1L)
})

test_that("addCausal broadcast one-to-many", {
  dag <- addCausal(make_nodes(), "A", c("B", "C"))
  expect_equal(nrow(dag$edges()), 2L)
})

test_that("addCausal broadcast many-to-one", {
  dag <- addCausal(make_nodes(), c("A", "B"), "C")
  expect_equal(nrow(dag$edges()), 2L)
})

test_that("addCausal pairwise equal-length vectors", {
  dag <- addCausal(make_nodes(), c("A", "B"), c("B", "C"))
  expect_equal(nrow(dag$edges()), 2L)
})

test_that("addCausal errors on mismatched multi-vectors", {
  expect_error(addCausal(make_nodes(), c("A", "B"), c("B", "C", "A")), "equal-length")
})

test_that("addCausal rejects cycle and names the path", {
  dag <- addCausal(make_nodes(), "A", "B")
  dag <- addCausal(dag,          "B", "C")
  expect_error(addCausal(dag, "C", "A"), "cycle")
})

test_that("addCausal rejects duplicate edge", {
  dag <- addCausal(make_nodes(), "A", "B")
  expect_error(addCausal(dag, "A", "B"), "already exists")
})

test_that("addCausal errors on unknown nodes", {
  dag <- make_nodes()
  expect_error(addCausal(dag, "X", "A"), "does not exist")
  expect_error(addCausal(dag, "A", "X"), "does not exist")
})
