test_that("addNode happy path adds the node", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "Hypertension", 320128L, "Condition")
  expect_equal(nrow(dag$constructs()), 1L)
  expect_equal(dag$constructs()$name,      "Hypertension")
  expect_equal(dag$constructs()$conceptId, 320128L)
  expect_equal(dag$constructs()$domain,    "Condition")
})

test_that("addNode accepts NA domain", {
  dag <- emptyOmopDag("Test")
  expect_no_error(addNode(dag, "X", 1L))
})

test_that("addNode errors on duplicate node name", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "X", 1L)
  expect_error(addNode(dag, "X", 2L), "already exists")
})

test_that("addNode errors on invalid domain", {
  dag <- emptyOmopDag("Test")
  expect_error(addNode(dag, "X", 1L, domain = "NotADomain"), "valid OMOP domain")
})

test_that("addNode errors on non-positive conceptId", {
  dag <- emptyOmopDag("Test")
  expect_error(addNode(dag, "X", -1L),  "positive integer")
  expect_error(addNode(dag, "X", 0L),   "positive integer")
  expect_error(addNode(dag, "X", "abc"), "positive integer")
})

test_that("addNode errors on non-OmopDag input", {
  expect_error(addNode(list(), "X", 1L), "OmopDag")
})

test_that("pipe chaining works", {
  dag <- emptyOmopDag("Test") |>
    addNode("A", 1L) |>
    addNode("B", 2L)
  expect_equal(nrow(dag$constructs()), 2L)
})
