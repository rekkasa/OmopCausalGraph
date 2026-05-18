test_that("constructor sets metadata fields", {
  dag <- emptyOmopDag("MyStudy", version = "1.0.0", author = "Smith")
  expect_equal(dag$name, "MyStudy")
  expect_equal(dag$metadata()$version, "1.0.0")
  expect_equal(dag$metadata()$author,  "Smith")
})

test_that("name mirrors metadata name", {
  dag <- emptyOmopDag("Mirror")
  expect_equal(dag$name, dag$metadata()$name)
})

test_that("print() does not error", {
  dag <- make_test_dag()
  expect_no_error(print(dag))
})

test_that("empty DAG has zero nodes and edges", {
  dag <- emptyOmopDag("Empty")
  expect_equal(nrow(dag$constructs()), 0L)
  expect_equal(nrow(dag$edges()),      0L)
})

test_that("constructor errors on missing name", {
  expect_error(emptyOmopDag(""),         "non-empty")
  expect_error(emptyOmopDag(character(0)), "non-empty")
})
