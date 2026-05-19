test_that("constructor sets metadata fields", {
  omopCausalGraph <- emptyOmopCausalGraph("MyStudy", version = "1.0.0", author = "Smith")
  expect_equal(omopCausalGraph$name, "MyStudy")
  expect_equal(omopCausalGraph$metadata()$version, "1.0.0")
  expect_equal(omopCausalGraph$metadata()$author,  "Smith")
})

test_that("name mirrors metadata name", {
  omopCausalGraph <- emptyOmopCausalGraph("Mirror")
  expect_equal(omopCausalGraph$name, omopCausalGraph$metadata()$name)
})

test_that("print() does not error", {
  omopCausalGraph <- makeTestOmopCausalGraph()
  expect_no_error(print(omopCausalGraph))
})

test_that("empty DAG has zero nodes and edges", {
  omopCausalGraph <- emptyOmopCausalGraph("Empty")
  expect_equal(nrow(omopCausalGraph$constructs()), 0L)
  expect_equal(nrow(omopCausalGraph$edges()),      0L)
})

test_that("constructor errors on missing name", {
  expect_error(emptyOmopCausalGraph(""),         "non-empty")
  expect_error(emptyOmopCausalGraph(character(0)), "non-empty")
})
