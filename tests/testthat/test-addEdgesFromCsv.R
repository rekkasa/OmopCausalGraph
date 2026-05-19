test_that("addEdgesFromCsv happy path", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "A", 1L)
  omopCausalGraph <- addNode(omopCausalGraph, "B", 2L)
  omopCausalGraph <- addNode(omopCausalGraph, "C", 3L)

  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("cause,effect", "A,B", "B,C"), csv)
  omopCausalGraph <- addEdgesFromCsv(omopCausalGraph, csv)
  expect_equal(nrow(omopCausalGraph$edges()), 2L)
})

test_that("addEdgesFromCsv errors on missing required columns", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("cause", "A"), csv)
  expect_error(addEdgesFromCsv(omopCausalGraph, csv), "effect")
})

test_that("addEdgesFromCsv rolls back when one row creates a cycle", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "A", 1L)
  omopCausalGraph <- addNode(omopCausalGraph, "B", 2L)
  omopCausalGraph <- addCausal(omopCausalGraph, "A", "B")

  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("cause,effect", "B,A"), csv)  # would create cycle
  expect_error(addEdgesFromCsv(omopCausalGraph, csv), "cycle")
  expect_equal(nrow(omopCausalGraph$edges()), 1L)
})
