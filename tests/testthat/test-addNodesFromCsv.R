test_that("addNodesFromCsv happy path", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("name,conceptId,domain", "Cond,100,Condition", "Drug,200,Drug"), csv)
  omopCausalGraph <- addNodesFromCsv(omopCausalGraph, csv)
  expect_equal(nrow(omopCausalGraph$constructs()), 2L)
})

test_that("addNodesFromCsv errors on missing required columns", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("name,domain", "Cond,Condition"), csv)
  expect_error(addNodesFromCsv(omopCausalGraph, csv), "conceptId")
})

test_that("addNodesFromCsv all-or-nothing: rolls back on bad row", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("name,conceptId,domain", "Good,100,Condition", "Bad,-1,Drug"), csv)
  expect_error(addNodesFromCsv(omopCausalGraph, csv), "Row 2")
  expect_equal(nrow(omopCausalGraph$constructs()), 0L)
})

test_that("addNodesFromCsv errors on nonexistent file", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  expect_error(addNodesFromCsv(omopCausalGraph, "/no/such/file.csv"), "File not found")
})
