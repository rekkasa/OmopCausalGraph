test_that("addNodesFromCsv happy path", {
  dag <- emptyOmopDag("Test")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("name,conceptId,domain", "Cond,100,Condition", "Drug,200,Drug"), csv)
  dag <- addNodesFromCsv(dag, csv)
  expect_equal(nrow(dag$constructs()), 2L)
})

test_that("addNodesFromCsv errors on missing required columns", {
  dag <- emptyOmopDag("Test")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("name,domain", "Cond,Condition"), csv)
  expect_error(addNodesFromCsv(dag, csv), "conceptId")
})

test_that("addNodesFromCsv all-or-nothing: rolls back on bad row", {
  dag <- emptyOmopDag("Test")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("name,conceptId,domain", "Good,100,Condition", "Bad,-1,Drug"), csv)
  expect_error(addNodesFromCsv(dag, csv), "Row 2")
  expect_equal(nrow(dag$constructs()), 0L)
})

test_that("addNodesFromCsv errors on nonexistent file", {
  dag <- emptyOmopDag("Test")
  expect_error(addNodesFromCsv(dag, "/no/such/file.csv"), "File not found")
})
