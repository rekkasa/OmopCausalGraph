test_that("addEdgesFromCsv happy path", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "A", 1L)
  dag <- addNode(dag, "B", 2L)
  dag <- addNode(dag, "C", 3L)

  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("cause,effect", "A,B", "B,C"), csv)
  dag <- addEdgesFromCsv(dag, csv)
  expect_equal(nrow(dag$edges()), 2L)
})

test_that("addEdgesFromCsv errors on missing required columns", {
  dag <- emptyOmopDag("Test")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("cause", "A"), csv)
  expect_error(addEdgesFromCsv(dag, csv), "effect")
})

test_that("addEdgesFromCsv rolls back when one row creates a cycle", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "A", 1L)
  dag <- addNode(dag, "B", 2L)
  dag <- addCausal(dag, "A", "B")

  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("cause,effect", "B,A"), csv)  # would create cycle
  expect_error(addEdgesFromCsv(dag, csv), "cycle")
  expect_equal(nrow(dag$edges()), 1L)
})
