test_that("importOmopCausalGraph hard-errors on major version mismatch", {
  path <- withr::local_tempfile(fileext = ".json")
  doc  <- list(
    `@context` = "https://omopcausalgraph.org/context/v1",
    version    = "99.0.0",
    metadata   = list(name = "X", version = "99.0.0"),
    constructs = list(), edges = list(), roles = list(),
    bindings   = list(), adjustmentSets = list()
  )
  jsonlite::write_json(doc, path, auto_unbox = TRUE)
  expect_error(importOmopCausalGraph(path), "Major version mismatch")
})

test_that("importOmopCausalGraph warns on patch version mismatch and proceeds", {
  path <- withr::local_tempfile(fileext = ".json")
  doc  <- list(
    `@context` = "https://omopcausalgraph.org/context/v1",
    version    = "0.1.99",
    metadata   = list(name = "X", version = "0.1.99"),
    constructs = list(), edges = list(), roles = list(),
    bindings   = list(), adjustmentSets = list()
  )
  jsonlite::write_json(doc, path, auto_unbox = TRUE)
  expect_warning(importOmopCausalGraph(path), "Version mismatch")
})
