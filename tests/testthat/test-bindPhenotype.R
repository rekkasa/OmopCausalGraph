test_that("atlasJson binding stores json and sha256", {
  dag  <- emptyOmopDag("Test")
  dag  <- addNode(dag, "E", 1L)
  json <- '{"ConceptSets":[]}'
  dag  <- bindPhenotype(dag, "E", type = "atlasJson", definition = json)
  b    <- dag$bindings()[["E"]]$alternatives[["default"]]
  expect_equal(b$type,   "atlasJson")
  expect_equal(b$json,   json)
  expect_false(is.null(b$sha256))
  expect_equal(nchar(b$sha256), 64L)
})

test_that("PhenotypeLibrary binding stores reference without resolving", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L)
  dag <- bindPhenotype(dag, "E", type = "PhenotypeLibrary",
                        definition = list(phenotypeId = 42L,
                                         commitHash = paste(rep("a", 40), collapse = "")))
  b <- dag$bindings()[["E"]]$alternatives[["default"]]
  expect_equal(b$type,        "PhenotypeLibrary")
  expect_equal(b$phenotypeId, 42L)
  expect_false(isTRUE(b$resolved))
})

test_that("demographics binding stores the column name", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L)
  dag <- bindPhenotype(dag, "E", type = "demographics",
                        definition = "gender_concept_id")
  b <- dag$bindings()[["E"]]$alternatives[["default"]]
  expect_equal(b$type,   "demographics")
  expect_equal(b$column, "gender_concept_id")
})

test_that("alias registers an alternative binding", {
  dag  <- emptyOmopDag("Test")
  dag  <- addNode(dag, "E", 1L)
  json <- '{"ConceptSets":[]}'
  dag  <- bindPhenotype(dag, "E", type = "atlasJson", definition = json)
  dag  <- bindPhenotype(dag, "E", type = "atlasJson", definition = json,
                        alias = "v2")
  alts <- dag$bindings()[["E"]]$alternatives
  expect_true("v2" %in% names(alts))
  # active remains "default"
  expect_equal(dag$bindings()[["E"]]$active, "default")
})

test_that("bindPhenotype errors on invalid type", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L)
  expect_error(bindPhenotype(dag, "E", type = "unknown", definition = "x"), "atlasJson")
})

test_that("bindPhenotype errors on unknown node", {
  dag <- emptyOmopDag("Test")
  expect_error(bindPhenotype(dag, "Ghost", type = "atlasJson", definition = "{}"), "does not exist")
})

test_that("bindPhenotype errors on invalid JSON for atlasJson", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L)
  expect_error(bindPhenotype(dag, "E", type = "atlasJson", definition = "{bad json"),
               "valid JSON")
})
