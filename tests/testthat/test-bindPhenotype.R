test_that("atlasJson binding stores json and sha256", {
  omopCausalGraph  <- emptyOmopCausalGraph("Test")
  omopCausalGraph  <- addNode(omopCausalGraph, "E", 1L)
  json <- '{"ConceptSets":[]}'
  omopCausalGraph  <- bindPhenotype(omopCausalGraph, "E", type = "atlasJson", definition = json)
  b    <- omopCausalGraph$bindings()[["E"]]$alternatives[["default"]]
  expect_equal(b$type,   "atlasJson")
  expect_equal(b$json,   json)
  expect_false(is.null(b$sha256))
  expect_equal(nchar(b$sha256), 64L)
})

test_that("PhenotypeLibrary binding stores reference without resolving", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "E", 1L)
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "E", type = "PhenotypeLibrary",
                        definition = list(phenotypeId = 42L,
                                         commitHash = paste(rep("a", 40), collapse = "")))
  b <- omopCausalGraph$bindings()[["E"]]$alternatives[["default"]]
  expect_equal(b$type,        "PhenotypeLibrary")
  expect_equal(b$phenotypeId, 42L)
  expect_false(isTRUE(b$resolved))
})

test_that("demographics binding stores the column name", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "E", 1L)
  omopCausalGraph <- bindPhenotype(omopCausalGraph, "E", type = "demographics",
                        definition = "gender_concept_id")
  b <- omopCausalGraph$bindings()[["E"]]$alternatives[["default"]]
  expect_equal(b$type,   "demographics")
  expect_equal(b$column, "gender_concept_id")
})

test_that("alias registers an alternative binding", {
  omopCausalGraph  <- emptyOmopCausalGraph("Test")
  omopCausalGraph  <- addNode(omopCausalGraph, "E", 1L)
  json <- '{"ConceptSets":[]}'
  omopCausalGraph  <- bindPhenotype(omopCausalGraph, "E", type = "atlasJson", definition = json)
  omopCausalGraph  <- bindPhenotype(omopCausalGraph, "E", type = "atlasJson", definition = json,
                        alias = "v2")
  alts <- omopCausalGraph$bindings()[["E"]]$alternatives
  expect_true("v2" %in% names(alts))
  # active remains "default"
  expect_equal(omopCausalGraph$bindings()[["E"]]$active, "default")
})

test_that("bindPhenotype errors on invalid type", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "E", 1L)
  expect_error(bindPhenotype(omopCausalGraph, "E", type = "unknown", definition = "x"), "atlasJson")
})

test_that("bindPhenotype errors on unknown node", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  expect_error(bindPhenotype(omopCausalGraph, "Ghost", type = "atlasJson", definition = "{}"), "does not exist")
})

test_that("bindPhenotype errors on invalid JSON for atlasJson", {
  omopCausalGraph <- emptyOmopCausalGraph("Test")
  omopCausalGraph <- addNode(omopCausalGraph, "E", 1L)
  expect_error(bindPhenotype(omopCausalGraph, "E", type = "atlasJson", definition = "{bad json"),
               "valid JSON")
})
