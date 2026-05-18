test_that("setActiveBinding switches the active alias", {
  dag  <- emptyOmopDag("Test")
  dag  <- addNode(dag, "E", 1L)
  json <- '{"ConceptSets":[]}'
  dag  <- bindPhenotype(dag, "E", type = "atlasJson", definition = json)
  dag  <- bindPhenotype(dag, "E", type = "atlasJson", definition = json, alias = "v2")
  dag  <- setActiveBinding(dag, "E", alias = "v2")
  expect_equal(dag$bindings()[["E"]]$active, "v2")
})

test_that("setActiveBinding errors on unknown alias", {
  dag  <- emptyOmopDag("Test")
  dag  <- addNode(dag, "E", 1L)
  dag  <- bindPhenotype(dag, "E", type = "atlasJson", definition = '{"ConceptSets":[]}')
  expect_error(setActiveBinding(dag, "E", "ghost"), "not found")
})

test_that("setActiveBinding errors on node with no bindings", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L)
  expect_error(setActiveBinding(dag, "E", "default"), "no bindings")
})
