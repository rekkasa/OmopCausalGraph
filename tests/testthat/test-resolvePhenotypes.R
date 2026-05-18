test_that("resolvePhenotypes marks binding as resolved on successful fetch", {
  skip_if_not_installed("mockery")

  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L)
  dag <- bindPhenotype(dag, "E", type = "PhenotypeLibrary",
                        definition = list(phenotypeId = 1L,
                                         commitHash = paste(rep("a", 40), collapse = "")))

  fake_json <- '{"ConceptSets":[]}'

  mockery::stub(
    resolvePhenotypes,
    ".resolve_one_binding",
    function(bl) {
      bl$json     <- fake_json
      bl$sha256   <- as.character(openssl::sha256(fake_json))
      bl$resolved <- TRUE
      bl
    }
  )

  dag <- resolvePhenotypes(dag)
  b   <- dag$bindings()[["E"]]$alternatives[["default"]]
  expect_true(isTRUE(b$resolved))
  expect_equal(b$json, fake_json)
  expect_false(is.null(b$sha256))
})

test_that("resolvePhenotypes skips already-resolved bindings", {
  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E", 1L)
  pre_resolved <- list(
    type        = "PhenotypeLibrary",
    phenotypeId = 1L,
    commitHash  = paste(rep("a", 40), collapse = ""),
    repo        = "OHDSI/PhenotypeLibrary",
    resolved    = TRUE,
    json        = '{"ConceptSets":[]}',
    sha256      = "abc123"
  )
  dag$bind_phenotype_impl("E", "default", pre_resolved)
  expect_no_error(resolvePhenotypes(dag))
})

test_that("resolvePhenotypes aggregates errors on partial failure", {
  skip_if_not_installed("mockery")

  dag <- emptyOmopDag("Test")
  dag <- addNode(dag, "E1", 1L)
  dag <- addNode(dag, "E2", 2L)
  for (n in c("E1", "E2")) {
    dag <- bindPhenotype(dag, n, type = "PhenotypeLibrary",
                          definition = list(phenotypeId = 1L,
                                           commitHash = paste(rep("a", 40), collapse = "")))
  }

  mockery::stub(
    resolvePhenotypes,
    ".resolve_one_binding",
    function(bl) stop("network error")
  )
  expect_error(resolvePhenotypes(dag), "Failed to resolve")
})
