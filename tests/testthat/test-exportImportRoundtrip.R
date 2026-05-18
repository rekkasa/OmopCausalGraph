test_that("export/import round-trip preserves structure and SHA-256", {
  dag <- make_test_dag()
  json <- '{"ConceptSets":[],"PrimaryCriteria":{"CriteriaList":[]}}'
  dag <- bindPhenotype(dag, "Exposure",   type = "atlasJson", definition = json)
  dag <- bindPhenotype(dag, "Outcome",    type = "atlasJson", definition = json)
  dag <- bindPhenotype(dag, "Confounder", type = "atlasJson", definition = json)
  dag <- setAdjustmentSet(dag, "Exposure", "Outcome", index = 1L)

  path <- withr::local_tempfile(fileext = ".json")
  exportOmopDag(dag, path)
  dag2 <- importOmopDag(path)

  # constructs
  expect_equal(
    sort(dag$constructs()$name),
    sort(dag2$constructs()$name)
  )

  # edges
  e1 <- dag$edges()[order(dag$edges()$cause, dag$edges()$effect), ]
  e2 <- dag2$edges()[order(dag2$edges()$cause, dag2$edges()$effect), ]
  expect_equal(e1$cause,  e2$cause)
  expect_equal(e1$effect, e2$effect)

  # roles
  r1 <- dag$roles()[order(dag$roles()$node, dag$roles()$role), ]
  r2 <- dag2$roles()[order(dag2$roles()$node, dag2$roles()$role), ]
  expect_equal(r1$node, r2$node)
  expect_equal(r1$role, r2$role)

  # SHA-256 survives round trip
  sha_orig <- dag$bindings()[["Exposure"]]$alternatives[["default"]]$sha256
  sha_reim <- dag2$bindings()[["Exposure"]]$alternatives[["default"]]$sha256
  expect_equal(sha_orig, sha_reim)

  # adjustment sets
  expect_equal(
    dag$adjustment_sets()$Exposure__Outcome$nodes,
    dag2$adjustment_sets()$Exposure__Outcome$nodes
  )
})
