test_that("setExposure sets the exposure role", {
  dag <- makeTestOmopCausalGraph()
  roles <- dag$roles()
  expect_true("Exposure" %in% roles$node[roles$role == "exposure"])
})

test_that("setOutcome sets the outcome role", {
  dag <- makeTestOmopCausalGraph()
  roles <- dag$roles()
  expect_true("Outcome" %in% roles$node[roles$role == "outcome"])
})

test_that("setUnobserved sets the unobserved role", {
  dag <- emptyOmopCausalGraph("Test")
  dag <- addNode(dag, "U", 99L)
  dag <- setUnobserved(dag, "U")
  expect_true("U" %in% dag$roles()$node[dag$roles()$role == "unobserved"])
})

test_that("setAdjusted sets the adjusted role", {
  dag <- emptyOmopCausalGraph("Test")
  dag <- addNode(dag, "C", 1L)
  dag <- setAdjusted(dag, "C")
  expect_true("C" %in% dag$roles()$node[dag$roles()$role == "adjusted"])
})

test_that("setSelected sets the selected role", {
  dag <- makeTestOmopCausalGraph()
  expect_true("Selected" %in% dag$roles()$node[dag$roles()$role == "selected"])
})

test_that("role setter errors on non-existent node", {
  dag <- emptyOmopCausalGraph("Test")
  expect_error(setExposure(dag, "Ghost"), "not found")
})

test_that("exposure + adjusted conflict is rejected", {
  dag <- emptyOmopCausalGraph("Test")
  dag <- addNode(dag, "X", 1L)
  dag <- setExposure(dag, "X")
  expect_error(setAdjusted(dag, "X"), "cannot also assign")
})

test_that("vectorized role assignment works", {
  dag <- emptyOmopCausalGraph("Test")
  dag <- addNode(dag, "A", 1L)
  dag <- addNode(dag, "B", 2L)
  dag <- setExposure(dag, c("A", "B"))
  expect_equal(sum(dag$roles()$role == "exposure"), 2L)
})

test_that("dagitty graph reflects exposure status after setExposure", {
  dag <- makeTestOmopCausalGraph()
  g   <- dag$dagittyGraph()
  expect_true("Exposure" %in% dagitty::exposures(g))
})

test_that("dagitty graph reflects outcome status after setOutcome", {
  dag <- makeTestOmopCausalGraph()
  g   <- dag$dagittyGraph()
  expect_true("Outcome" %in% dagitty::outcomes(g))
})
