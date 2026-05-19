test_that("plotDag returns a ggplot object", {
  omopCausalGraph <- makeTestOmopCausalGraph()
  p <- plotOmopCausalGraph(omopCausalGraph)
  expect_s3_class(p, "ggplot")
})

test_that("plotOmopCausalGraph does not error with custom node_colors", {
  omopCausalGraph <- makeTestOmopCausalGraph()
  expect_no_error(plotOmopCausalGraph(omopCausalGraph, node_colors = c(exposure = "#ff0000")))
})

test_that("plotDag result supports + extension", {
  omopCausalGraph <- makeTestOmopCausalGraph()
  p   <- plotOmopCausalGraph(omopCausalGraph) + ggplot2::ggtitle("Extended")
  expect_s3_class(p, "ggplot")
})

test_that("plotDag errors on non-OmopCausalGraph", {
  expect_error(plotOmopCausalGraph(list()), "OmopCausalGraph")
})
