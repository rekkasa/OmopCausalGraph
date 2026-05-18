test_that("plotDag returns a ggplot object", {
  dag <- make_test_dag()
  p   <- plotDag(dag)
  expect_s3_class(p, "ggplot")
})

test_that("plotDag does not error with custom node_colors", {
  dag <- make_test_dag()
  expect_no_error(plotDag(dag, node_colors = c(exposure = "#ff0000")))
})

test_that("plotDag result supports + extension", {
  dag <- make_test_dag()
  p   <- plotDag(dag) + ggplot2::ggtitle("Extended")
  expect_s3_class(p, "ggplot")
})

test_that("plotDag errors on non-OmopDag", {
  expect_error(plotDag(list()), "OmopDag")
})
