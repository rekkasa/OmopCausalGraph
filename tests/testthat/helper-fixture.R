make_test_dag <- function() {
  dag <- emptyOmopDag("TestStudy")
  dag <- addNode(dag, "Confounder", 1L, "Condition")
  dag <- addNode(dag, "Exposure",   2L, "Drug")
  dag <- addNode(dag, "Outcome",    3L, "Condition")
  dag <- addNode(dag, "Selected",   4L, "Observation")
  dag <- addCausal(dag, "Confounder", c("Exposure", "Outcome"))
  dag <- addCausal(dag, "Exposure",  "Outcome")
  dag <- setExposure(dag,  "Exposure")
  dag <- setOutcome(dag,   "Outcome")
  dag <- setSelected(dag,  "Selected")
  dag
}
