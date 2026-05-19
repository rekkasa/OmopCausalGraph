makeTestOmopCausalGraph <- function() {
  omopCausalGraph <- emptyOmopCausalGraph("TestStudy")
  omopCausalGraph <- addNode(omopCausalGraph, "Confounder", 1L, "Condition")
  omopCausalGraph <- addNode(omopCausalGraph, "Exposure",   2L, "Drug")
  omopCausalGraph <- addNode(omopCausalGraph, "Outcome",    3L, "Condition")
  omopCausalGraph <- addNode(omopCausalGraph, "Selected",   4L, "Observation")
  omopCausalGraph <- addCausal(omopCausalGraph, "Confounder", c("Exposure", "Outcome"))
  omopCausalGraph <- addCausal(omopCausalGraph, "Exposure",  "Outcome")
  omopCausalGraph <- setExposure(omopCausalGraph,  "Exposure")
  omopCausalGraph <- setOutcome(omopCausalGraph,   "Outcome")
  omopCausalGraph <- setSelected(omopCausalGraph,  "Selected")
  omopCausalGraph
}
