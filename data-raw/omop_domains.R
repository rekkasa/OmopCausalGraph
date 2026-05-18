omop_domains <- c(
  "Condition", "Drug", "Procedure", "Measurement", "Observation",
  "Device", "Visit", "Specimen", "Note", "Death", "Demographics",
  "Metadata", "Type Concept", "Unit", "Currency", "Cost",
  "Provider", "Plan", "Sponsor", "Race", "Gender", "Ethnicity",
  "Geography", "Regimen", "Route", "Spec Anatomic Site",
  "Spec Disease Status", "Episode", "Relationship", "Revenue Code",
  "Place of Service", "Language", "Visit Detail", "Meas Value",
  "Condition Status", "Drug Exposure", "Drug Era", "Dose Era",
  "Condition Era"
)

usethis::use_data(omop_domains, internal = TRUE, overwrite = TRUE)
