# Internal constant: valid OMOP domain IDs sourced from Athena DOMAIN vocabulary.
# Update from Athena when new domains are added.
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
