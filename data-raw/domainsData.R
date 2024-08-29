## code to prepare `DATASET` dataset goes here

domainsData <- dplyr::tribble(
  ~"domain_id",   ~"table",              ~"concept",              ~"source",                         ~"start",                     ~"end",
  "drug",        "drug_exposure",        "drug_concept_id",        "drug_source_concept_id",         "drug_exposure_start_date",   "drug_exposure_end_date",
  "condition",   "condition_occurrence", "condition_concept_id",   "condition_source_concept_id",    "condition_start_date",       "condition_end_date",
  "procedure",   "procedure_occurrence", "procedure_concept_id",   "procedure_source_concept_id",    "procedure_date",             "procedure_date",
  "observation", "observation",          "observation_concept_id", "observation_source_concept_id",  "observation_date",           "observation_date",
  "measurement", "measurement",          "measurement_concept_id", "measurement_source_concept_id",  "measurement_date",           "measurement_date",
  "visit",       "visit_occurrence",     "visit_concept_id",       "visit_source_concept_id",        "visit_start_date",           "visit_end_date",
  "device",      "device_exposure",      "device_concept_id",      "device_source_concept_id",       "device_exposure_start_date", "device_exposure_end_date"
)

usethis::use_data(domainsData, internal = TRUE, overwrite = TRUE)
