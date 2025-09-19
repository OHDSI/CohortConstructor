domainsData <- dplyr::tribble(
  ~"domain_id",   ~"table",              ~"concept",              ~"source",                         ~"start",                     ~"end",
  "drug",        "drug_exposure",        "drug_concept_id",        "drug_source_concept_id",         "drug_exposure_start_date",   "drug_exposure_end_date",
  "condition",   "condition_occurrence", "condition_concept_id",   "condition_source_concept_id",    "condition_start_date",       "condition_end_date",
  "procedure",   "procedure_occurrence", "procedure_concept_id",   "procedure_source_concept_id",    "procedure_date",             "procedure_date",
  "observation", "observation",          "observation_concept_id", "observation_source_concept_id",  "observation_date",           "observation_date",
  "measurement", "measurement",          "measurement_concept_id", "measurement_source_concept_id",  "measurement_date",           "measurement_date",
  "visit",       "visit_occurrence",     "visit_concept_id",       "visit_source_concept_id",        "visit_start_date",           "visit_end_date",
  "device",      "device_exposure",      "device_concept_id",      "device_source_concept_id",       "device_exposure_start_date", "device_exposure_end_date",
  "visit_detail",       "visit_detail",     "visit_detail_concept_id",       "visit_detail_source_concept_id",        "visit_detail_start_date",           "visit_detail_end_date",
  "drug_era",       "drug_era",     "drug_concept_id",       NA,        "drug_era_start_date",           "drug_era_end_date",
  "condition_era",       "condition_era",     "condition_concept_id",       NA,        "condition_era_start_date",           "condition_era_end_date"
  )

jsons <- CDMConnector::readCohortSet(here::here("extras", "JSONCohorts")) |>
  dplyr::filter(.data$cohort_name != "first_depression")

load(here::here("extras", "benchmarkCodelists.RData"))

usethis::use_data(domainsData, jsons, codes, internal = TRUE, overwrite = TRUE)

