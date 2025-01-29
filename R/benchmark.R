benchmarkCohortConstructor <- function(cdm) {
  # check inputs
  cdm <- omopgenerics::validateCdmArgument(cdm)

  # Read JSONs of cohorts to create ----
  jsons <- CDMConnector::readCohortSet(here::here("extras", "JSONCohorts")) |>
    dplyr::filter(.data$cohort_name != "first_depression")

  # Instantiate or read Atlas ----
  cli::cli_inform(c("*" = "Instantiating JSONs"))
  for (json in jsons$cohort_name_snakecase) {
    tictoc::tic(msg = paste0("atlas_", json))
    cdm <- CDMConnector::generateCohortSet(
      cdm = cdm,
      cohortSet = jsons |> dplyr::filter(.data$cohort_name_snakecase == .env$json),
      name = paste0("atlas_", json),
      computeAttrition = TRUE,
      overwrite = TRUE
    )
    tictoc::toc(log = TRUE)
  }

  # Get codelist ----
  concept_sets <- c(
    "inpatient_visit", "beta_blockers", "symptoms_for_transverse_myelitis",
    "asthma_therapy", "fluoroquinolone_systemic",
    "congenital_or_genetic_neutropenia_leukopenia_or_agranulocytosis",
    "endometriosis", "chronic_obstructive_lung_disease", "essential_hypertension",
    "asthma", "neutropenia_agranulocytosis_or_unspecified_leukopenia",
    "dementia", "schizophrenia_not_including_paraphenia", "psychotic_disorder",
    "bipolar_disorder", "major_depressive_disorder", "transverse_myelitis",
    "schizoaffective_disorder", "endometriosis_related_laproscopic_procedures",
    "long_acting_muscarinic_antagonists_lamas", "neutrophilia", "covid_19",
    "sars_cov_2_test", "neutrophil_absolute_count",
    "mncs_abdominal_aortic_aneurysm_repair", "mncs_above_knee_amputation",
    "mncs_adrenalectomy", "mncs_appendectomy", "mncs_below_knee_amputation",
    "mncs_breast_reconstruction", "mncs_celiac_artery_revascularization",
    "mncs_cerebrovascular_surgery", "mncs_cholecystectomy",
    "mncs_complex_visceral_resection_liver", "mncs_complex_visceral_resection_oesophagus",
    "mncs_complex_visceral_resection_pancreas_biliary", "mncs_craniotomy",
    "mncs_head_neck_resection", "mncs_hysterectomy_opherectomy",
    "mncs_iliac_femoral_bypass", "mncs_internal_fixation_femur", "mncs_knee_arthroplasty",
    "mncs_lobectomy", "mncs_lymph_node_dissection", "mncs_major_hip_pelvic_surgery",
    "mncs_mytoreductive_surgery", "mncs_peripheral_vascular_lower_limb_arterial_bypass",
    "mncs_pneumonectomy", "mncs_radical_hysterectomy", "mncs_radical_prostatectomy",
    "mncs_renal_artery_revascularization", "mncs_sb_colon_rectal", "mncs_splenectomy",
    "mncs_stomach_surgery", "mncs_thoracic_resection", "mncs_thoracic_vascular_surgery",
    "mncs_transurethral_prostatectomy", "mncs_ureteric_kidney_bladder_surgery"
  )
  codes_cdm <- CodelistGenerator::codesFromCohort(here::here("extras", "JSONCohorts"), cdm)
  codes <- as.list(rep(100001L, length(concept_sets))) # mock concept as pleace-holder
  names(codes) <- concept_sets
  for (nm in names(codes_cdm)) {
    codes[nm] <- codes_cdm[nm]
  }

  # CohortConstructor by definition ----
  tictoc::tic(msg = paste0("cc1_asthma_no_copd"))
  cdm <- getAsthma(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_beta_blockers_hypertension"))
  cdm <- getBetaBlockers(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_covid"))
  cdm <- getCovid(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_covid_female"))
  cdm <- getCovidStrata(
    cdm, codes, sex = "Female", ageRange = NULL, name = "cc1_covid_female"
  )
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_covid_female_0_to_50"))
  cdm <- getCovidStrata(
    cdm, codes, sex = "Female", ageRange = list(c(0, 50)), name = "cc1_covid_female_0_to_50"
  )
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_covid_female_51_to_150"))
  cdm <- getCovidStrata(
    cdm, codes, sex = "Female", ageRange = list(c(51, 150)), name = "cc1_covid_female_51_to_150"
  )
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_covid_male"))
  cdm <- getCovidStrata(
    cdm, codes, sex = "Male", ageRange = NULL, name = "cc1_covid_male"
  )
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_covid_male_0_to_50"))
  cdm <- getCovidStrata(
    cdm, codes, sex = "Male", ageRange = list(c(0, 50)), name = "cc1_covid_male_0_to_50"
  )
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_covid_male_51_to_150"))
  cdm <- getCovidStrata(
    cdm, codes, sex = "Male", ageRange = list(c(51, 150)), name = "cc1_covid_male_51_to_150"
  )
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_endometriosis_procedure"))
  cdm <- getEndometriosisProcedure(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_hospitalisation"))
  cdm <- getHospitalisation(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_major_non_cardiac_surgery"))
  cdm <- getMajorNonCardiacSurgery(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_neutropenia_leukopenia"))
  cdm <- getNeutropeniaLeukopenia(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_new_fluoroquinolone"))
  cdm <- getNewFluoroquinolone(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc1_transverse_myelitis"))
  cdm <- getTransverseMyelitis(cdm, codes)
  tictoc::toc(log = TRUE)

  # CohortConstructor by concept ----
  tictoc::tic(msg = paste0("cc_set_no_strata"))
  cdm <- getCohortConstructorSet(cdm, codes)
  tictoc::toc(log = TRUE)
  tictoc::tic(msg = paste0("cc_set_strata"))
  cdm <- getCohortConstructorSet(cdm, codes)
  tictoc::toc(log = TRUE)

  # Evaluate cohorts ----

  # Use SQL indexes ----

  # Format time results ----
  results <- tic.log(format = FALSE) |> purrr::map_df(~as_tibble(.x))
}

# functions ----
getId <- function(cohort, cohort_names) {
  settings(cohort) |>
    dplyr::filter(.data$cohort_name %in% .env$cohort_names) |>
    dplyr::pull("cohort_definition_id")
}

getCohortConstructorSet <- function(cdm, codes) {

}

getAsthma <- function(cdm, codes) {
  cdm$cc1_asthma_no_copd <- conceptCohort(
    cdm = cdm,
    conceptSet = codes[c("asthma", "asthma_therapy")],
    name = "cc1_asthma_no_copd",
    exit = "event_end_date",
    overlap = "merge",
    useSourceFields = FALSE,
    subsetCohort = NULL,
    subsetCohortId = NULL
  )

  cdm$cc1_asthma_no_copd <- cdm$cc1_asthma_no_copd |>
    # age to asthma therapy
    requireAge(
      ageRange = list(c(0, 54)),
      cohortId = getId(cdm$cc1_asthma_no_copd, "asthma_therapy"),
      indexDate = "cohort_start_date"
    ) |>
    # previous asthma therapy concepts
    requireConceptIntersect(
      conceptSet = codes["asthma_therapy"],
      window = list(c(-365, -180)),
      intersections = c(1, Inf),
      cohortId = getId(cdm$cc1_asthma_no_copd, "asthma_therapy"),
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL,
      inObservation = FALSE,
      censorDate = NULL
    ) |>
    # union all entries
    unionCohorts(
      cohortId = NULL,
      gap = 0,
      cohortName = "cc1_asthma_no_copd",
      keepOriginalCohorts = FALSE
    ) |>
    # get first entry
    requireIsFirstEntry() |>
    # NO chronic_obstructive_lung_disease
    requireConceptIntersect(
      conceptSet = codes["chronic_obstructive_lung_disease"],
      window = list(c(-Inf, 0)),
      intersections = 0,
      cohortId = NULL,
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL,
      inObservation = TRUE,
      censorDate = NULL
    ) |>
    # NO long_acting_muscarinic_antagonists_lamas
    requireConceptIntersect(
      conceptSet = codes["long_acting_muscarinic_antagonists_lamas"],
      window = list(c(-Inf, 0)),
      intersections = 0,
      cohortId = NULL,
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL,
      inObservation = TRUE,
      censorDate = NULL
    )
  return(cdm)
}

getBetaBlockers <- function(cdm, codes) {
  cdm$cc1_beta_blockers_hypertension <- conceptCohort(
    cdm = cdm,
    conceptSet = codes["beta_blockers"],
    name = "cc1_beta_blockers_hypertension"
  ) |>
    collapseCohorts(gap = 90) |>
    requireIsFirstEntry() |>
    requirePriorObservation(minPriorObservation = 365) |>
    requireConceptIntersect(
      conceptSet = codes["essential_hypertension"],
      window = list(c(-Inf, 0)),
      intersections = c(1, Inf),
      cohortId = NULL,
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL
    )
  cdm$cc1_beta_blockers_hypertension <- cdm$cc1_beta_blockers_hypertension |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm$cc1_beta_blockers_hypertension) |>
        dplyr::mutate("cohort_name" = "cc1_beta_blockers_hypertension"),
      .softValidation = TRUE
    )
  return(cdm)
}

getCovid <- function(cdm, codes, name = "cc1_covid") {
  cdm[[name]] <- conceptCohort(
    cdm = cdm,
    conceptSet = codes[c("covid_19")],
    name = name
  )
  cdm$temp_cc1_covid_test_positive <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["sars_cov_2_test"],
    name = "temp_cc1_covid_test_positive",
    valueAsConcept = c(4126681, 45877985, 9191, 45884084, 4181412, 45879438)
  )
  cdm$temp_cc1_covid_test_negative <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["sars_cov_2_test"],
    name = "temp_cc1_covid_test_negative",
    valueAsConcept = c(9189, 9190, 9191, 4132135, 3661867, 45878583, 45880296, 45884086)
  )
  cdm[[name]] <- cdm[[name]] |>
    requireCohortIntersect(
      targetCohortTable = "temp_cc1_covid_test_negative",
      targetCohortId = 1,
      targetEndDate = NULL,
      window = list(c(-3,3)),
      intersections = 0
    )
  cdm <- omopgenerics::bind(cdm[[name]], cdm$temp_cc1_covid_test_positive, name = name)
  cdm[[name]] <-  cdm[[name]] |>
    CohortConstructor::unionCohorts(cohortName = name) |>
    requireInDateRange(dateRange = c(as.Date("2019-12-02"), NA))
  return(cdm)
}

getCovidStrata <- function(cdm, codes, sex, ageRange, name) {
  cdm <- getCovid(cdm, codes, name = name)
  cdm[[name]] <- cdm[[name]] |>
    requireDemographics(
      ageRange = ageRange,
      sex = sex
    )
  return(cdm)
}

getEndometriosisProcedure <- function(cdm, codes) {
  cdm$cc1_endometriosis_procedure <- conceptCohort(
    cdm = cdm,
    conceptSet = codes["endometriosis_related_laproscopic_procedures"],
    name = "cc1_endometriosis_procedure"
  ) |>
    requireConceptIntersect(
      conceptSet = codes["endometriosis"],
      window = list(c(-30, 30)),
      intersections = c(1, Inf),
      cohortId = NULL,
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL,
      inObservation = TRUE
    ) |>
    requireConceptIntersect(
      conceptSet = codes["endometriosis"],
      window = list(c(0, Inf)),
      intersections = c(2, Inf),
      cohortId = NULL,
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL,
      inObservation = TRUE
    ) |>
    requireDemographics(
      ageRange = list(c(15, 49)),
      sex = "Female"
    ) |>
    exitAtObservationEnd()
  cdm$cc1_endometriosis_procedure <- cdm$cc1_endometriosis_procedure |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm$cc1_endometriosis_procedure) |>
        dplyr::mutate("cohort_name" = "cc1_endometriosis_procedure"),
      .softValidation = TRUE
    )
  return(cdm)
}

getHospitalisation <- function(cdm, codes) {
  cdm$cc1_hospitalisation <- conceptCohort(
    cdm = cdm,
    conceptSet = codes["inpatient_visit"],
    name = "cc1_hospitalisation"
  ) |>
    padCohortEnd(days = 1)
  cdm$cc1_hospitalisation <- cdm$cc1_hospitalisation |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm$cc1_hospitalisation) |>
        dplyr::mutate("cohort_name" = "cc1_hospitalisation"),
      .softValidation = TRUE
    )
  return(cdm)
}

getMajorNonCardiacSurgery <- function(cdm, codes) {
  codesMNCS <- codes[grepl("mncs_", names(codes))] |> unlist(use.names = FALSE)
  codesMNCS <- list("cc1_major_non_cardiac_surgery" = codesMNCS)
  cdm$cc1_major_non_cardiac_surgery <- conceptCohort(
    cdm = cdm,
    conceptSet = codesMNCS,
    name = "cc1_major_non_cardiac_surgery"
  ) |>
    exitAtObservationEnd() |>
    requireAge(ageRange = list(c(18, 150)))
  return(cdm)
}

getNeutropeniaLeukopenia <- function(cdm, codes) {
  # entry
  cdm$cc1_neutropenia_leukopenia <- conceptCohort(
    cdm = cdm,
    conceptSet = codes["neutropenia_agranulocytosis_or_unspecified_leukopenia"],
    name = "cc1_neutropenia_leukopenia"
  )
  cdm$temp_cc1_neutrophil_absolute_count <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["neutrophil_absolute_count"],
    name = "temp_cc1_neutrophil_absolute_count",
    valueAsNumber = list(
      "9444" = c(0.01, 1.499), "8848" = c(0.01, 1.499), "8816" = c(0.01, 1.499),
      "8961" = c(0.01, 1.499), "44777588" = c(0.01, 1.499),
      "8784" = c(10, 1499), "8647" = c(10, 1499)
    )
  )
  cdm$temp_cc1_normal_neutrophil <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["neutrophil_absolute_count"],
    name = "temp_cc1_normal_neutrophil",
    valueAsNumber = list(
      "9444" = c(4, 8.25), "8848" = c(4, 8.25), "8816" = c(4, 8.25),
      "8961" = c(4, 8.25), "44777588" = c(4, 8.25),
      "8784" = c(4000, 8250), "8647" = c(4000, 8250)
    )
  )
  cdm <- omopgenerics::bind(
    cdm$cc1_neutropenia_leukopenia,
    cdm$temp_cc1_neutrophil_absolute_count,
    name = "cc1_neutropenia_leukopenia"
  )
  cdm$cc1_neutropenia_leukopenia <- cdm$cc1_neutropenia_leukopenia |>
    CohortConstructor::unionCohorts(cohortName = "cc1_neutropenia_leukopenia") |>
    # exclusion
    requireConceptIntersect(
      conceptSet = codes["congenital_or_genetic_neutropenia_leukopenia_or_agranulocytosis"],
      window = list(c(-Inf, 7)),
      intersections = 0,
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL,
      inObservation = FALSE
    ) |>
    requireConceptIntersect(
      conceptSet = codes["neutrophilia"],
      window = list(c(0, 0)),
      intersections = 0,
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL,
      inObservation = FALSE
    ) |>
    # TODO "No Normal Neutrophil count on index date" (requireMeasurementInteresct!!)
    PatientProfiles::addCohortIntersectDate(
      targetCohortTable = "temp_cc1_normal_neutrophil",
      window = list(c(0,Inf)),
      nameStyle = "normal_count_date",
      name = "cc1_neutropenia_leukopenia"
    ) |>
    exitAtFirstDate(dateColumns = c("cohort_end_date", "normal_count_date"), returnReason = FALSE)
  return(cdm)
}

getNewFluoroquinolone <- function(cdm, codes) {
  cdm$cc1_new_fluoroquinolone <- conceptCohort(
    cdm = cdm,
    conceptSet = codes["fluoroquinolone_systemic"],
    name = "cc1_new_fluoroquinolone"
  ) |>
    collapseCohorts(gap = 30) |>
    requireIsFirstEntry() |>
    requirePriorObservation(minPriorObservation = 365)
  cdm$cc1_new_fluoroquinolone <- cdm$cc1_new_fluoroquinolone |>
    omopgenerics::newCohortTable(
      cohortSetRef = omopgenerics::settings(cdm$cc1_new_fluoroquinolone) |>
        dplyr::mutate("cohort_name" = "cc1_new_fluoroquinolone"),
      .softValidation = TRUE
    )
  return(cdm)
}

getTransverseMyelitis <- function(cdm, codes) {
  cdm$cc1_transverse_myelitis <- conceptCohort(
    cdm = cdm,
    conceptSet = codes[c("transverse_myelitis", "symptoms_for_transverse_myelitis")],
    exit = "event_start_date",
    name = "cc1_transverse_myelitis"
  )
  cdm$cc1_transverse_myelitis <- cdm$cc1_transverse_myelitis |>
    requireCohortIntersect(
      targetCohortTable = "cc1_transverse_myelitis",
      window = list(c(0, 30)),
      intersections = c(1, Inf),
      cohortId = getId(cdm$cc1_transverse_myelitis, "symptoms_for_transverse_myelitis"),
      targetCohortId = getId(cdm$cc1_transverse_myelitis, "transverse_myelitis"),
      indexDate = "cohort_start_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    ) |>
    requireConceptIntersect(
      conceptSet = codes["transverse_myelitis"],
      window = list(c(-365, -1)),
      intersections = 0,
      cohortId = NULL,
      indexDate = "cohort_start_date",
      targetStartDate = "event_start_date",
      targetEndDate = NULL,
      inObservation = FALSE,
    )
  cdm$cc1_transverse_myelitis <- cdm$cc1_transverse_myelitis |>
    # THIS SHOULD INCLUDE EVENTS OUTSIDE OBSERVATION - NOT POSSIBLE ATM (29/01/2025)
    requireCohortIntersect(
      targetCohortTable = "cc1_transverse_myelitis",
      window = list(c(-365, -1)),
      intersections = 0,
      cohortId = NULL,
      targetCohortId = getId(cdm$cc1_transverse_myelitis, "symptoms_for_transverse_myelitis"),
      indexDate = "cohort_start_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    ) |>
    CohortConstructor::unionCohorts(cohortName = "cc1_transverse_myelitis") |>
    padCohortEnd(days = 1)
  return(cdm)
}

getCohortConstructorSet <- function(cdm, codes) {
  # base cohorts
  codesMNCS <- codes[grepl("mncs_", names(codes))] |> unlist(use.names = FALSE)
  base_end <- c(
    list("cc_major_non_cardiac_surgery" = codesMNCS),
    codes[c(
      "asthma", "asthma_therapy", "beta_blockers", "covid_19",
      "endometriosis_related_laproscopic_procedures", "inpatient_visit",
      "neutropenia_agranulocytosis_or_unspecified_leukopenia",
      "fluoroquinolone_systemic"
    )]
  )
  base_start <- codes[c("transverse_myelitis", "symptoms_for_transverse_myelitis")]
  cdm$base_end <- conceptCohort(cdm = cdm, conceptSet = base_end, name = "base_end")
  cdm$base_start <- conceptCohort(cdm = cdm, conceptSet = base_start, name = "base_start")

  # measurement cohorts
  cdm$temp_cc1_covid_test_positive <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["sars_cov_2_test"],
    name = "temp_cc1_covid_test_positive",
    valueAsConcept = c(4126681, 45877985, 9191, 45884084, 4181412, 45879438)
  )
  cdm$temp_cc1_covid_test_negative <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["sars_cov_2_test"],
    name = "temp_cc1_covid_test_negative",
    valueAsConcept = c(9189, 9190, 9191, 4132135, 3661867, 45878583, 45880296, 45884086)
  )
  cdm$temp_cc1_neutrophil_absolute_count <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["neutrophil_absolute_count"],
    name = "temp_cc1_neutrophil_absolute_count",
    valueAsNumber = list(
      "9444" = c(0.01, 1.499), "8848" = c(0.01, 1.499), "8816" = c(0.01, 1.499),
      "8961" = c(0.01, 1.499), "44777588" = c(0.01, 1.499),
      "8784" = c(10, 1499), "8647" = c(10, 1499)
    )
  )
  cdm$temp_cc1_normal_neutrophil <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["neutrophil_absolute_count"],
    name = "temp_cc1_normal_neutrophil",
    valueAsNumber = list(
      "9444" = c(4, 8.25), "8848" = c(4, 8.25), "8816" = c(4, 8.25),
      "8961" = c(4, 8.25), "44777588" = c(4, 8.25),
      "8784" = c(4000, 8250), "8647" = c(4000, 8250)
    )
  )

  # cc_asthma_no_copd

  # cc_beta_blockers_hypertension

  # cc_covid

  # cc_endometriosis_procedure

  # cc_hospitalisation

  # cc_major_non_cardiac_surgery

  # cc_neutropenia_leukopenia

  # cc_new_fluoroquinolone

  # cc_transverse_myelitis
}
