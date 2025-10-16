#' Run benchmark of CohortConstructor package
#'
#' @description
#' Run benchmark of CohortConstructor cohort instantiation time compared to
#' CIRCE from JSON. More information in the benchmarking vignette.
#'
#' @inheritParams cdmDoc
#' @param runCIRCE Whether to run cohorts from JSON definitions generated with
#' Atlas.
#' @param runCohortConstructorDefinition Whether to run the benchmark part where
#' cohorts are created with CohortConstructor by definition (one by one,
#' separately).
#' @param runCohortConstructorDomain Whether to run the benchmark part where
#' cohorts are created with CohortConstructor by domain (instantianting base
#' cohort all together, as a set).
#' @param dropCohorts Whether to drop cohorts created during benchmark.
#'
#' @export
#'
benchmarkCohortConstructor <- function(cdm,
                                       runCIRCE = TRUE,
                                       runCohortConstructorDefinition = TRUE,
                                       runCohortConstructorDomain = TRUE,
                                       dropCohorts = TRUE) {
  # Package checks
  rlang::check_installed("tictoc")
  rlang::check_installed("CohortCharacteristics")
  rlang::check_installed("CDMConnector")
  if(isTRUE(runCIRCE)){
  rlang::check_installed("CirceR")
  rlang::check_installed("SqlRender")
  }

  # check inputs
  cdm <- omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertLogical(runCIRCE)
  omopgenerics::assertLogical(runCohortConstructorDefinition)
  omopgenerics::assertLogical(runCohortConstructorDomain)
  omopgenerics::assertLogical(dropCohorts)
  tictoc::tic.clearlog()

  # check local
  if (runCIRCE & "local_cdm" %in% class(omopgenerics::cdmSource(cdm))) {
    runCIRCE <- FALSE
    cli::cli_warn(c("!" = "ATLAS cohort instantiation is not supported for local_cdm, `runCIRCE` has been switch to {.pkg FALSE}."))
  }

  # prefix for cohorts created - to drop later
  pref <- paste0("bench", paste0(sample.int(9,3), collapse = ""))

  # Instantiate or read Atlas ----
  if (runCIRCE) {
    cli::cli_inform(c("*" = "{.strong Instantiating JSON cohort defintions with CDMConnector}"))
    rlang::check_installed("CirceR")
    rlang::check_installed("SqlRender")
    for (json in jsons$cohort_name_snakecase) {
      tictoc::tic(msg = paste0("atlas_", json))
      cdm <- CDMConnector::generateCohortSet(
        cdm = cdm,
        cohortSet = jsons |> dplyr::filter(.data$cohort_name_snakecase == .env$json) |> dplyr::mutate("cohort_name" = paste0("atlas_", .data$cohort_name)),
        name = paste0(pref, "atlas_", json),
        computeAttrition = TRUE,
        overwrite = TRUE
      )
      tictoc::toc(log = TRUE)
    }
  }

  # CohortConstructor by definition ----
  if (runCohortConstructorDefinition) {
    cli::cli_inform(c(""))
    cli::cli_inform(c("*" = "{.strong Instantiating cohorts with CohortConstructor - by cohort definition}"))
    tictoc::tic(msg = paste0("cc_asthma_no_copd"))
    cdm <- getAsthma(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_beta_blockers_hypertension"))
    cdm <- getBetaBlockers(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_covid"))
    cdm <- getCovid(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_covid_female"))
    cdm <- getCovidStrata(
      cdm, codes, sex = "Female", ageRange = list(c(0, 150)), pref = pref, name = "cc1_covid_female"
    )
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_covid_female_0_to_50"))
    cdm <- getCovidStrata(
      cdm, codes, sex = "Female", ageRange = list(c(0, 50)), pref = pref, name = "cc1_covid_female_0_to_50"
    )
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_covid_female_51_to_150"))
    cdm <- getCovidStrata(
      cdm, codes, sex = "Female", ageRange = list(c(51, 150)), pref = pref, name = "cc1_covid_female_51_to_150"
    )
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_covid_male"))
    cdm <- getCovidStrata(
      cdm, codes, sex = "Male", ageRange = list(c(0, 150)), pref = pref, name = "cc1_covid_male"
    )
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_covid_male_0_to_50"))
    cdm <- getCovidStrata(
      cdm, codes, sex = "Male", ageRange = list(c(0, 50)), pref = pref, name = "cc1_covid_male_0_to_50"
    )
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_covid_male_51_to_150"))
    cdm <- getCovidStrata(
      cdm, codes, sex = "Male", ageRange = list(c(51, 150)), pref = pref, name = "cc1_covid_male_51_to_150"
    )
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_endometriosis_procedure"))
    cdm <- getEndometriosisProcedure(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_hospitalisation"))
    cdm <- getHospitalisation(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_major_non_cardiac_surgery"))
    cdm <- getMajorNonCardiacSurgery(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_neutropenia_leukopenia"))
    cdm <- getNeutropeniaLeukopenia(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_new_fluoroquinolone"))
    cdm <- getNewFluoroquinolone(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_transverse_myelitis"))
    cdm <- getTransverseMyelitis(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
  }

  # CohortConstructor by concept ----
  if (runCohortConstructorDomain) {
    cli::cli_inform(c(""))
    cli::cli_inform(c("*" = "{.strong Instantiating cohorts with CohortConstructor - by domain}"))
    tictoc::tic(msg = paste0("cc_set_no_strata"))
    cdm <- getCohortConstructorSet(cdm, codes, pref = pref)
    tictoc::toc(log = TRUE)
    tictoc::tic(msg = paste0("cc_set_strata"))
    cdm <- getCohortConstructorSetStrata(cdm, pref)
    tictoc::toc(log = TRUE)
  }

  # Summarise cohorts and CDM ----
  cli::cli_inform(c(""))
  cli::cli_inform(c("*" = "{.strong Summarising cohorts}"))

  if (!runCIRCE & !runCohortConstructorDefinition & !runCohortConstructorDomain) {
    cli::cli_warn(c(">" = "No cohorts created - skipping cohorts summary"))
  } else {
    eliminate <- c("cc1_", "cc_")[c(!runCohortConstructorDefinition, !runCohortConstructorDomain)]
    if (length(eliminate) == 0) eliminate <- "cc1_"
    cdm <- bindCohorts(cdm, cohortNames = jsons$cohort_name, pref = pref, eliminate = paste0(eliminate, collapse = "|"))
    resultsCohortCount <- summary(cdm$benchmark)
    resultsOmopCount <- getOmopCounts(cdm)
  }

  if (runCIRCE & (runCohortConstructorDefinition | runCohortConstructorDomain)) {
    resultsOverlap <- CohortCharacteristics::summariseCohortOverlap(cdm$benchmark) |>
      omopgenerics::splitGroup() |>
      dplyr::filter(grepl("cc_", .data$cohort_name_comparator) & grepl("atlas_", .data$cohort_name_reference)) |>
      dplyr::filter(gsub("cc_", "", .data$cohort_name_comparator) == gsub("atlas_", "", .data$cohort_name_reference)) |>
      omopgenerics::uniteGroup(c("cohort_name_reference", "cohort_name_comparator"))
    resultsTiming <- CohortCharacteristics::summariseCohortTiming(cdm$benchmark) |>
      omopgenerics::splitGroup() |>
      dplyr::filter(grepl("cc_", .data$cohort_name_comparator) & grepl("atlas_", .data$cohort_name_reference)) |>
      dplyr::filter(gsub("cc_", "", .data$cohort_name_comparator) == gsub("atlas_", "", .data$cohort_name_reference)) |>
      omopgenerics::uniteGroup(c("cohort_name_reference", "cohort_name_comparator"))
  } else {
    cli::cli_warn(c(">" = "No cohorts created with both CIRCE and CohortConstructor - skipping cohort overlap and timing"))
    resultsOverlap <- NULL
    resultsTiming <- NULL
  }

  # Format time results ----
  resultsTime <- getTimes(tictoc::tic.log(format = FALSE), cdm)

  # Drop tables ----
  cli::cli_inform(c(""))
  cli::cli_inform(c("*" = "{.strong Dropping intrmediate benchmark intermediate tables.}"))
  omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(pref))
  if (dropCohorts) {
    cli::cli_inform(c("*" = "{.strong Dropping benchmark cohort}"))
    omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with("benchmark"))
  }

  # Bind and return results ---
  cli::cli_inform(c("*" = "{.strong Benchmarking finished}"))
  return(omopgenerics::bind(list(
    resultsOverlap, resultsTiming, resultsCohortCount, resultsOmopCount, resultsTime
  )))
}

# functions ----
getId <- function(cohort, cohort_names) {
  settings(cohort) |>
    dplyr::filter(.data$cohort_name %in% .env$cohort_names) |>
    dplyr::pull("cohort_definition_id")
}

getAsthma <- function(cdm, codes, pref, name = NULL) {
  if (is.null(name)) {
    cdm[[paste0(pref, "cc1_asthma_no_copd")]] <- conceptCohort(
      cdm = cdm,
      conceptSet = codes[c("asthma", "asthma_therapy")],
      name = paste0(pref, "cc1_asthma_no_copd"),
      exit = "event_end_date",
      overlap = "merge",
      useSourceFields = FALSE,
      subsetCohort = NULL,
      subsetCohortId = NULL
    )
    name <- "cc1_asthma_no_copd"
  }

  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
    # age to asthma therapy
    requireAge(
      ageRange = list(c(0, 54)),
      cohortId = getId(cdm[[paste0(pref, name)]], "asthma_therapy"),
      indexDate = "cohort_start_date"
    ) |>
    # previous asthma therapy concepts
    requireConceptIntersect(
      conceptSet = codes["asthma_therapy"],
      window = list(c(-365, -180)),
      intersections = c(1, Inf),
      cohortId = getId(cdm[[paste0(pref, name)]], "asthma_therapy"),
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
      cohortName = name,
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

getBetaBlockers <- function(cdm, codes, pref, name = NULL) {
  if (is.null(name)) {
    cdm[[paste0(pref, "cc1_bb_hyp")]] <- conceptCohort(
      cdm = cdm,
      conceptSet = codes["beta_blockers"],
      name = paste0(pref, "cc1_bb_hyp")
    )
    name <- "cc1_bb_hyp"
  }
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
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
    ) |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[paste0(pref, name)]] ) |>
        dplyr::mutate("cohort_name" = name),
      .softValidation = TRUE
    )
  return(cdm)
}

getCovid <- function(cdm, codes, pref, name = "cc1_covid", base = FALSE) {
  if (!base) {
    cdm[[paste0(pref, name)]] <- conceptCohort(
      cdm = cdm,
      conceptSet = codes[c("covid_19")],
      name = paste0(pref, name)
    )
  }
  cdm[[paste0(pref, "temp_cc1_covid_tp")]] <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["sars_cov_2_test"],
    name = paste0(pref, "temp_cc1_covid_tp"),
    valueAsConcept = list("sars_cov_2_test" = c(4126681, 45877985, 9191, 45884084, 4181412, 45879438))
  )
  cdm[[paste0(pref, "temp_cc1_covid_tn")]] <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["sars_cov_2_test"],
    name = paste0(pref, "temp_cc1_covid_tn"),
    valueAsConcept = list("sars_cov_2_test" = c(9189, 9190, 9191, 4132135, 3661867, 45878583, 45880296, 45884086))
  )
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
    requireCohortIntersect(
      targetCohortTable = paste0(pref, "temp_cc1_covid_tn"),
      targetCohortId = 1,
      targetEndDate = NULL,
      window = list(c(-3,3)),
      intersections = 0
    )
  cdm <- omopgenerics::bind(
    cdm[[paste0(pref, name)]],
    cdm[[paste0(pref, "temp_cc1_covid_tp")]],
    name = paste0(pref, name)
  )
  cdm[[paste0(pref, name)]] <-  cdm[[paste0(pref, name)]] |>
    CohortConstructor::unionCohorts(cohortName = name) |>
    requireInDateRange(dateRange = c(as.Date("2019-12-02"), NA))
  return(cdm)
}

getCovidStrata <- function(cdm, codes, sex, ageRange, name, pref) {
  cdm <- getCovid(cdm, codes, pref, name = name)
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
    requireDemographics(
      ageRange = ageRange,
      sex = sex
    )
  return(cdm)
}

getEndometriosisProcedure <- function(cdm, codes, pref, name = NULL) {
  if (is.null(name)) {
    cdm[[paste0(pref, "cc1_endo_proc")]] <- conceptCohort(
      cdm = cdm,
      conceptSet = codes["endometriosis_related_laproscopic_procedures"],
      name = paste0(pref, "cc1_endo_proc")
    )
    name <- "cc1_endo_proc"
  }
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
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
    exitAtObservationEnd() |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[paste0(pref, name)]]) |> dplyr::mutate("cohort_name" = name),
      .softValidation = TRUE
    )
  return(cdm)
}

getHospitalisation <- function(cdm, codes, pref, name = NULL) {
  if (is.null(name)) {
    cdm[[paste0(pref, "cc1_hospitalisation")]] <- conceptCohort(
      cdm = cdm,
      conceptSet = codes["inpatient_visit"],
      name = paste0(pref, "cc1_hospitalisation")
    )
    name <- "cc1_hospitalisation"
  }
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
    padCohortEnd(days = 1) |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[paste0(pref, name)]]) |> dplyr::mutate("cohort_name" = name),
      .softValidation = TRUE
    )
  return(cdm)
}

getMajorNonCardiacSurgery <- function(cdm, codes, pref) {
  codesMNCS <- codes[grepl("mncs_", names(codes))] |> unlist(use.names = FALSE)
  codesMNCS <- list("cc1_major_nc_surgery" = codesMNCS)
  cdm[[paste0(pref, "cc1_major_nc_surgery")]] <- conceptCohort(
    cdm = cdm,
    conceptSet = codesMNCS,
    name = paste0(pref, "cc1_major_nc_surgery")
  ) |>
    exitAtObservationEnd() |>
    requireAge(ageRange = list(c(18, 150)))
  return(cdm)
}

getNeutropeniaLeukopenia <- function(cdm, codes, pref, name = NULL) {
  # entry
  if (is.null(name)) {
    cdm[[paste0(pref, "cc1_neut_leukopenia")]] <- conceptCohort(
      cdm = cdm,
      conceptSet = codes["neutropenia_agranulocytosis_or_unspecified_leukopenia"],
      name = paste0(pref, "cc1_neut_leukopenia")
    )
    name <- "cc1_neut_leukopenia"
  }
  cdm[[paste0(pref, "temp_cc1_neutrophil_absolute_count")]] <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["neutrophil_absolute_count"],
    name = paste0(pref, "temp_cc1_neutrophil_absolute_count"),
    valueAsNumber = list("neutrophil_absolute_count" = list(
      "9444" = c(0.01, 1.499), "8848" = c(0.01, 1.499), "8816" = c(0.01, 1.499),
      "8961" = c(0.01, 1.499), "44777588" = c(0.01, 1.499),
      "8784" = c(10, 1499), "8647" = c(10, 1499)
    ))
  )
  cdm[[paste0(pref, "temp_cc1_normal_neutrophil")]] <- measurementCohort(
    cdm = cdm,
    conceptSet = codes["neutrophil_absolute_count"],
    name = paste0(pref, "temp_cc1_normal_neutrophil"),
    valueAsNumber = list("neutrophil_absolute_count" = list(
      "9444" = c(4, 8.25), "8848" = c(4, 8.25), "8816" = c(4, 8.25),
      "8961" = c(4, 8.25), "44777588" = c(4, 8.25),
      "8784" = c(4000, 8250), "8647" = c(4000, 8250)
    ))
  )
  cdm <- omopgenerics::bind(
    cdm[[paste0(pref, name)]],
    cdm[[paste0(pref, "temp_cc1_neutrophil_absolute_count")]],
    name = paste0(pref, name)
  )
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
    CohortConstructor::unionCohorts(cohortName = name) |>
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
      targetCohortTable = paste0(pref, "temp_cc1_normal_neutrophil"),
      window = list(c(0,Inf)),
      nameStyle = "normal_count_date",
      name = paste0(pref, name)
    ) |>
    exitAtFirstDate(dateColumns = c("cohort_end_date", "normal_count_date"), returnReason = FALSE)
  return(cdm)
}

getNewFluoroquinolone <- function(cdm, codes, pref, name = NULL) {
  if (is.null(name)) {
    name <- "cc1_new_fluoro"
    cdm[[paste0(pref, name)]] <- conceptCohort(
      cdm = cdm,
      conceptSet = codes["fluoroquinolone_systemic"],
      name = paste0(pref, name)
    )
  }
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
    collapseCohorts(gap = 30) |>
    requireIsFirstEntry() |>
    requirePriorObservation(minPriorObservation = 365) |>
    omopgenerics::newCohortTable(
      cohortSetRef = omopgenerics::settings(cdm[[paste0(pref, name)]]) |> dplyr::mutate("cohort_name" = name),
      .softValidation = TRUE
    )
  return(cdm)
}

getTransverseMyelitis <- function(cdm, codes, pref, name = NULL) {
  if (is.null(name)) {
    name <- "cc1_transverse_myelitis"
    cdm[[paste0(pref, name)]] <- conceptCohort(
      cdm = cdm,
      conceptSet = codes[c("transverse_myelitis", "symptoms_for_transverse_myelitis")],
      exit = "event_start_date",
      name = paste0(pref, name)
    )
  }
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
    requireCohortIntersect(
      targetCohortTable = paste0(pref, name),
      window = list(c(0, 30)),
      intersections = c(1, Inf),
      cohortId = getId(cdm[[paste0(pref, name)]], "symptoms_for_transverse_myelitis"),
      targetCohortId = getId(cdm[[paste0(pref, name)]], "transverse_myelitis"),
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
  cdm[[paste0(pref, name)]] <- cdm[[paste0(pref, name)]] |>
    requireCohortIntersect(
      targetCohortTable = paste0(pref, name),
      window = list(c(-365, -1)),
      intersections = 0,
      cohortId = NULL,
      targetCohortId = getId(cdm[[paste0(pref, name)]], "symptoms_for_transverse_myelitis"),
      indexDate = "cohort_start_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    ) |>
    CohortConstructor::unionCohorts(cohortName = name) |>
    padCohortEnd(days = 1)
  return(cdm)
}

getCohortConstructorSet <- function(cdm, codes, pref) {
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
  cdm[[paste0(pref, "base_end")]] <- conceptCohort(cdm = cdm, conceptSet = base_end, name = paste0(pref, "base_end"))

  # cc_asthma_no_copd
  cdm[[paste0(pref, "cc_asthma_no_copd")]] <- subsetCohorts(
    cohort = cdm[[paste0(pref, "base_end")]],
    cohortId = getId(cdm[[paste0(pref, "base_end")]], c("asthma", "asthma_therapy")),
    name = paste0(pref, "cc_asthma_no_copd")
  )
  cdm <- getAsthma(cdm, codes, pref = pref, name = "cc_asthma_no_copd")

  # cc_beta_blockers_hypertension
  cdm[[paste0(pref, "cc_beta_blockers_hypertension")]] <- subsetCohorts(
    cohort = cdm[[paste0(pref, "base_end")]],
    cohortId = getId(cdm[[paste0(pref, "base_end")]], "beta_blockers"),
    name = paste0(pref, "cc_beta_blockers_hypertension")
  )
  cdm <- getBetaBlockers(cdm, codes, pref = pref, name = "cc_beta_blockers_hypertension")

  # cc_covid
  cdm[[paste0(pref, "cc_covid")]] <- subsetCohorts(
    cohort = cdm[[paste0(pref, "base_end")]],
    cohortId = getId(cdm[[paste0(pref, "base_end")]], "covid_19"),
    name = paste0(pref, "cc_covid")
  )
  cdm <- getCovid(cdm, codes, name = "cc_covid", pref = pref, base = TRUE)

  # cc_endometriosis_procedure
  cdm[[paste0(pref, "cc_endometriosis_procedure")]] <- subsetCohorts(
    cohort = cdm[[paste0(pref, "base_end")]],
    cohortId = getId(cdm[[paste0(pref, "base_end")]], "endometriosis_related_laproscopic_procedures"),
    name = paste0(pref, "cc_endometriosis_procedure")
  )
  cdm <- getEndometriosisProcedure(cdm, codes, pref = pref, name = "cc_endometriosis_procedure")

  # cc_hospitalisation
  cdm[[paste0(pref, "cc_hospitalisation")]] <- subsetCohorts(
    cohort = cdm[[paste0(pref, "base_end")]],
    cohortId = getId(cdm[[paste0(pref, "base_end")]], "inpatient_visit"),
    name = paste0(pref, "cc_hospitalisation")
  )
  cdm <- getHospitalisation(cdm, codes, pref = pref, name = "cc_hospitalisation")

  # cc_major_non_cardiac_surgery
  cdm[[paste0(pref, "cc_major_non_cardiac_surgery")]] <- subsetCohorts(
    cohort = cdm[[paste0(pref, "base_end")]],
    cohortId = getId(cdm[[paste0(pref, "base_end")]], "cc_major_non_cardiac_surgery"),
    name = paste0(pref, "cc_major_non_cardiac_surgery")
  ) |>
    exitAtObservationEnd() |>
    requireAge(ageRange = list(c(18, 150)))

  # cc_neutropenia_leukopenia
  cdm[[paste0(pref, "cc_neutropenia_leukopenia")]] <- subsetCohorts(
    cohort = cdm[[paste0(pref, "base_end")]],
    cohortId = getId(cdm[[paste0(pref, "base_end")]], "neutropenia_agranulocytosis_or_unspecified_leukopenia"),
    name = paste0(pref, "cc_neutropenia_leukopenia")
  )
  cdm <- getNeutropeniaLeukopenia(cdm, codes, pref = pref, name = "cc_neutropenia_leukopenia")

  # cc_new_fluoroquinolone
  cdm[[paste0(pref, "cc_new_fluoroquinolone")]] <- subsetCohorts(
    cohort = cdm[[paste0(pref, "base_end")]],
    cohortId = getId(cdm[[paste0(pref, "base_end")]], "fluoroquinolone_systemic"),
    name = paste0(pref, "cc_new_fluoroquinolone")
  )
  cdm <- getNewFluoroquinolone(cdm, codes, pref = pref, name = "cc_new_fluoroquinolone")

  # cc_transverse_myelitis
  cdm[[paste0(pref, "cc_transverse_myelitis")]] <- conceptCohort(
    cdm = cdm,
    conceptSet = codes[c("transverse_myelitis", "symptoms_for_transverse_myelitis")],
    name = paste0(pref, "cc_transverse_myelitis")
  )
  cdm <- getTransverseMyelitis(cdm, codes, pref = pref, name = "cc_transverse_myelitis")

  return(cdm)
}

getCohortConstructorSetStrata <- function(cdm, pref) {
  cdm[[paste0(pref, "cc_covid_strata")]] <- cdm[[paste0(pref, "cc_covid")]] |>
    PatientProfiles::addDemographics(
      ageGroup = list(c(0,50), c(51, 150)),
      priorObservation = FALSE,
      futureObservation = FALSE,
      name = paste0(pref, "cc_covid_strata")
    ) |>
    dplyr::select(!"age") |>
    stratifyCohorts(strata = list("sex", c("sex", "age_group")))
  if (cdm[[paste0(pref, "cc_covid_strata")]] |> dplyr::tally() |> dplyr::pull("n") == 0) {
    cdm[[paste0(pref, "cc_covid_strata")]] <- cdm[[paste0(pref, "cc_covid_strata")]] |>
      omopgenerics::newCohortTable(
        cohortSetRef = dplyr::tibble(
          cohort_definition_id = 1:6,
          cohort_name = c(
            "cc_covid_female", "cc_covid_male", "cc_covid_female_0_to_50",
            "cc_covid_female_51_to_150", "cc_covid_male_0_to_50", "cc_covid_male_51_to_150"
          )
        ),
        cohortAttritionRef = NULL
      )
  }
  return(cdm)
}

bindCohorts <- function(cdm, cohortNames, pref, eliminate) {
  cohortNames <- names(cdm)[grepl(paste0(cohortNames, collapse = "|"), names(cdm))]
  cohortNames <- cohortNames[!grepl(paste0(eliminate, "|temp"), cohortNames) & grepl(pref, cohortNames)]
  cdm <- eval(parse(
    text = paste0("omopgenerics::bind(", paste0("cdm$", cohortNames, collapse = ", "), ", name = 'benchmark')")
  ))
  cdm$benchmark <- cdm$benchmark |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm$benchmark) |>
        dplyr::mutate(cohort_name = gsub("cc1_", "cc_", .data$cohort_name)),
      .softValidation = TRUE
    )
  return(cdm)
}

getOmopCounts <- function(cdm) {
  tabNames <- c(
    "person", "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "observation_period", "measurement", "observation",
    "death"
  )
  tableCounts <- NULL
  for (tab in tabNames) {
    tableCounts <- tableCounts |>
      dplyr::union_all(
        dplyr::tibble(
          table_name = tab,
          estimate_value = cdm[[tab]] |> dplyr::tally() |> dplyr::pull("n") |> as.character()
        )
      )
  }
  tableCounts <- tableCounts |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      variable_name = "number_records",
      variable_level = NA_character_,
      estimate_name = "count",
      estimate_type = "numeric"
    ) |>
    omopgenerics::uniteGroup("table_name") |>
    omopgenerics::uniteStrata() |>
    omopgenerics::uniteAdditional() |>
    omopgenerics::newSummarisedResult(
      settings = dplyr::tibble(
        result_id = 1L,
        result_type = "omop_counts",
        package_name = "CohortConstructor",
        package_version = as.character(utils::packageVersion("CohortConstructor"))
      )
    )
  return(tableCounts)
}

getTimes <- function(log, cdm) {
  return(
    log |>
      purrr::map_df(~dplyr::as_tibble(.x)) |>
      dplyr::distinct() |>
      dplyr::mutate(
        estimate_value = as.character((as.numeric(.data$toc) - as.numeric(.data$tic))/60),
        tool = dplyr::if_else(grepl("cc", .data$msg), "CohortConstructor", "CIRCE"),
        variable_name = stringr::str_to_sentence(gsub("_", " ", gsub("cc_|cc1_|atlas_", "", .data$msg)))
      ) |>
      dplyr::select(-dplyr::all_of(c("tic", "toc", "msg", "callback_msg"))) |>
      dplyr::mutate(
        variable_name = dplyr::case_when(
          grepl("Asthma", .data$variable_name) ~ "Asthma without COPD",
          "Covid" == .data$variable_name ~ "COVID-19",
          grepl("male", .data$variable_name) ~ gsub("male ", "male, ", gsub("Covid", "COVID-19:", .data$variable_name)),
          grepl("eutropenia", .data$variable_name) ~ "Acquired neutropenia or unspecified leukopenia",
          grepl("Hosp", .data$variable_name) ~ "Inpatient hospitalisation",
          grepl("First", .data$variable_name) ~ "First major depression",
          grepl("fluoro", .data$variable_name) ~ "New fluoroquinolone users",
          grepl("Beta", .data$variable_name) ~ "New users of beta blockers nested in essential hypertension",
          "Set no strata" == .data$variable_name ~ "Cohort set - Overall",
          "Set strata" == .data$variable_name ~ "Cohort set - COVID-19 strata",
          .default = .data$variable_name
        )
      ) |>
      dplyr::arrange(.data$variable_name) |>
      dplyr::mutate(
        variable_level = dplyr::if_else(
          grepl("Cohort set", .data$variable_name), gsub("Cohort set - ", "", .data$variable_name), NA
        ),
        variable_name = dplyr::if_else(
          grepl("Cohort set", .data$variable_name), "Cohort set", .data$variable_name
        ),
        cdm_name = omopgenerics::cdmName(cdm),
        result_id = 1L,
        estimate_name = "time",
        estimate_type = "numeric"
      ) |>
      omopgenerics::uniteGroup("tool") |>
      omopgenerics::uniteStrata() |>
      omopgenerics::uniteAdditional() |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(
          result_id = 1L,
          result_type = "instanciation_time",
          package_name = "CohortConstructor",
          package_version = as.character(utils::packageVersion("CohortConstructor"))
        )
      )
  )
}
