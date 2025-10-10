
# check if we will need to filter based on cohort ID
needsIdFilter <- function(cohort, cohortId){
  !identical(omopgenerics::settings(cohort)$cohort_definition_id, cohortId)
}

filterCohortInternal <- function(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged) {
  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (isFALSE(needsIdFilter(cohort = cohort, cohortId = cohortId))){
    cdm[[tmpNewCohort]] <- cohort |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_utilities_newCohort1_") |>
      omopgenerics::newCohortTable(.softValidation = TRUE)
  } else {
    cdm[[tmpUnchanged]] <- cohort |>
      dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpUnchanged, temporary = FALSE,
                     logPrefix = "CohortConstructor_utilities_unchangedCohort_")
    cdm[[tmpNewCohort]] <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_utilities_newCohort2_")
    if (!isFALSE(useIndexes)) {
      addIndex(
        cohort = cdm[[tmpUnchanged]],
        cols = c("subject_id", "cohort_start_date")
      )
    }
  }
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[tmpNewCohort]],
      cols = c("subject_id", "cohort_start_date")
    )
  }
  return(cdm)
}

uniqueColumnName <- function(table, n = 1) {
  cols <- colnames(table)
  newCols <- paste0("cohortconstructor_", 1:n)
  ids <- newCols %in% cols
  while (any(ids)) {
    newCols <- paste0(newCols, round(stats::runif(1, max = 10)))
    ids <- newCols %in% cols
  }
  return(newCols)
}

getObservationPeriodId <- function(x, name) {
  cdm <- omopgenerics::cdmReference(x)
  currentObsId <- x |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          "subject_id" ="person_id",
          "observation_period_start_date",
          "observation_period_end_date",
          "observation_period_id"
        ),
      by = "subject_id"
    ) |>
    dplyr::filter(
      .data[["cohort_start_date"]] <= .data[["observation_period_end_date"]] &
        .data[["cohort_start_date"]] >= .data[["observation_period_start_date"]]
    ) |>
    dplyr::select(dplyr::all_of(c(
     "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "observation_period_id"
    ))) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_utilities_observationPeriodId1_")
}
