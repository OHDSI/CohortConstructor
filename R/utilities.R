
# check if we will need to filter based on cohort ID
needsIdFilter <- function(cohort, cohortId){
  !identical(omopgenerics::cohortCount(cohort) |>
             dplyr::pull("cohort_definition_id"),
             cohortId)
}

filterCohortInternal <- function(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged) {
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
  }
  return(cdm)
}
