#' Set cohort end date to end of observation
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param name Name of the new cohort with the restriction.
#'
#' @return The cohort table.
#'
#' @description
#' This functions changes cohort end date to the end date of the observation
#' period corresponding to the cohort entry. In the case were this generates
#' overlapping records in the cohort, overlapping entries will be merged.
#'
#'
#' @export
#'
#' @examples
#' library(PatientProfiles)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   exitAtObservationEnd()

exitAtObservationEnd <- function(cohort,
                                 cohortId = NULL,
                                 name = omopgenerics::tableName(cohort)) {
  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)

  # warning
  extraColumns <- colnames(cohort)
  extraColumns <- extraColumns[!extraColumns %in% c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  )]
  if (length(extraColumns) > 0) {
    cli::cli_inform(c(
      "!" = "Extra columns are not supported in this function, the following
      columns will be dropped: {paste0(extraColumns, collapse = ', ')}"
    ))
  }

  # create new cohort
  newCohort <- cohort |>
    PatientProfiles::addFutureObservation(
      futureObservationType = "date"
    ) |>
    # exit at observation end
    dplyr::mutate(
      "cohort_end_date" = dplyr::if_else(
        .data$cohort_definition_id %in% .env$cohortId,
        .data$future_observation,
        .data$cohort_end_date
      )
    ) |>
    # no overlapping periods
    joinOverlap() |>
    dplyr::compute(name = name, temporary = FALSE)
  omopgenerics::newCohortTable()

  return(newCohort)
}
