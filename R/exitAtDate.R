#' Set cohort end date to end of observation
#'
#' @description
#' `exitAtObservationEnd()` resets cohort end date based on a set of specified
#' column dates. The last date that occurs is chosen.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
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
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |> exitAtObservationEnd()
#'}
exitAtObservationEnd <- function(cohort,
                                 cohortId = NULL,
                                 name = tableName(cohort)) {
  # checks
  name <- validateName(name)
  cohort <- validateCohortTable(cohort, dropExtraColumns = TRUE)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)

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
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(
      reason = "Exit at observation period end date",
      cohortId = cohortId
    )

  return(newCohort)
}


#' Set cohort end date to death date
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param requireDeath If TRUE, subjects without a death record will be dropped,
#' while if FALSE their end date will be left as is.
#' @param name Name of the new cohort with the restriction.
#'
#' @return The cohort table.
#'
#' @description
#' This functions changes cohort end date to subject's death date. In the case
#' were this generates overlapping records in the cohort, those overlapping
#' entries will be merged.
#'
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 |> exitAtDeath()
#' }
exitAtDeath <- function(cohort,
                        cohortId = NULL,
                        requireDeath = FALSE,
                        name = tableName(cohort)) {
  # checks
  name <- validateName(name)
  cohort <- validateCohortTable(cohort, dropExtraColumns = TRUE)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertLogical(requireDeath, length = 1)


  # create new cohort
  newCohort <- cohort |>
    PatientProfiles::addDeathDate() |>
    # exit
    dplyr::mutate(
      "cohort_end_date" = dplyr::if_else(
        .data$cohort_definition_id %in% .env$cohortId & !is.na(.data$date_of_death),
        .data$date_of_death,
        .data$cohort_end_date
      )
    ) %>%
    {if (requireDeath) {
      dplyr::filter(
        .,
        !is.na(.data$date_of_death) | !.data$cohort_definition_id %in% .env$cohortId
      ) |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::recordCohortAttrition(
          reason = "No death recorded",
          cohortId = cohortId
        )
    } else . } |>
    # no overlapping periods
    joinOverlap() |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(
      reason = "Exit at death",
      cohortId = cohortId
    )

  return(newCohort)
}
