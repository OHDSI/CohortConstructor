#' Set cohort end date to end of observation
#'
#' @description
#' `exitAtObservationEnd()` resets cohort end date based on a set of specified
#' column dates. The last date that occurs is chosen.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param limitToCurrentPeriod If TRUE, limits the cohort to one entry per
#' person, ending at the current observation period. If FALSE, subsequent
#' observation periods will create new cohort entries.
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
                                 limitToCurrentPeriod = TRUE,
                                 name = tableName(cohort)) {
  # checks
  cohort <- omopgenerics::validateCohortArgument(cohort, dropExtraColumns = TRUE)
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |>
      dplyr::compute(name = name, temporary = FALSE,
                    logPrefix = "CohortConstructor_exitAtObservationEnd_entry_")
    return(cdm[[name]])
  }

  tmpTable <- omopgenerics::uniqueTableName()
  if (all(cohortId %in% settings(cohort)$cohort_definition_id)) {
    newCohort <- cohort |>
      dplyr::compute(name = tmpTable, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtObservationEnd_copy_")
  } else {
    newCohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpTable, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtObservationEnd_filter_")
  }

  newCohort <- newCohort |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          "subject_id" = "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "subject_id"
    ) |>
    # filter to current or future observation periods
    dplyr::filter(.data$observation_period_end_date >= .data$cohort_end_date) |>
    dplyr::compute(name = tmpTable, temporary = FALSE,
                   logPrefix = "CohortConstructor_exitAtObservationEnd_filterFuture_")

  if (limitToCurrentPeriod) {
    reason <- "Exit at observation period end date, limited to current observation period"
    newCohort <- newCohort |>
      # filter to current observation period
      dplyr::filter(.data$observation_period_start_date <= .data$cohort_start_date) |>
      dplyr::compute(name = tmpTable, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtObservationEnd_limitToCurrentPeriod_")

  } else {
    reason <- "Exit at observation period end date"
    newCohort <- newCohort |>
      # filter to current
      dplyr::mutate(
        "cohort_start_date" = dplyr::if_else(
          .data$cohort_definition_id %in% .env$cohortId &
            .data$observation_period_start_date > .data$cohort_start_date,
          .data$observation_period_start_date,
          .data$cohort_start_date
        )
      ) |>
      dplyr::compute(name = tmpTable, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtObservationEnd_exit_")
  }

  newCohort <- newCohort |>
    dplyr::mutate(
      "cohort_end_date" = dplyr::if_else(
        .data$cohort_definition_id %in% .env$cohortId,
        .data$observation_period_end_date,
        .data$cohort_end_date
      )
    ) |>
    # no overlapping periods
    joinOverlap(name = tmpTable)

  if (!all(cohortId %in% settings(cohort)$cohort_definition_id)) {
    newCohort <- newCohort |>
      dplyr::union_all(
        cohort |> dplyr::filter(!.data$cohort_definition_d %in% .env$cohortId)
      ) |>
      dplyr::compute(name = tmpTable, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtObservationEnd_union_")
  }

  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = FALSE) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  omopgenerics::dropTable(cdm = cdm, name = tmpTable)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}


#' Set cohort end date to death date
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param requireDeath If TRUE, subjects without a death record will be dropped,
#' while if FALSE their end date will be left as is.
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
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort, dropExtraColumns = TRUE)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertLogical(requireDeath, length = 1)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |>
      dplyr::compute(name = name, temporary = FALSE,
                    logPrefix = "CohortConstructor_exitAtDeath_entry_")
    return(cdm[[name]])
  }

  # create new cohort
  newCohort <- cohort |>
    PatientProfiles::addDeathDate(name = name) |>
    # exit
    dplyr::mutate(
      "cohort_end_date" = dplyr::if_else(
        .data$cohort_definition_id %in% .env$cohortId &
          !is.na(.data$date_of_death),
        .data$date_of_death,
        .data$cohort_end_date
      )
    )

  if (requireDeath) {
    newCohort <- newCohort |>
      dplyr::filter(!is.na(.data$date_of_death) |
                      !.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtDeath_requireDeath_") |>
      omopgenerics::recordCohortAttrition(reason = "No death recorded", cohortId = cohortId)
  } else {
    newCohort <- newCohort |>
      dplyr::compute(name = name, temporary = FALSE)
  }

  newCohort <- newCohort |>
    # no overlapping periods
    joinOverlap(name = name) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = FALSE) |>
    omopgenerics::recordCohortAttrition(reason = "Exit at death",
                                        cohortId = cohortId)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}

