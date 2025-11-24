#' Set cohort end date to end of observation
#'
#' @description
#' `exitAtObservationEnd()` resets cohort end date based on a set of specified
#' column dates. The last date that occurs is chosen.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param persistAcrossObservationPeriods If FALSE, limits the cohort to one entry per
#' person, ending at the current observation period. If TRUE, subsequent
#' observation periods will create new cohort entries (starting from the start
#' of that observation period and ending at the end of that observation
#' period).
#' @inheritParams softValidationDoc
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
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |> exitAtObservationEnd()
#'}
exitAtObservationEnd <- function(cohort,
                                 cohortId = NULL,
                                 persistAcrossObservationPeriods = FALSE,
                                 name = tableName(cohort),
                                 .softValidation = FALSE) {
  # checks
  cohort <- omopgenerics::validateCohortArgument(cohort, dropExtraColumns = TRUE)
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertLogical(persistAcrossObservationPeriods, length = 1)
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtObservationEnd_entry_")
    return(cdm[[name]])
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

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
    dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                   logPrefix = "CohortConstructor_exitAtObservationEnd_filterFuture_")

  if (isFALSE(persistAcrossObservationPeriods)) {
    reason <- "Exit at observation period end date, limited to current observation period"
    newCohort <- newCohort |>
      # filter to current observation period
      dplyr::filter(.data$observation_period_start_date <= .data$cohort_start_date) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
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
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
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
    joinOverlap(name = tmpNewCohort)

  if (isTRUE(needsIdFilter(cohort = cohort, cohortId = cohortId))) {
    newCohort <- cdm[[tmpUnchanged]] |>
      dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
      dplyr::union_all(newCohort)
  }

  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = .softValidation) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

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
#' @inheritParams softValidationDoc
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
                        name = tableName(cohort),
                        .softValidation = FALSE) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort, dropExtraColumns = TRUE)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertLogical(requireDeath, length = 1)
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtDeath_entry_")
    return(cdm[[name]])
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  # create new cohort
  newCohort <- newCohort |>
    PatientProfiles::addDeathDate(name = tmpNewCohort) |>
    # exit
    dplyr::mutate(
      "cohort_end_date" = dplyr::if_else(
        !is.na(.data$date_of_death),
        .data$date_of_death,
        .data$cohort_end_date
      )
    )

  if (requireDeath) {
    newCohort <- newCohort |>
      dplyr::filter(!is.na(.data$date_of_death)) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtDeath_requireDeath_") |>
      omopgenerics::recordCohortAttrition(reason = "No death recorded", cohortId = cohortId)
  }

  newCohort <- newCohort |>
    # no overlapping periods
    joinOverlap(name = tmpNewCohort)

  if (isTRUE(needsIdFilter(cohort = cohort, cohortId = cohortId))) {
    newCohort <- newCohort |>
      dplyr::union_all(
        cdm[[tmpUnchanged]] |>
          dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort")))
      )
  }

  newCohort <- newCohort |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_exitAtDeath_name_"
    ) |>
    omopgenerics::newCohortTable(
      cohortAttritionRef = attrition(newCohort),
      .softValidation = .softValidation
    ) |>
    omopgenerics::recordCohortAttrition(reason = "Exit at death", cohortId = cohortId)

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}

