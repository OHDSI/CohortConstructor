#' Set cohort end date to the first of a set of column dates
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateColumns Date columns in the cohort table to consider.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param outOfObservation Strategy to handle end dates outside observation period:
#' "asis" to keep the original cohort end date, "drop" to drop those entries,
#' and "end" to set cohort end at observation end date.
#' @param na.rm If TRUE empty dates will not be used to compute the
#' end date, but if FALSE cohort end date will be NA when one of the
#' dates is empty.
#' @param name Name of the new cohort with the restriction.
#'
#' @return The cohort table.
#'
#'
#' @export
#'
#' @examples
#' library(PatientProfiles)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% exitAtFirstDate()

exitAtFirstDate <- function(cohort,
                            dateColumns,
                            cohortId = NULL,
                            returnReason = TRUE,
                            name = tableName(cohort),
                            .softValidation = FALSE) {
  exitAtColumnDate(
    cohort = cohort,
    dateColumns = dateColumns,
    cohortId = cohortId,
    returnReason = returnReason,
    name = name,
    order = "first",
    .softValidation = .softValidation
  )
}


#' Set cohort end date to the first of a set of column dates
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateColumns description
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param outOfObservation Strategy to handle end dates outside observation period:
#' "asis" to keep the original cohort end date, "drop" to drop those entries,
#' and "end" to set cohort end at observation end date.
#' @param na.rm If TRUE empty dates will not be used to compute the
#' end date, but if FALSE cohort end date will be NA when one of the
#' dates is empty.
#' @param name Name of the new cohort with the restriction.
#'
#' @return The cohort table.
#'
#'
#' @export
#'
#' @examples
#' library(PatientProfiles)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% exitAtLastDate()

exitAtLastDate <- function(cohort,
                           dateColumns,
                           cohortId = NULL,
                           returnReason = TRUE,
                           name = tableName(cohort),
                           .softValidation = FALSE) {
  exitAtColumnDate(
    cohort = cohort,
    dateColumns = dateColumns,
    cohortId = cohortId,
    returnReason = returnReason,
    name = name,
    order = "last",
    .softValidation = .softValidation
  )
}

exitAtColumnDate <- function(cohort,
                             dateColumns,
                             cohortId,
                             returnReason,
                             order,
                             name,
                             .softValidation) {
  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertLogical(returnReason, length = 1)
  validateCohortColumn(dateColumns, cohort, "Date")
  if ("cohort_start_date" %in% dateColumns) {
    cli::cli_abort("`cohort_start_date` cannot be one of the dateColumns")
  }
  assertLogical(.softValidation, length = 1)

  if (order == "first") {
    atDateFunction <- rlang::expr(min(.data$new_end_date_0123456789, na.rm = TRUE)) # NA always removed in SQL
  } else if (order == "last") {
    atDateFunction <- rlang::expr(max(.data$new_end_date_0123456789, na.rm = TRUE)) # NA always removed in SQL
  }

  tmpName <- omopgenerics::uniqueTableName()

  if (all(ids %in% cohortId)) {
    newCohort <- cohort |>
      dplyr::mutate("cohort_end_date_0123456789" = .data$cohort_end_date) |>
      dplyr::compute(name = tmpName, temporary = FALSE)
  } else {
    newCohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::mutate("cohort_end_date_0123456789" = .data$cohort_end_date) |>
      dplyr::compute(name = tmpName, temporary = FALSE)
  }

  newCohort <- newCohort |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(dateColumns),
      names_to = "exit_reason",
      values_to = "new_end_date_0123456789"
    ) |>
    dplyr::group_by(
      .data$cohort_definition_id, .data$subject_id,
      .data$cohort_start_date, .data$cohort_end_date_0123456789
    ) |>
    dplyr::filter(.data$new_end_date_0123456789 == !!atDateFunction) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(!"exit_reason")) |>
    dplyr::arrange(.data$exit_reason) |>
    dplyr::summarise(exit_reason = str_flatten(exit_reason, collapse = '; '), .groups = "drop") |>
    dplyr::mutate("cohort_end_date" = .data$new_end_date_0123456789) |>
    dplyr::select(!c("new_end_date_0123456789", "cohort_end_date_0123456789")) |>
    dplyr::distinct() |>
    dplyr::compute(name = tmpName, temporary = FALSE)

  # checks with informative errors
  validateNewCohort(newCohort, tmpName)

  if (any(!ids %in% cohortId)) {
    dateColumns <- dateColumns[dateColumns != "cohort_end_date"]
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(
        cohort |>
          dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId) |>
          dplyr::select(!dplyr::all_of(dateColumns)) |>
          dplyr::mutate(exit_reason = "cohort_end_date")
      ) |>
      dplyr::compute(name = tmpName, temporary = FALSE)
  }

  if (!returnReason) {
    newCohort <- newCohort |> dplyr::select(!"exit_reason")
  }

  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = .softValidation)

  cdm <- omopgenerics::dropTable(cdm, name = dplyr::starts_with(tmpName))

  return(newCohort)
}

validateNewCohort <- function(newCohort, tmpName) {
  ## NA
  checkNA <- newCohort |>
    dplyr::filter(is.na(.data$cohort_end_date)) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if (checkNA > 0) {
    cdm <- omopgenerics::dropTable(cdm, name = dplyr::starts_with(tmpName))
    cli::cli_abort("There should be at least one non-empty date in `dateColumns` options for all cohort records")
  }
  ## start > end
  checkStart <- newCohort |>
    dplyr::filter(.data$cohort_start_date > .data$cohort_end_date) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if (checkStart > 0) {
    cdm <- omopgenerics::dropTable(cdm, name = dplyr::starts_with(tmpName))
    cli::cli_abort("There are new cohort end dates smaller than the start date.
    Please provide dates valid date options")
  }
  ## Out of observation
  checkObservation <- newCohort |>
    PatientProfiles::addFutureObservation(
      futureObservationName = "observation_end_0123456789",
      futureObservationType = "date"
    ) |>
    dplyr::filter(.data$cohort_end_date > .data$observation_end_0123456789) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if (checkObservation > 0) {
    cdm <- omopgenerics::dropTable(cdm, name = dplyr::starts_with(tmpName))
    cli::cli_abort("There are new cohort end dates outside of the observation period.
    Please provide dates within the subject's observation period")
  }
  ## overlapping
  checkOverlap <- newCohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(next_start = dplyr::lead(.data$cohort_start_date)) |>
    dplyr::filter(.data$next_start <= .data$cohort_end_date) |>
    dplyr::ungroup() |>
    dplyr::tally() |>
    dplyr::pull("n")
  if (checkOverlap > 0) {
    cdm <- omopgenerics::dropTable(cdm, name = dplyr::starts_with(tmpName))
    cli::cli_abort("There are new cohort end dates which resulted in overlapping records.
                   Please check the dates provided in `dateColumns`.")
  }
}
