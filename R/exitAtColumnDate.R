#' Set cohort end date to the first of a set of column dates
#'
#' @description
#' `exitAtFirstDate()` resets cohort end date based on a set of specified
#' column dates. The first date that occurs is chosen.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams columnDateDoc
#' @inheritParams nameDoc
#' @inheritParams softValidationDoc
#'
#' @return The cohort table.
#'
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' library(PatientProfiles)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   addTableIntersectDate(tableName = "observation", nameStyle = "next_obs", order = "first") |>
#'   addFutureObservation(futureObservationType = "date", name = "cohort1")
#'
#' cdm$cohort1 |>
#'   exitAtFirstDate(dateColumns = c("next_obs", "future_observation"))
#' }
#' }
exitAtFirstDate <- function(cohort,
                            dateColumns,
                            cohortId = NULL,
                            returnReason = FALSE,
                            keepDateColumns = TRUE,
                            name = tableName(cohort),
                            .softValidation = FALSE) {
  exitAtColumnDate(
    cohort = cohort,
    dateColumns = dateColumns,
    cohortId = cohortId,
    returnReason = returnReason,
    name = name,
    order = "first",
    exit = TRUE,
    keepDateColumns = keepDateColumns,
    .softValidation = .softValidation
  )
}


#' Set cohort end date to the last of a set of column dates
#'
#' @description
#' `exitAtLastDate()` resets cohort end date based on a set of specified
#' column dates. The last date that occurs is chosen.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams columnDateDoc
#' @inheritParams nameDoc
#' @inheritParams softValidationDoc
#'
#' @return The cohort table.
#'
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' library(PatientProfiles)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   addTableIntersectDate(tableName = "observation", nameStyle = "next_obs", order = "first") |>
#'   addFutureObservation(futureObservationType = "date", name = "cohort1")
#'
#' cdm$cohort1 |>
#'   exitAtLastDate(dateColumns = c("next_obs", "future_observation"))
#' }
#' }
exitAtLastDate <- function(cohort,
                           dateColumns,
                           cohortId = NULL,
                           returnReason = FALSE,
                           keepDateColumns = TRUE,
                           name = tableName(cohort),
                           .softValidation = FALSE) {
  exitAtColumnDate(
    cohort = cohort,
    dateColumns = dateColumns,
    cohortId = cohortId,
    returnReason = returnReason,
    name = name,
    order = "last",
    exit = TRUE,
    keepDateColumns = keepDateColumns,
    .softValidation = .softValidation
  )
}

exitAtColumnDate <- function(cohort,
                             dateColumns,
                             cohortId,
                             returnReason,
                             order,
                             name,
                             exit,
                             keepDateColumns,
                             .softValidation) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  validateCohortColumn(dateColumns, cohort, "date")
  omopgenerics::assertLogical(returnReason, length = 1)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_exitAtColumnDate_entryCohort_")
    return(cdm[[name]])
  }

  if (order == "first") {
    atDateFunction <- rlang::expr(min(.data$new_date_0123456789, na.rm = TRUE)) # NA always removed in SQL
  } else if (order == "last") {
    atDateFunction <- rlang::expr(max(.data$new_date_0123456789, na.rm = TRUE)) # NA always removed in SQL
  }

  if (exit) {
    newDate <- "cohort_end_date"
    keptDate <- "cohort_start_date"
    reason <- "exit_reason"
  } else {
    newDate <- "cohort_start_date"
    keptDate <- "cohort_end_date"
    reason <- "entry_reason"
  }

  if (reason %in% colnames(cohort) & returnReason) {
    cli::cli_inform("Column {reason} will be overwritten.")
    cohort <- cohort |> dplyr::select(!dplyr::all_of(reason))
  } else if (reason %in% colnames(cohort) & !returnReason) {
    reason <- "date_column_name_1234"
  }

  # check NA
  checkNA <- cohort |>
    dplyr::filter(dplyr::if_all(
      .cols = dplyr::all_of(dateColumns),
      .fns = ~ is.na(.x)
    )) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if (checkNA > 0) {
    cli::cli_abort("All cohort records must have at least one non-empty date in the `dateColumns`")
  }


  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]] |>
    dplyr::mutate(
      "cohort_start_date_0123456789" = .data$cohort_start_date,
      "cohort_end_date_0123456789" = .data$cohort_end_date
    )

  newCohort <- newCohort |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(dateColumns),
      names_to = reason,
      values_to = "new_date_0123456789"
    ) |>
    dplyr::group_by(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$cohort_start_date_0123456789,
      .data$cohort_end_date_0123456789
    ) |>
    dplyr::filter(.data$new_date_0123456789 == !!atDateFunction) |>
    dplyr::ungroup() |>
    dplyr::compute(
      name = tmpNewCohort, temporary = FALSE,
      logPrefix = "CohortConstructor_exitAtColumnDate_newDate_1_"
    )

  if (returnReason) {
    if (omopgenerics::sourceType(cdm) == "spark") {
      newCohort <- newCohort |>
        dplyr::group_by(dplyr::across(!dplyr::all_of(reason))) |>
        dplyr::arrange(.data[[reason]]) |>
        dplyr::summarise(
          !!reason := dplyr::sql(paste0("CONCAT_WS(', ', COLLECT_LIST(", reason, "))")),
          .groups = "drop"
        ) |>
        dplyr::compute(
          name = tmpNewCohort, temporary = FALSE,
          logPrefix = "CohortConstructor_exitAtColumnDate_newDate_1_"
        )
    } else {
      newCohort <- newCohort |>
        dplyr::group_by(dplyr::across(!dplyr::all_of(reason))) |>
        dplyr::arrange(.data[[reason]]) |>
        dplyr::summarise(
          !!reason := stringr::str_flatten(.data[[reason]], collapse = '; '),
          .groups = "drop"
        ) |>
        dplyr::compute(
          name = tmpNewCohort, temporary = FALSE,
          logPrefix = "CohortConstructor_exitAtColumnDate_newDate_1_"
        )
    }
    excludeReason <- NULL
  } else {
    excludeReason <- reason
  }

  newCohort <- newCohort |>
    dplyr::mutate(!!newDate := .data$new_date_0123456789, !!keptDate := .data[[paste0(keptDate, "_0123456789")]]) |>
    dplyr::select(!dplyr::all_of(c(
      "new_date_0123456789", "cohort_end_date_0123456789", "cohort_start_date_0123456789", excludeReason
    ))) |>
    dplyr::distinct() |>
    dplyr::compute(
      name = tmpNewCohort, temporary = FALSE,
      logPrefix = "CohortConstructor_exitAtColumnDate_newDate_1_"
    )

  # checks with informative errors
  if (isFALSE(.softValidation)) {
    cdm <- validateNewCohort(newCohort, cdm, tablePrefix)
  }

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    dateColumns <- dateColumns[!dateColumns %in% c("cohort_end_date", "cohort_start_date")]
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(
        cdm[[tmpUnchanged]]  |>
          dplyr::mutate(!!reason := !!newDate) |>
          dplyr::select(!dplyr::all_of(c(dateColumns, excludeReason)))
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_exitAtColumnDate_union_")
  }

  if (keepDateColumns) {
    newCohort <- newCohort |>
      dplyr::inner_join(
        cohort |>
          dplyr::select(dplyr::any_of(c(
            "cohort_definition_id", "subject_id", keptDate, dateColumns
          ))) |>
          dplyr::select(!dplyr::any_of(newDate))
      ) |>
      dplyr::compute(
        name = tmpNewCohort, temporary = FALSE,
        logPrefix = "CohortConstructor_exitAtColumnDate_keepDates_"
      )
  }

  newCohort <- newCohort |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_exitAtColumnDate_relocate_"
    ) |>
    omopgenerics::newCohortTable(.softValidation = .softValidation)

  cdm <- omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}

validateNewCohort <- function(newCohort, cdm, tmpName) {
  ## start > end
  checkStart <- newCohort |>
    dplyr::filter(.data$cohort_start_date > .data$cohort_end_date) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if (checkStart > 0) {
    cdm <- omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(tmpName))
    cli::cli_abort(
      "There are new cohort end dates smaller than the start date.
    Please provide valid dates in `dateColumns`"
    )
  }
  ## Out of observation
  checkObservation <- newCohort |>
    PatientProfiles::addFutureObservation(
      futureObservationName = "observation_end_0123456789",
      futureObservationType = "date",
      name = omopgenerics::uniqueTableName(prefix = tmpName)
    ) |>
    dplyr::filter(.data$cohort_end_date > .data$observation_end_0123456789) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if (checkObservation > 0) {
    cdm <- omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(tmpName))
    cli::cli_abort(
      "There are new cohort end dates outside of the observation period.
    Please provide dates in observation in `dateColumns`"
    )
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
    cdm <- omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(tmpName))
    cli::cli_abort(
      "There are new cohort end dates which resulted in overlapping records.
                   Please check the dates provided in `dateColumns`."
    )
  }

  return(cdm)
}
