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
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   addTableIntersectDate(tableName = "observation", nameStyle = "next_obs", order = "first") |>
#'   addFutureObservation(futureObservationType = "date", name = "cohort1")
#'
#' cdm$cohort1 |>
#'   exitAtFirstDate(dateColumns = c("next_obs", "future_observation"))
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
    missingName = missing(name),
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
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   addTableIntersectDate(tableName = "observation", nameStyle = "next_obs", order = "first") |>
#'   addFutureObservation(futureObservationType = "date", name = "cohort1")
#'
#' cdm$cohort1 |>
#'   exitAtLastDate(dateColumns = c("next_obs", "future_observation"))
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
    cohortId = {{cohortId}},
    returnReason = returnReason,
    missingName = missing(name),
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
                             missingName,
                             name,
                             exit,
                             keepDateColumns,
                             .softValidation,
                             call = parent.frame()) {
  # checks
  name <- validateNameArgumentInternal(missingName, name, tableName(cohort), call = call)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort), call = call)
  cohort <- omopgenerics::validateCohortArgument(cohort, call = call)
  cohortId <- omopgenerics::validateCohortIdArgument(cohortId = {{cohortId}}, cohort, validation = "warning", call = call)
  validateCohortColumn(dateColumns, cohort, "date")
  omopgenerics::assertLogical(returnReason, length = 1, call = call)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  omopgenerics::assertLogical(.softValidation, length = 1, call = call)

  if (length(cohortId) == 0) {
    cli::cli_warn("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_exitAtColumnDate_entryCohort_")
    return(cdm[[name]])
  }

  if (exit) {
    newDate <- "cohort_end_date"
    reason <- "exit_reason"
  } else {
    newDate <- "cohort_start_date"
    reason <- "entry_reason"
  }

  if (reason %in% colnames(cohort)) {
    cli::cli_inform("Column {reason} will be overwritten for cohort ID: {cohortId}.")
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

  # add min/max column
  id <- omopgenerics::uniqueId(exclude = colnames(cdm[[tmpNewCohort]]))
  q <- paste0(
    ifelse(order == "first", "pmin(", "pmax("),
    paste0(".data[['", dateColumns, "']]", collapse = ", "),
    ", na.rm = TRUE)"
  ) |>
    rlang::parse_exprs() |>
    rlang::set_names(id)
  newCohort <- cdm[[tmpNewCohort]] |>
    dplyr::mutate(!!!q) |>
    dplyr::compute(
      name = tmpNewCohort,
      logPrefix = "CohortConstructor_exitAtColumnDate_newDate_1_"
    )

  if (returnReason) {
    newCohort <- newCohort |>
      dplyr::select(!dplyr::any_of(reason))
    q <- paste0(
      "dplyr::case_when(",
      paste0(
        ".data[['", dateColumns, "']] == .data[[id]] ~ '", dateColumns, "'",
        collapse = ", "
      ),
      ")"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(reason)
    newCohort <- newCohort |>
      dplyr::mutate(!!!q)
  }

  newCohort <- newCohort |>
    dplyr::select(!dplyr::any_of(newDate)) |>
    dplyr::rename(rlang::set_names(id, newDate)) |>
    dplyr::compute(
      name = tmpNewCohort,
      logPrefix = "CohortConstructor_exitAtColumnDate_newDate_2_"
    )

  # check dates (no overlap) with informative errors if .softValidation,
  # otherwise do omopgenerics validation
  if (isTRUE(.softValidation)) {
    cdm <- validateNewCohort(newCohort, cdm, tablePrefix, exit)
  }

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    if (!reason %in% colnames(cdm[[tmpUnchanged]]) & returnReason) {
      cdm[[tmpUnchanged]] <- cdm[[tmpUnchanged]] |>
        dplyr::mutate(!!reason := !!newDate)
    }

    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(cdm[[tmpUnchanged]])
  }

  cohortCols <- omopgenerics::cohortColumns("cohort")

  if (!keepDateColumns) {
    dateColumns <- dateColumns[!dateColumns %in% cohortCols]
    newCohort <- newCohort |>
      dplyr::select(!dplyr::all_of(dateColumns))
  }

  newCohort <- newCohort |>
    dplyr::relocate(dplyr::all_of(cohortCols)) |>
    dplyr::compute(
      name = name,
      logPrefix = "CohortConstructor_exitAtColumnDate_relocate_"
    ) |>
    omopgenerics::newCohortTable(.softValidation = TRUE)

  # drop temp tables before validation
  cdm <- omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(tablePrefix))
  if (!.softValidation) {
    newCohort <- newCohort |>
      omopgenerics::newCohortTable(.softValidation = .softValidation)
  }

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}

validateNewCohort <- function(newCohort, cdm, tmpName, exit) {
  ## start > end
  checkStart <- newCohort |>
    dplyr::filter(.data$cohort_start_date > .data$cohort_end_date) |>
    dplyr::filter(dplyr::row_number() <= 3) |>
    dplyr::pull("subject_id")
  if (exit) {
    suggestion <- "Please ensure all potential exit dates come after current cohort start dates."
  } else {
    suggestion <- "Please ensure all potential entry dates come before current cohort end dates."
  }
  if (length(checkStart) > 0) {
    cdm <- omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(tmpName))
    cli::cli_abort(c(
      "New cohort dates result in some subjects having cohort end date earlier than cohort start date.",
      "See for example subject IDs {glue::glue_collapse(checkStart, sep = ', ', last = ', and ')}.",
      "i" = suggestion
    ))
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
      "There are new cohort dates outside of the observation period.",
      "i" = suggestion
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
    cli::cli_warn(
      "There are new cohort end dates which resulted in overlapping records.",
      "i" = suggestion
    )
  }

  return(cdm)
}
