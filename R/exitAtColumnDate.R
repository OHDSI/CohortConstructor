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
                            multipleReasons = TRUE,
                            keepDateColumns = TRUE,
                            name = tableName(cohort),
                            .softValidation = FALSE) {
  exitAtColumnDate(
    cohort = cohort,
    dateColumns = dateColumns,
    cohortId = cohortId,
    returnReason = returnReason,
    multipleReasons = multipleReasons,
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
                           multipleReasons = TRUE,
                           keepDateColumns = TRUE,
                           name = tableName(cohort),
                           .softValidation = FALSE) {
  exitAtColumnDate(
    cohort = cohort,
    dateColumns = dateColumns,
    cohortId = cohortId,
    returnReason = returnReason,
    multipleReasons = multipleReasons,
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
                             multipleReasons,
                             order,
                             name,
                             exit,
                             keepDateColumns,
                             .softValidation,
                             call = parent.frame()) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning", call = call)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort), call = call)
  cohort <- omopgenerics::validateCohortArgument(cohort, call = call)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning", call = call)
  validateCohortColumn(dateColumns, cohort, "date")
  omopgenerics::assertLogical(returnReason, length = 1, call = call)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  omopgenerics::assertLogical(.softValidation, length = 1, call = call)
  omopgenerics::assertLogical(multipleReasons, length = 1, call = call)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
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
    dplyr::select(!dplyr::any_of(reason)) |>
    dplyr::mutate(!!!q) |>
    dplyr::compute(
      name = tmpNewCohort,
      logPrefix = "CohortConstructor_exitAtColumnDate_newDate_1_"
    )

  if (returnReason) {
    if (multipleReasons) {
      newCohort <- newCohort |>
        dplyr::mutate(dplyr::across(
          dplyr::all_of(dateColumns),
          \(x) dplyr::case_when(
            is.na(x) ~ 0,
            x == .data[[id]] ~ 1,
            .default = 0
          )
        )) |>
        dplyr::compute(
          name = tmpNewCohort,
          logPrefix = "CohortConstructor_exitAtColumnDate_newDate_2_"
        )
      createReasons <- newCohort |>
        dplyr::select(dplyr::all_of(dateColumns)) |>
        dplyr::distinct() |>
        dplyr::collect() |>
        dplyr::mutate(!!reason := "")
      for (col in dateColumns) {
        createReasons <- createReasons |>
          dplyr::mutate(!!reason := dplyr::case_when(
            .data[[col]] == 1 & .data[[reason]] == "" ~ col,
            .data[[col]] == 1 ~ paste0(.data[[reason]], "; ", col),
            .default = .data[[reason]]
          ))
      }
      nm <- omopgenerics::uniqueTableName()
      cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = createReasons)
      newCohort <- newCohort |>
        dplyr::inner_join(cdm[[nm]], by = dateColumns) |>
        dplyr::compute(
          name = tmpNewCohort,
          logPrefix = "CohortConstructor_exitAtColumnDate_newDate_3_"
        )
    } else {
      q <- paste0(
        "dplyr::case_when(",
        paste0(".data[['", dateColumns, "']] == .data[[id]] ~ '", dateColumns, "'", collapse = ", "),
        ")"
      ) |>
        rlang::parse_exprs() |>
        rlang::set_names(reason)
      newCohort <- newCohort |>
        dplyr::mutate(!!!q) |>
        dplyr::compute(
          name = tmpNewCohort,
          logPrefix = "CohortConstructor_exitAtColumnDate_newDate_2_"
        )
    }
  }

  newCohort <- newCohort |>
    dplyr::mutate(!!newDate := .data[[id]]) |>
    dplyr::select(!dplyr::all_of(id)) |>
    dplyr::compute(
      name = tmpNewCohort,
      logPrefix = "CohortConstructor_exitAtColumnDate_newDate_4_"
    )

  # checks with informative errors
  if (isFALSE(.softValidation)) {
    cdm <- validateNewCohort(newCohort, cdm, tablePrefix)
  }

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    if (!reason %in% colnames(cdm[[tmpUnchanged]])) {
      cdm[[tmpUnchanged]] <- cdm[[tmpUnchanged]] |>
        dplyr::mutate(!!reason := !!newDate)
    }

    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(cdm[[tmpUnchanged]]) |>
      dplyr::compute(
        name = tmpNewCohort,
        logPrefix = "CohortConstructor_exitAtColumnDate_union_"
      )
  }

  if (!keepDateColumns) {
    newCohort <- newCohort |>
      dplyr::select(!dplyr::all_of(keepDateColumns))
  }

  newCohort <- newCohort |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(
      name = name,
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
