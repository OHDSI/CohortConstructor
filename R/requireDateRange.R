#' Require that an index date is within a date range
#'
#' @description
#' `requireInDateRange()` filters cohort records, keeping only those for
#' which the index date is within the specified date range.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param dateRange A date vector with the minimum and maximum dates between
#' which the index date must have been observed.
#' @param indexDate Name of the column in the cohort that contains the date of
#' interest.
#' @inheritParams softValidationDoc
#'
#' @return The cohort table with any cohort entries outside of the date range
#' dropped
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#' cdm$cohort1 |>
#'   requireInDateRange(indexDate = "cohort_start_date",
#'                      dateRange = as.Date(c("2010-01-01", "2019-01-01")))
#' }
requireInDateRange <- function(cohort,
                               dateRange,
                               cohortId = NULL,
                               indexDate = "cohort_start_date",
                               name = tableName(cohort),
                               .softValidation = TRUE) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  dateRange <- validateDateRange(dateRange)
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireInDateRange_entry_")
    return(cdm[[name]])
  }

  # filters
  filt1 <- glue::glue(".data[[indexDate]] >= as.Date('{dateRange[1]}')")
  filt2 <- glue::glue(".data[[indexDate]] <= as.Date('{dateRange[2]}')")
  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    filt1 <- glue::glue("{filt1} | (!.data$cohort_definition_id %in% cohortId)")
    filt2 <- glue::glue("{filt2} | (!.data$cohort_definition_id %in% cohortId)")
  }
  filt1 <- rlang::parse_exprs(glue::glue(filt1))
  filt2 <- rlang::parse_exprs(glue::glue(filt2))

  # requirement
  if (!is.na(dateRange[1])) {
    cohort <- cohort |>
      dplyr::filter(!!!filt1) |>
      dplyr::compute(
        name = name, temporary = FALSE,
        logPrefix = "CohortConstructor_requireInDateRange_dateRange1_"
      ) |>
      omopgenerics::recordCohortAttrition(reason = "{indexDate} after {dateRange[1]}",
                                          cohortId = cohortId)
  }

  if (!is.na(dateRange[2])) {
    cohort <- cohort |>
      dplyr::filter(!!!filt2) |>
      dplyr::compute(
        name = name, temporary = FALSE,
        logPrefix = "CohortConstructor_requireInDateRange_dateRange2_"
      ) |>
      omopgenerics::recordCohortAttrition(reason = "{indexDate} before {dateRange[2]}",
                                          cohortId = cohortId)
  }

  cohort <- cohort |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_requireInDateRange_newCohort_"
    ) |>
    omopgenerics::newCohortTable(.softValidation = .softValidation)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cohort)
}

#' Trim cohort dates to be within a date range
#'
#' @description
#' `trimToDateRange()` resets the cohort start and end date based on the
#' specified date range.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param dateRange A window of time during which the start and end date must
#'  have been observed.
#' @param startDate Variable with earliest date.
#' @param endDate Variable with latest date.
#' @inheritParams softValidationDoc
#'
#' @return The cohort table with record timings updated to only be within the
#' date range. Any records with all time outside of the range will have
#' been dropped.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   trimToDateRange(startDate = "cohort_start_date",
#'                   endDate = "cohort_end_date",
#'                   dateRange = as.Date(c("2015-01-01",
#'                                         "2015-12-31")))
#' }
trimToDateRange <- function(cohort,
                            dateRange,
                            cohortId = NULL,
                            startDate = "cohort_start_date",
                            endDate = "cohort_end_date",
                            name = tableName(cohort),
                            .softValidation = FALSE) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(startDate, cohort, class = "Date")
  validateCohortColumn(endDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  dateRange <- validateDateRange(dateRange)
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |>
      dplyr::compute(
        name = name, temporary = FALSE,
        logPrefix = "CohortConstructor_trimToDateRange_entry_"
      )
    return(cdm[[name]])
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  # trim start
  if (!is.na(dateRange[1])) {
    newCohort <- newCohort |>
      trimStartDate(
        startDate = startDate,
        endDate = endDate,
        minDate = dateRange[1]
      ) |>
      dplyr::compute(
        name = tmpNewCohort, temporary = FALSE,
        logPrefix = "CohortConstructor_trimToDateRange_start_"
      ) |>
      omopgenerics::recordCohortAttrition(
        reason = "{startDate} trimmed >= {dateRange[1]}", cohortId = cohortId
      )
  }

  # trim end
  if (!is.na(dateRange[2])) {
    newCohort <- newCohort |>
      trimEndDate(
        startDate = startDate,
        endDate = endDate,
        maxDate = dateRange[2]
      ) |>
      dplyr::compute(
        name = tmpNewCohort, temporary = FALSE,
        logPrefix = "CohortConstructor_trimToDateRange_end_"
      ) |>
      omopgenerics::recordCohortAttrition(
        reason = "{endDate} trimmed <= {dateRange[2]}", cohortId = cohortId
      )
  }

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(cdm[[tmpUnchanged]]) |>
      dplyr::compute(
        name = tmpNewCohort, temporary = FALSE,
        logPrefix = "CohortConstructor_requireDateRange_union_"
      )
  }

  newCohort <- newCohort |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_trimToDateRange_newCohort_"
    ) |>
    omopgenerics::newCohortTable(.softValidation = .softValidation)

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

trimStartDate <- function(cohort,
                          startDate,
                          endDate,
                          minDate) {
  cohort <- cohort |>
    dplyr::mutate(
      !!startDate := dplyr::if_else(
        .data[[startDate]] <= !!minDate,
        as.Date(minDate),
        .data[[startDate]]
      )
    ) |>
    dplyr::filter(.data[[startDate]] <= .data[[endDate]])
  return(cohort)
}

trimEndDate <- function(cohort,
                        startDate,
                        endDate,
                        maxDate) {
  cohort <- cohort |>
    dplyr::mutate(!!endDate := dplyr::if_else(
      .data[[endDate]] >= !!maxDate,
      as.Date(maxDate),
      .data[[endDate]]
    )) |>
    dplyr::filter(.data[[startDate]] <= .data[[endDate]])
  return(cohort)
}
