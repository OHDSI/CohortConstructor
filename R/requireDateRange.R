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
                               name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  dateRange <- validateDateRange(dateRange)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireInDateRange_entry_")
    return(cdm[[name]])
  }

  # requirement
  if (!is.na(dateRange[1])) {
    cohort <- cohort |>
      dplyr::filter(.data[[indexDate]] >= !!dateRange[1] |
                      (!.data$cohort_definition_id %in% cohortId)) |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireInDateRange_dateRange1_") |>
      omopgenerics::recordCohortAttrition(reason = "{indexDate} after {dateRange[1]}",
                                          cohortId = cohortId)
  }

  if (!is.na(dateRange[2])) {
    cohort <- cohort |>
      dplyr::filter(.data[[indexDate]] <= !!dateRange[2] |
                      (!.data$cohort_definition_id %in% cohortId)) |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireInDateRange_dateRange2_") |>
      omopgenerics::recordCohortAttrition(reason = "{indexDate} before {dateRange[2]}",
                                          cohortId = cohortId)
  }

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_requireInDateRange_newCohort_") |>
    omopgenerics::newCohortTable(.softValidation = TRUE)

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
                            name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(startDate, cohort, class = "Date")
  validateCohortColumn(endDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  dateRange <- validateDateRange(dateRange)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_trimToDateRange_entry_")
    return(cdm[[name]])
  }

  # trim start
  if (!is.na(dateRange[1])) {
    cohort <- cohort |>
      trimStartDate(
        requirementIds = cohortId,
        startDate = startDate,
        endDate = endDate,
        minDate = dateRange[1]
      ) |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_trimToDateRange_start_") |>
      omopgenerics::recordCohortAttrition(reason = "{startDate} trimmed >= {dateRange[1]}",
                                          cohortId = cohortId)
  }

  # trim end
  if (!is.na(dateRange[2])) {
    cohort <- cohort |>
      trimEndDate(
        requirementIds = cohortId,
        startDate = startDate,
        endDate = endDate,
        maxDate = dateRange[2]
      ) |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_trimToDateRange_end_") |>
      omopgenerics::recordCohortAttrition(reason = "{endDate} trimmed <= {dateRange[2]}",
                                          cohortId = cohortId)
  }

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_trimToDateRange_newCohort_") |>
    omopgenerics::newCohortTable(.softValidation = FALSE)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cohort)
}

trimStartDate <- function(cohort,
                          startDate,
                          endDate,
                          minDate,
                          requirementIds) {
  cohort <- cohort |>
    dplyr::mutate(
      !!startDate := dplyr::if_else(
        .data[[startDate]] <= !!minDate &
          .data$cohort_definition_id %in% .env$requirementIds,
        as.Date(minDate),
        .data[[startDate]]
      )
    ) |>
    dplyr::filter(.data[[startDate]] <= .data[[endDate]] |
                    (!.data$cohort_definition_id %in% .env$requirementIds))
  return(cohort)
}

trimEndDate <- function(cohort,
                        startDate,
                        endDate,
                        maxDate,
                        requirementIds) {
  cohort <- cohort |>
    dplyr::mutate(!!endDate := dplyr::if_else(
      .data[[endDate]] >= !!maxDate &
        (.data$cohort_definition_id %in% .env$requirementIds),
      as.Date(maxDate),
      .data[[endDate]]
    )) |>
    dplyr::filter(.data[[startDate]] <= .data[[endDate]] |
                    (!.data$cohort_definition_id %in% requirementIds))
  return(cohort)
}
