#' Require that an index date is within a date range
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateRange A window of time during which the index date must have
#' been observed.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param indexDate Variable in cohort that contains the index date of interest
#' @param name Name of the new cohort with the restriction.
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
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  validateDateRange(dateRange)

  # requirement
  if (!is.na(dateRange[1])) {
    cohort <- cohort |>
      dplyr::filter(
        .data[[indexDate]] >= !!dateRange[1] | (!.data$cohort_definition_id %in% cohortId)
      ) |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::recordCohortAttrition(
        reason = paste0(indexDate, " after ", dateRange[1]),
        cohortId = cohortId
      )
  }

  if (!is.na(dateRange[2])) {
    cohort <- cohort |>
      dplyr::filter(
        .data[[indexDate]] <= !!dateRange[2] | (!.data$cohort_definition_id %in% cohortId)
      ) |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::recordCohortAttrition(
        reason = paste0(indexDate, " before ", dateRange[2]),
        cohortId = cohortId
      )
  }

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE)

  return(cohort)
}

#' Trim cohort dates to be within a date range
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateRange A window of time during which the index date must have
#' been observed.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param startDate Variable with earliest date.
#' @param endDate Variable with latest date.
#' @param name Name of the new cohort with the restriction.
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
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(startDate, cohort, class = "Date")
  validateCohortColumn(endDate, cohort, class = "Date")
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  validateDateRange(dateRange)

  # trim start
  if (!is.na(dateRange[1])) {
    cohort <- cohort |>
      trimStartDate(
        requirementIds = cohortId,
        startDate = startDate,
        endDate = endDate,
        minDate = dateRange[1]
      ) %>%
      dplyr::compute(name = name, temporary = FALSE) %>%
      omopgenerics::recordCohortAttrition(
        reason = paste0(startDate, " >= ", dateRange[1]),
        cohortId = cohortId
      )
  }

  # trim end
  if (!is.na(dateRange[2])) {
    cohort <- cohort |>
      trimEndDate(
        requirementIds = cohortId,
        startDate = startDate,
        endDate = endDate,
        maxDate = dateRange[2]
      ) %>%
      dplyr::compute(name = name, temporary = FALSE) %>%
      omopgenerics::recordCohortAttrition(
        reason = paste0(endDate, " <= ", dateRange[2]),
        cohortId = cohortId
      )
  }

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE)

  return(cohort)
}

trimStartDate <- function(cohort,
                          startDate,
                          endDate,
                          minDate,
                          requirementIds) {
  cohort <- cohort %>%
    dplyr::mutate(!!startDate := dplyr::if_else(
      .data[[startDate]] <= !!minDate &
        .data$cohort_definition_id %in% .env$requirementIds,
      as.Date(minDate), .data[[startDate]]
    )) %>%
    dplyr::filter(
      .data[[startDate]] <= .data[[endDate]] |
        (!.data$cohort_definition_id %in% .env$requirementIds)
    )
  return(cohort)
}

trimEndDate <- function(cohort,
                        startDate,
                        endDate,
                        maxDate,
                        requirementIds) {
  cohort <- cohort %>%
    dplyr::mutate(!!endDate := dplyr::if_else(
      .data[[endDate]] >= !!maxDate &
        (.data$cohort_definition_id %in% .env$requirementIds),
      as.Date(maxDate), .data[[endDate]]
    )) %>%
    dplyr::filter(
      .data[[startDate]] <= .data[[endDate]] |
        (!.data$cohort_definition_id %in% requirementIds)
    )
  return(cohort)
}
