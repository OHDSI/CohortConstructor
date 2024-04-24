#' Require that an index date is within a date range
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param dateRange A window of time during which the index date must have
#' been observed.
#' @param indexDate Variable in cohort that contains the index date of interest
#' @param name Name of the new cohort with the restriction.
#'
#' @return The cohort table with any cohort entries outside of the date range
#' dropped
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   requireInDateRange(indexDate = "cohort_start_date",
#'                      dateRange = as.Date(c("2010-01-01", "2019-01-01")))
requireInDateRange <- function(cohort,
                               cohortId = NULL,
                               dateRange,
                               indexDate = "cohort_start_date",
                               name = omopgenerics::tableName(cohort)) {

  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  validateDateRange(dateRange)

  # requirement
  cohort <- cohort |>
    dplyr::filter(
      (.data[[indexDate]] >= !!dateRange[1] & .data[[indexDate]] <= !!dateRange[2]) |
        (!.data$cohort_definition_id %in% cohortId)
    ) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    CDMConnector::recordCohortAttrition(
      reason = paste0(indexDate, " between ", dateRange[1], " & ", dateRange[2]),
      cohortId = cohortId
    )

  return(cohort)
}

#' Trim cohort dates to be within a date range
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param dateRange A window of time during which the index date must have
#' been observed.
#' @param startDate Variable with earliest date.
#' @param endDate Variable with latest date.
#' @param name Name of the new cohort with the restriction.
#'
#'
#' @return The cohort table with record timings updated to only be within the
#' date range. Any records with all time outside of the range will have
#' been dropped.
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   trimToDateRange(startDate = "cohort_start_date",
#'                   endDate = "cohort_end_date",
#'                   dateRange = as.Date(c("2015-01-01",
#'                                         "2015-12-31")))
trimToDateRange <- function(cohort,
                            cohortId = NULL,
                            dateRange,
                            startDate = "cohort_start_date",
                            endDate = "cohort_end_date",
                            name = omopgenerics::tableName(cohort)) {

  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(startDate, cohort)
  validateCohortColumn(endDate, cohort)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  validateDateRange(dateRange)

  # requirement
  cohort <- cohort %>%
    trimStartDate(
      requirementIds = cohortId,
      startDate = startDate,
      endDate = endDate,
      minDate = dateRange[1]
    ) %>%
    dplyr::compute(name = name, temporary = FALSE) %>%
    CDMConnector::recordCohortAttrition(
      reason = paste0(startDate, " >= ", dateRange[1]),
      cohortId = cohortId
    ) %>%
    trimEndDate(
      requirementIds = cohortId,
      startDate = startDate,
      endDate = endDate,
      maxDate = dateRange[2]
    ) %>%
    dplyr::compute(name = name, temporary = FALSE) %>%
    CDMConnector::recordCohortAttrition(
      reason = paste0(endDate, " <= ", dateRange[2]),
      cohortId = cohortId
    )

  return(cohort)
}

trimStartDate <- function(cohort,
                          startDate,
                          endDate,
                          minDate,
                          requirementIds) {

  if (!is.na(startDate)) {
    cohort <- cohort %>%
      dplyr::mutate(!!startDate := dplyr::if_else(
        .data[[startDate]] <= !!minDate &
          .data$cohort_definition_id %in% .env$requirementIds,
        as.Date(minDate), .data[[startDate]]
      )) %>%
      dplyr::filter(
        .data[[startDate]] <= .data[[endDate]] |
         (!.data$cohort_definition_id %in% .env$requirementId)
      )
  }
  return(cohort)
}
trimEndDate <- function(cohort,
                        startDate,
                        endDate,
                        maxDate,
                        requirementIds) {

  if (!is.na(endDate)) {
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
  }
  return(cohort)
}
