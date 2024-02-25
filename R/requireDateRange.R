#' Require that an index date is within a date range
#'
#' @param cohort A cohort table in a cdm reference
#' @param indexDate Variable in cohort that contains the index date of interest
#' @param dateRange A window of time during which the index date must have
#' been observed
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
                             indexDate = "cohort_start_date",
                             dateRange = as.Date(c(NA, NA))) {

  checkCohort(cohort)
  checkDateVariable(cohort = cohort, dateVar = indexDate)
  checkDateRange(dateRange)

  cohort <- cohort %>%
    dplyr::filter(.data[[indexDate]] >= !!dateRange[1] &
                    .data[[indexDate]] <= !!dateRange[2]) %>%
    CDMConnector::recordCohortAttrition(reason = paste0(
      indexDate,
      " between ", dateRange[1], " & ", dateRange[2]
    ))

  cohort

}

#' Trim cohort dates to be within a date range
#'
#' @param cohort A cohort table in a cdm reference
#' @param startDate Variable with earliest date
#' @param endDate Variable with latest date
#' @param dateRange A window of time during which the index date must have
#' been observed
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
                            startDate = "cohort_start_date",
                            endDate = "cohort_end_date",
                            dateRange = as.Date(c(NA, NA))) {

  checkCohort(cohort)
  checkDateVariable(cohort = cohort, dateVar = startDate)
  checkDateVariable(cohort = cohort, dateVar = endDate)
  checkDateRange(dateRange)

  #
  #   # validate inputs
  #   if (!isTRUE(inherits(cdm, "cdm_reference"))) {
  #     cli::cli_abort("cohort must be part of a cdm reference")
  #   }
  #   if (!"GeneratedCohortSet" %in% class(cohort) ||
  #     !all(c(
  #       "cohort_definition_id", "subject_id",
  #       "cohort_start_date", "cohort_end_date"
  #     ) %in%
  #       colnames(cohort))) {
  #     cli::cli_abort("cohort must be a GeneratedCohortSet")
  #   }
  #
  #   if (!indexDate %in% colnames(cohort)) {
  #     cli::cli_abort(paste0(indexDate, " must be a date column in the cohort table"))
  #   }
  #
  #   if (!endDateName %in% colnames(cohort)) {
  #     cli::cli_abort(paste0(endDateName, " must be a date column in the cohort table"))
  #   }

  cohort <- trimStartDate(cohort = cohort,
                          startDate = startDate,
                          endDate = endDate,
                          minDate = dateRange[1]
  ) %>%
    CDMConnector::recordCohortAttrition(reason = paste0(
      startDate,
      " >= ", dateRange[1]))

  cohort <- trimEndDate(
    cohort = cohort,
    startDate = startDate,
    endDate = endDate,
    maxDate = dateRange[2]
  ) %>%
    CDMConnector::recordCohortAttrition(reason = paste0(
      endDate,
      " <= ", dateRange[2]
    ))

  cohort
}

trimStartDate <- function(cohort,
                          startDate,
                          endDate,
                          minDate) {

  if (!is.na(startDate)) {
    cohort <- cohort %>%
      dplyr::mutate(!!startDate := dplyr::if_else(
        .data[[startDate]] <= !!minDate,
        as.Date(minDate), .data[[startDate]]
      )) %>%
      dplyr::filter(.data[[startDate]] <= .data[[endDate]])
  }
  return(cohort)
}
trimEndDate <- function(
    cohort,
    startDate,
    endDate,
    maxDate) {

  if (!is.na(endDate)) {
    cohort <- cohort %>%
      dplyr::mutate(!!endDate := dplyr::if_else(
        .data[[endDate]] >= !!maxDate,
        as.Date(maxDate), .data[[endDate]]
      )) %>%
      dplyr::filter(.data[[startDate]] <= .data[[endDate]])
  }
  return(cohort)
}


checkCohort <- function(cohort){
  if (!"GeneratedCohortSet" %in% class(cohort) ||
      !all(c(
        "cohort_definition_id", "subject_id",
        "cohort_start_date", "cohort_end_date"
      ) %in%
      colnames(cohort))) {
    cli::cli_abort("cohort must be a GeneratedCohortSet")
  }
}

checkDateVariable <- function(cohort, dateVar){
  if (!dateVar %in% colnames(cohort)) {
    cli::cli_abort(paste0(dateVar, " must be a date column in the cohort table"))
  }
}

checkDateRange<-function(dateRange){
  if(!"Date" %in% class(dateRange)){
    cli::cli_abort("dateRange is not a date")
  }
  if(length(dateRange) != 2){
    cli::cli_abort("dateRange must be length two")
  }
  if(dateRange[1]>dateRange[2]){
    cli::cli_abort("First date in dateRange cannot be after second")
  }
  return(invisible(dateRange))
}



