#' Set cohort start date to the first of a set of column dates
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateColumns Date columns in the cohort table to consider.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param returnReason If TRUE it will return a column stating which column in
#' `dateColumns` is used as a new cohort end date.
#' @param name Name of the new cohort with the restriction.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) a check that
#' cohort end date is not before cohort start date, 2) a check that there are no
#' missing values in required columns, 3) a check that cohort duration is all
#' within observation period, and 4) that there are no overlapping cohort
#' entries
#'
#' @return The cohort table.
#'
#'
#' @export
#'
#' @examples
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(tables = list(
#' "cohort" = dplyr::tibble(
#'   cohort_definition_id = 1,
#'   subject_id = c(1, 2, 3, 4),
#'   cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "2000-12-09")),
#'   cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "2002-12-09")),
#'   date_1 = as.Date(c("2001-08-01", "2001-01-01", "2015-01-15", "2002-12-09")),
#'   date_2 = as.Date(c("2001-08-01", NA, "2015-02-14", "2002-12-09"))
#' )
#' ))
#' cdm$cohort |> entryAtLastDate(dateColumns = c("date_1", "date_2"))

entryAtFirstDate <- function(cohort,
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
    exit = FALSE,
    .softValidation = .softValidation
  )
}


#' Set cohort start date to the last of a set of column dates
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateColumns description
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param returnReason If TRUE it will return a column stating which column in
#' `dateColumns` is used as a new cohort end date. description
#' @param name Name of the new cohort with the restriction.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) a check that
#' cohort end date is not before cohort start date, 2) a check that there are no
#' missing values in required columns, 3) a check that cohort duration is all
#' within observation period, and 4) that there are no overlapping cohort
#' entries
#'
#' @return The cohort table.
#'
#'
#' @export
#'
#' @examples
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(tables = list(
#' "cohort" = dplyr::tibble(
#'   cohort_definition_id = 1,
#'   subject_id = c(1, 2, 3, 4),
#'   cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "2000-12-09")),
#'   cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "2002-12-09")),
#'   date_1 = as.Date(c("2001-08-01", "2001-01-01", "2015-01-15", "2002-12-09")),
#'   date_2 = as.Date(c("2001-08-01", NA, "2015-02-14", "2002-12-09"))
#' )
#' ))
#' cdm$cohort |> entryAtLastDate(dateColumns = c("date_1", "date_2"))

entryAtLastDate <- function(cohort,
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
    exit = FALSE,
    .softValidation = .softValidation
  )
}

