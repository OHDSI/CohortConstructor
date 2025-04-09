#' Update cohort start date to be the first date from of a set of column dates
#'
#' @description
#' `entryAtFirstDate()` resets cohort start date based on a set of specified
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
#' }
entryAtFirstDate <- function(cohort,
                             dateColumns,
                             cohortId = NULL,
                             returnReason = TRUE,
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
    exit = FALSE,
    keepDateColumns = keepDateColumns,
    .softValidation = .softValidation
  )
}


#' Set cohort start date to the last of a set of column dates
#'
#' @description
#' `entryAtLastDate()` resets cohort end date based on a set of specified
#' column dates. The last date is chosen.
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
#' }
entryAtLastDate <- function(cohort,
                            dateColumns,
                            cohortId = NULL,
                            returnReason = TRUE,
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
    exit = FALSE,
    keepDateColumns = keepDateColumns,
    .softValidation = .softValidation
  )
}
