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
#' library(PatientProfiles)
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   addTableIntersectDate(
#'     tableName = "drug_exposure",
#'     nameStyle = "prior_drug",
#'     order = "last",
#'     window = c(-Inf, 0)
#'   ) |>
#'   addPriorObservation(priorObservationType = "date", name = "cohort1")
#'
#' cdm$cohort1 |>
#'   entryAtFirstDate(dateColumns = c("prior_drug", "prior_observation"))
#' }
entryAtFirstDate <- function(cohort,
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
#' library(PatientProfiles)
#'
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   addTableIntersectDate(
#'     tableName = "drug_exposure",
#'     nameStyle = "prior_drug",
#'     order = "last",
#'     window = c(-Inf, 0)
#'   ) |>
#'   addPriorObservation(priorObservationType = "date", name = "cohort1")
#'
#' cdm$cohort1 |>
#'   entryAtLastDate(dateColumns = c("prior_drug", "prior_observation"))
#' }
entryAtLastDate <- function(cohort,
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
    exit = FALSE,
    keepDateColumns = keepDateColumns,
    .softValidation = .softValidation
  )
}
