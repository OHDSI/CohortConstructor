#' Restrict cohort to first entry by index date
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Column name in cohort that contains the date to restrict on.
#' @param name Name of the new cohort with the restriction.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) check that
#' cohort end date is not before cohort start date, 2) check that there are no
#' missing values in required columns, 3) check that cohort duration is all
#' within observation period, and 4) check that there are no overlapping cohort
#' entries.
#'
#' @return A cohort table in a cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 <- requireIsFirstEntry(cdm$cohort1)
#' }
#'
requireIsFirstEntry <- function(cohort,
                                cohortId = NULL,
                                indexDate = "cohort_start_date",
                                name = tableName(cohort),
                                .softValidation = FALSE){

  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertLogical(.softValidation, length = 1)

  # restrict to first entry
  indexDateSym <- rlang::sym(indexDate)

  cohort <- cohort |>
    dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
    dplyr::filter(
      !!indexDateSym == min(!!indexDateSym, na.rm = TRUE) |
        (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = .softValidation) |>
    CDMConnector::recordCohortAttrition("Restricted to first entry", cohortId = cohortId)

  return(cohort)
}

#' Restrict cohort to last entry by index date
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Column name in cohort that contains the date to restrict on.
#' @param name Name of the new cohort with the restriction.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) check that
#' cohort end date is not before cohort start date, 2) check that there are no
#' missing values in required columns, 3) check that cohort duration is all
#' within observation period, and 4) check that there are no overlapping cohort
#' entries.
#'
#' @return A cohort table in a cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 <- requireIsLastEntry(cdm$cohort1)
#' }
#'
requireIsLastEntry <- function(cohort,
                               cohortId = NULL,
                               indexDate = "cohort_start_date",
                               name = tableName(cohort),
                               .softValidation = FALSE){

  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)

  # restrict to first entry
  indexDateSym <- rlang::sym(indexDate)

  cohort <- cohort |>
    dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
    dplyr::filter(
      !!indexDateSym == max(!!indexDateSym, na.rm = TRUE) |
        (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = .softValidation) |>
    CDMConnector::recordCohortAttrition("Restricted to last entry", cohortId = cohortId)

  return(cohort)
}
