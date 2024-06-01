#' Restrict cohort to first entry
#'
#' @description
#' `requireIsFirstEntry()` filters cohort records, keeping only the first
#' cohort entry per person.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param indexDate Column name in cohort that contains the date to restrict on.
#' @param name Name of the new cohort with the restriction.
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
                                name = tableName(cohort)) {

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
      !!indexDateSym == min(!!indexDateSym, na.rm = TRUE) |
        (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition("Restricted to first entry", cohortId = cohortId)

  return(cohort)
}

#' Restrict cohort to last entry per person
#'
#' @description
#' `requireIsLastEntry()` filters cohort records, keeping only the last
#' cohort entry per person.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param indexDate Column name in cohort that contains the date to restrict on.
#' @param name Name of the new cohort with the restriction.
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
                               name = tableName(cohort)){

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
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition("Restricted to last entry", cohortId = cohortId)

  return(cohort)
}
