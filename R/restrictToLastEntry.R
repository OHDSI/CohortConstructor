#' Restrict cohort to first entry by index date
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Column name in cohort that contains the date to restrict on.
#' @param name Name of the new cohort with the restriction.
#' @return A cohort table in a cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 <- restrictToFirstEntry(cdm$cohort1)
#' }
#'
restrictToLastEntry <- function(cohort,
                                 cohortId = NULL,
                                 indexDate = "cohort_start_date",
                                 name = omopgenerics::tableName(cohort)){

  # checks
  assertCharacter(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateIndexDate(indexDate, cohort)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)

  # restrict to first entry
  indexDateSym <- rlang::sym(indexDate)

  if (all(ids %in% cohortId)) {
    cohort <- cohort |>
      dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
      dplyr::filter(!!indexDateSym == min(!!indexDateSym, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::newCohortTable() |>
      CDMConnector::recordCohortAttrition("Restricted to first entry")
  } else {
    cohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
      dplyr::filter(!!indexDateSym == min(!!indexDateSym, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::union_all(
        cohort |>
          dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId)
      ) |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::newCohortTable() |>
      CDMConnector::recordCohortAttrition("Restricted to first entry", cohortId = cohortId)
  }

  return(cohort)
}
