#' Generate a cohort table using a subset of cohorts from another table.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL all
#' cohort will be selected.
#' @param name Name of the new cohort with the demographic requirements.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) check that
#' cohort end date is not before cohort start date, 2) check that there are no
#' missing values in required columns, 3) check that cohort duration is all
#' within observation period, and 4) check that there are no overlapping cohort
#' entries.
#'
#' @return Cohort table with only cohorts in cohortId.
#'
#' @export
#'
#' @examples
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort1 |> subsetCohorts(cohortId = 1)
#'
subsetCohorts <- function(cohort,
                          cohortId,
                          name = tableName(cohort),
                          .softValidation = FALSE) {
  # checks
  cohort <- validateCohortTable(cohort, TRUE)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  name <- validateName(name)
  assertLogical(.softValidation, length = 1)

  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cohort) |> dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      cohortAttritionRef = attrition(cohort) |> dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      cohortCodelistRef = attr(cohort, "cohort_codelist") |> dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      .softValidation = .softValidation
    )

  return(cohort)
}
