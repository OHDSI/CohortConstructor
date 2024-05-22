#' Generate a cohort table using a subset of cohorts from another table.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to include. If NULL all cohorts will be
#' considered. Cohorts not included will be removed from the cohort set.
#' @param name Name of the new cohort with the demographic requirements.
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
                          name = tableName(cohort)) {
  # checks
  cohort <- validateCohortTable(cohort, TRUE)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  name <- validateName(name)

  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cohort) |> dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      cohortAttritionRef = attrition(cohort) |> dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      cohortCodelistRef = attr(cohort, "cohort_codelist") |> dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      .softValidation = TRUE
    )

  return(cohort)
}
