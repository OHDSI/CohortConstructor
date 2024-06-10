#' Generate a cohort table using a subset of cohorts from another table.
#'
#' @description
#' `subsetCohorts()` filters an existing cohort table, keeping only the records
#' from cohorts that are specified.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to include. If NULL all cohorts will be
#' considered. Cohorts not included will be removed from the cohort set.
#' @param minCohortCount the minimum count of a cohort to be included. Default
#' is 0, meaning all non-empty cohorts will be included. Cohorts not included
#' will be removed from the cohort set.
#' @param name Name of the new cohort with the demographic requirements.
#'
#' @return Cohort table with only cohorts in cohortId.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort1 |> subsetCohorts(cohortId = 1)
#' }
subsetCohorts <- function(cohort,
                          cohortId,
                          minCohortCount = 0,
                          name = tableName(cohort)) {
  # checks
  cohort <- validateCohortTable(cohort, TRUE)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  name <- validateName(name)
  minCohortCount <- validateN(minCohortCount)

  minId <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::tally() %>%
    dplyr::filter(.data$n >= .env$minCohortCount) %>%
    dplyr::pull("cohort_definition_id")

  cohortId <- intersect(cohortId, minId)

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
