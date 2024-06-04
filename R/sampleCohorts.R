#' Sample a cohort table for a given number of individuals.
#'
#' @description
#' `sampleCohorts()` samples an existing cohort table for a given number of
#' people. All records of these individuals are preserved.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to include. If NULL all cohorts will be
#' considered. Cohorts not included will not be sampled.
#' @param n Number of people to be sampled for each included cohort.
#'
#' @return Cohort table with the specified cohorts sampled.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort2 |> sampleCohorts(cohortId = 1, n = 10)
#' }
sampleCohorts <- function(cohort,
                          cohortId = NULL,
                          n) {
  # checks
  cohort <- validateCohortTable(cohort, TRUE)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  n <- validateN(n)

  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::select("subject_id", "cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::slice_sample(n = n) |>
    dplyr::left_join(cohort,
                     by = c("subject_id", "cohort_definition_id")) |>
    dplyr::union_all(cohort |>
                       dplyr::filter(!(.data$cohort_definition_id %in% .env$cohortId))) |>
    dplyr::ungroup() |>
    omopgenerics::recordCohortAttrition(
      reason = paste0("Sample ",n," individuals"),
      cohortId = cohortId
      ) |>
    dplyr::compute(name = tableName(cohort), temporary = FALSE)

  return(cohort)
}
