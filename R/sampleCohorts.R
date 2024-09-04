#' Sample a cohort table for a given number of individuals.
#'
#' @description
#' `sampleCohorts()` samples an existing cohort table for a given number of
#' people. All records of these individuals are preserved.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdSubsetDoc
#' @inheritParams nameDoc
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
                          n,
                          name = tableName(cohort)) {
  # checks
  cohort <- validateCohortTable(cohort, TRUE)
  cohortId <- validateCohortId(cohortId, settings(cohort))
  n <- validateN(n)
  name <- validateName(name)

  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::select("subject_id", "cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::slice_sample(n = n) |>
    dplyr::left_join(cohort, by = c("subject_id", "cohort_definition_id")) |>
    dplyr::union_all(cohort |>
      dplyr::filter(!(
        .data$cohort_definition_id %in% .env$cohortId
      ))) |>
    dplyr::ungroup() |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    omopgenerics::recordCohortAttrition(
      reason = paste0("Sample ", n, " individuals"),
      cohortId = cohortId
    ) |>
    dplyr::compute(name = name, temporary = FALSE)

  return(cohort)
}
