
#' Filter cohorts to keep only records for those with a minimum amount of subjects
#'
#' @description
#' `requireMinimumCohortCount()` filters an existing cohort table, keeping only
#' records from cohorts with a minimum number of individuals
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to apply minimum cell count requirement.
#' If NULL it will be applied to all cohorts.
#' @param minCohortCount The minimum count of sbjects for a cohort to be
#' included.
#' @param name Name of the new cohort with the demographic requirements.
#'
#' @return Cohort table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort1 |>
#' requireMinimumCohortCount(minCohortCount = 5)
#' }
requireMinimumCohortCount <- function(cohort,
                                      cohortId = NULL,
                                      minCohortCount = 5,
                                      name = tableName(cohort)){

  cdm <- omopgenerics::cdmReference(cohort)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  name <- validateName(name)
  minCohortCount <- validateN(minCohortCount)

  cohortsToDrop <- cohortCount(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% cohortId,
                  .data$number_subjects < minCohortCount) |>
    dplyr::pull("cohort_definition_id")


  if(length(cohortsToDrop) > 0){
    cohort <- cohort |>
      dplyr::filter(!.data$cohort_definition_id %in% {{cohortsToDrop}})
  }

  cdm[[name]] <- cohort |>
    dplyr::compute(temporary = FALSE,
                   name = name) |>
    omopgenerics::recordCohortAttrition(
      reason = "Fewer than minimum cohort count of {minCohortCount}")

  cdm[[name]]
}
