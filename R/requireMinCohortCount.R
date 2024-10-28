
#' Filter cohorts to keep only records for those with a minimum amount of subjects
#'
#' @description
#' `requireMinCohortCount()` filters an existing cohort table, keeping only
#' records from cohorts with a minimum number of individuals
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param minCohortCount The minimum count of sbjects for a cohort to be
#' included.
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
#' requireMinCohortCount(5)
#' }
requireMinCohortCount <- function(cohort,
                                  minCohortCount,
                                  cohortId = NULL,
                                  name = tableName(cohort)){
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- validateCohortId(cohortId, settings(cohort))
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
    dplyr::compute(temporary = FALSE, name = name) |>
    omopgenerics::recordCohortAttrition(
      reason = "Fewer than minimum cohort count of {minCohortCount}",
      cohortId = cohortsToDrop
      )

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  cdm[[name]]
}
