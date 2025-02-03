#' Copy a cohort table
#'
#' @description
#' `copyCohorts()` copies an existing cohort table to a new location.
#'
#' @inheritParams cohortDoc
#' @inheritParams nameDoc
#' @inheritParams cohortIdSubsetDoc
#'
#' @return A new cohort table containing cohorts from the original cohort table.
#' @export
#'
#' @examples
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort3 <- copyCohorts(cdm$cohort1, name = "cohort3")
copyCohorts <- function(cohort, name, cohortId = NULL) {
  omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  if(name == omopgenerics::tableName(cohort)){
    cli::cli_abort("Cohort cannot be copied to the same table. Please provide a different name.")
  }
  omopgenerics::validateNameArgument(name, cdm = cdm, validation = "warning")
  cohorts_to_keep <- omopgenerics::validateCohortIdArgument(cohortId, cohort = cohort)

  if (length(unique(cohorts_to_keep)) == length(settings(cohort) |>
    dplyr::pull("cohort_definition_id"))) {
    # copy all cohorts
    return(cohort |>
      dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE))
  } else {
    cohort |>
      CohortConstructor::subsetCohorts(cohortId = cohorts_to_keep, name = name)
  }
}
