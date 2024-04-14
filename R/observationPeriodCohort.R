#' Create the observation period cohort
#'
#' @param cdm A cdm_reference.
#' @param name Name of the new cohort_table object.
#' @param cohortName Name of the observation period cohort
#'
#' @return The cohort with the observation period
#'
# TO BE EXPORTED WHEN TESTS ARE ADDED
#' @noRd
#'
observationPeriodCohort <- function(cdm,
                                    name,
                                    cohortName = "observation_period") {
  # initial checks
  cdm <- validateCdm(cdm)
  name <- validateName(name)
  cohortName <- assertCharacter(cohortName, length = 1)

  cdm[[name]] <- cdm$observation_period |>
    dplyr::select(
      "subject_id" = "person_id",
      "cohort_start_date" = "observation_period_start_date",
      "cohort_end_date" = "observation_period_end_date"
    ) |>
    dplyr::mutate("cohort_definition_id" = 1) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = dplyr::tibble(
        "cohort_definition_id" = 1, "cohort_name" = cohortName
      ),
      cohortAttritionRef = NULL,
      cohortCodelistRef = NULL
    )

  return(cdm[[name]])
}
