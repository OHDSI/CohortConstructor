#' Create cohorts based on patient demographics
#'
#' @param cdm A cdm reference.
#' @param name Name of the new cohort table
#' @param ageRange A list of vectors specifying minimum and maximum age.
#' @param sex Can be "Both", "Male" or "Female".
#' @param minPriorObservation A minimum number of prior observation days in
#' the database.
#' @param minFutureObservation A minimum number of future observation days in
#' the database.
#'
#' @return A cohort table
#'
#' @export
#'
demographicsCohort <- function(cdm,
                               name,
                               ageRange = NULL,
                               sex = NULL,
                               minPriorObservation = NULL,
                               minFutureObservation = NULL) {
  # initial checks
  cdm <- validateCdm(cdm)
  name <- validateName(name)

  cdm[[name]] <- cdm$observation_period |>
    dplyr::inner_join(
      cdm$person |> dplyr::select("person_id") |> dplyr::distinct(),
      by = "person_id"
    ) |>
    dplyr::select(
      "subject_id" = "person_id",
      "cohort_start_date" = "observation_period_start_date",
      "cohort_end_date" = "observation_period_end_date"
    ) |>
    dplyr::mutate("cohort_definition_id" = 1) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = dplyr::tibble(
        "cohort_definition_id" = 1, "cohort_name" = "demographics"
      ),
      cohortAttritionRef = NULL,
      cohortCodelistRef = NULL
    )

  cdm[[name]] <- trimDemographics(
    cohort = cdm[[name]],
    cohortId = NULL,
    ageRange = ageRange,
    sex = sex,
    minPriorObservation = minPriorObservation,
    minFutureObservation = minFutureObservation,
    name = name
  )

  return(cdm[[name]])
}
