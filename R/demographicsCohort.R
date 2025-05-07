#' Create cohorts based on patient demographics
#'
#' @description
#' `demographicsCohort()` creates a cohort table based on patient
#' characteristics. If and when an individual satisfies all the criteria they
#' enter the cohort. When they stop satisfying any of the criteria their
#' cohort entry ends.
#'
#' @inheritParams cdmDoc
#' @inheritParams nameDoc
#' @inheritParams requireDemographicsDoc
#' @inheritParams softValidationDoc
#'
#' @return A cohort table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor()
#'
#' cohort <-  cdm |>
#'     demographicsCohort(name = "cohort3", ageRange = c(18,40), sex = "Male")
#'
#' attrition(cohort)
#'
#' # Can also create multiple demographic cohorts, and add minimum prior history requirements.
#'
#' cohort <- cdm |>
#'     demographicsCohort(name = "cohort4",
#'     ageRange = list(c(0, 19),c(20, 64),c(65, 150)),
#'     sex = c("Male", "Female", "Both"),
#'     minPriorObservation = 365)
#'
#'attrition(cohort)
#' }
demographicsCohort <- function(cdm,
                               name,
                               ageRange = NULL,
                               sex = NULL,
                               minPriorObservation = NULL,
                               .softValidation = TRUE) {
  # initial checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(cdm)

  cdm[[name]] <- cdm$observation_period |>
    dplyr::inner_join(cdm$person |>
                        dplyr::select("person_id") |>
                        dplyr::distinct(),
                      by = "person_id") |>
    dplyr::select(
      "subject_id" = "person_id",
      "cohort_start_date" = "observation_period_start_date",
      "cohort_end_date" = "observation_period_end_date"
    ) |>
    dplyr::mutate("cohort_definition_id" = 1L) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_demographicsCohort_") |>
    omopgenerics::newCohortTable(
      cohortSetRef = dplyr::tibble(
        "cohort_definition_id" = 1L,
        "cohort_name" = "demographics"
      ),
      cohortAttritionRef = NULL,
      cohortCodelistRef = NULL,
      .softValidation = TRUE
    )

  cdm[[name]] <- trimDemographics(
    cohort = cdm[[name]],
    cohortId = NULL,
    ageRange = ageRange,
    sex = sex,
    minPriorObservation = minPriorObservation,
    name = name,
    .softValidation = .softValidation
  )

  return(cdm[[name]])

}
