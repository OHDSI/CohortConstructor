#' Generate a cohort table keeping a subset of cohorts.
#'
#' @description
#' `subsetCohorts()` filters an existing cohort table, keeping only the records
#' from cohorts that are specified.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdSubsetDoc
#' @inheritParams nameDoc
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
                          name = tableName(cohort)) {
  # checks
  cohort <- validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  cohortId <- validateCohortId(cohortId, settings(cohort))
  name <- validateName(name)

  cdm[[name]] <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::compute(name = name, temporary = FALSE)

  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cohort) |>
        dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      cohortAttritionRef = attrition(cohort) |>
        dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      cohortCodelistRef = attr(cohort, "cohort_codelist") |>
        dplyr::filter(.data$cohort_definition_id %in% .env$cohortId),
      .softValidation = TRUE
    )

  return(cdm[[name]])
}
