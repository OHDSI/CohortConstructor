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
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 |>
#'   subsetCohorts(cohortId = 1)
#' }
#' }
subsetCohorts <- function(cohort,
                          cohortId,
                          name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning empty cohort as `cohortId` is not valid.")
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  cdm[[name]] <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_subsetCohorts_filter_")

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

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cdm[[name]])
}
