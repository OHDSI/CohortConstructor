#' Collapse cohort entries using a certain gap to concatenate records.
#'
#' @param cohort A cohort table
#' @param cohortId The cohort definition ids to subset, if NULL all cohort
#' definition ids are used.
#' @param gap Number of days to use when merging cohort entries.
#' @param name Name of the cohort table.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) check that
#' cohort end date is not before cohort start date, 2) check that there are no
#' missing values in required columns, 3) check that cohort duration is all
#' within observation period, and 4) check that there are no overlapping cohort
#' entries.
#'
#' @export
#'
#' @return A cohort table
#'
erafy <- function(cohort,
                  cohortId = NULL,
                  gap = 0,
                  name = tableName(cohort),
                  .softValidation = FALSE) {

  # input validation
  cohort <- validateCohortTable(cohort, dropExtraColumns = TRUE)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  gap <- validateGap(gap)
  assertLogical(.softValidation, length = 1)

  # restrict to cohort ids of interest
  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  if (gap > 0) {
    cohort <- cohort |> joinOverlap(gap = gap)
  }

  cohort <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = .softValidation) |>
    omopgenerics::recordCohortAttrition(
      reason = "Collapse cohort with a gap of {gap} days."
    )

  return(cohort)
}
