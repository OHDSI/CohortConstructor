#' Collapse cohort entries using a certain gap to concatenate records.
#'
#' @param cohort A cohort table
#' @param cohortId The cohort definition ids to subset, if NULL all cohort
#' definition ids are used.
#' @param gap Number of days to use when merging cohort entries.
#' @param name Name of the cohort table.
#'
#' @export
#'
#' @return A cohort table
#'
erafy <- function(cohort,
                  cohortId = NULL,
                  gap = 0,
                  name = tableName(cohort)) {
  # input validation
  cohort <- validateCohortTable(cohort)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  gap <- validateGap(gap)

  # warning
  extraColumns <- colnames(cohort)
  extraColumns <- extraColumns[!extraColumns %in% c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  )]
  if (length(extraColumns) > 0) {
    cli::cli_inform(c(
      "!" = "Extra columns are not supported in this function, the following
      columns will be dropped: {paste0(extraColumns, collapse = ', ')}"
    ))
  }

  # restrict to cohort ids of interest
  cohort <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  if (gap > 0) {
    # cl <- class(cohort)
    # oldAttributes <- keepAttributes(cohort, cl)
    cohort <- cohort |> joinOverlap(gap = gap)
    # due to issue: https://github.com/darwin-eu-dev/omopgenerics/issues/256
    # cohort <- restoreClass(cohort, cl)
    # cohort <- restoreAttributes(cohort, oldAttributes)
  }

  cohort <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(
      reason = "Collapse cohort with a gap of {gap} days."
    )

  return(cohort)
}
