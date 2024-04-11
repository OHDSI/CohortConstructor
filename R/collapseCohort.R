#' Collapse a cohort using a certain gap to join records.
#'
#' @param cohort A cohort_table object.
#' @param cohortId The cohort definition ids to subset, if NULL all cohort
#' definition ids are used.
#' @param gap number of days to join consecutive records.
#' @param name Name of the resultant cohort.
#'
#' @export
#'
#' @return A cohort_table object.
#'
collapseCohort <- function(cohort,
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
    cohort <- cohort |> collapseGap(gap = gap)
  }

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(
      reason = paste0("Collapse cohort with gap = ", gap, " days.")
    )

  return(cohort)
}
