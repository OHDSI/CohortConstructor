
collectCohort <- function(cohort, id) {
  x <- cohort |>
    dplyr::filter(.data$cohort_definition_id == .env$id) |>
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") |>
    dplyr::collect() |>
    dplyr::arrange(subject_id, cohort_start_date, cohort_end_date)
  attr(x, "cohort_set") <- NULL
  attr(x, "cohort_attrition") <- NULL
  attr(x, "cohort_codelist") <- NULL
  return(x)
}
compareCohort <- function(cohort1, id1, cohort2, id2) {
  if (!identical(collectCohort(cohort1, id1), collectCohort(cohort2, id2))) {
    cli::cli_abort("cohorts are not equal")
  }
  return(invisible(TRUE))
}
