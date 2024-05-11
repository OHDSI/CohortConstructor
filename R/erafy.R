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
    cl <- class(cohort)
    oldAttributes <- keepAttributes(cohort, cl)
    cohort <- cohort |> joinOverlap(gap = gap)
    # due to issue: https://github.com/darwin-eu-dev/omopgenerics/issues/256
    cohort <- restoreClass(cohort, cl)
    cohort <- restoreAttributes(cohort, oldAttributes)
  }

  cohort <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(
      reason = paste0("Collapse cohort with gap = ", gap, " days.")
    )

  return(cohort)
}

keepAttributes <- function(x, cl) {
  xx <- list(
    tbl_source = attr(x, "tbl_source"),
    tbl_name = attr(x, "tbl_name"),
    cdm_reference = attr(x, "cdm_reference")
  )
  if ("cohort_table" %in% cl) {
    xx[["cohort_set"]] <- attr(x, "cohort_set")
    xx[["cohort_attrition"]] <- attr(x, "cohort_attrition")
  }
  return(xx)
}
keepClass <- function(x) {
  removeClass(x = x, value = c(
    "cdm_table", "omop_table", "achilles_table", "cohort_table"
  ))
}
restoreAttributes <- function(x, at) {
  for (nm in names(at)) {
    if (!nm %in% names(attributes(x))) {
      attr(x, nm) <- at[[nm]]
    }
  }
  return(x)
}
restoreClass <- function(x, cl) {
  x <- addClass(x, "cdm_table")
  if ("cohort_table" %in% cl &
      "cohort_definition_id" %in% colnames(x)) {
    x <- addClass(x, "cohort_table")
  }
  return(x)
}
addClass <- function(x, value) {
  if (any(value %in% class(x))) x <- removeClass(x, value)
  base::class(x) <- c(value, base::class(x))
  return(x)
}
removeClass <- function(x, value) {
  base::class(x) <- base::class(x)[!(base::class(x) %in% value)]
  return(x)
}
