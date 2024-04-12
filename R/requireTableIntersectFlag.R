#' Require cohort subjects are present in another table
#'
#' @param x Cohort table.
#' @param tableName Name of the table to check for intersect.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param targetStartDate Date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate Date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of x.
#' @param window Window to consider events over.
#' @param negate If set as TRUE, criteria will be applied as exclusion
#' rather than inclusion (i.e. require absence in another cohort).
#' @param name Name of the new cohort with the future observation restriction.
#'
#' @return Cohort table with only those in the other table kept (or those that
#' are not in the table if negate = TRUE)
#'
#' @export
#'
#' @examples
#' library(PatientProfiles)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   requireTableIntersectFlag(tableName = "drug_exposure",
#'                             indexDate = "cohort_start_date",
#'                             window = c(-Inf, 0))
requireTableIntersectFlag <- function(x,
                                      tableName,
                                      indexDate = "cohort_start_date",
                                      targetStartDate = startDateColumn(tableName),
                                      targetEndDate = endDateColumn(tableName),
                                      censorDate = NULL,
                                      window = list(c(0, Inf)),
                                      negate = FALSE,
                                      name = omopgenerics::tableName(x)){
  # checks
  assertCharacter(name, length = 1)
  assertLogical(negate, length = 1)
  assertCharacter(tableName)
  validateCohortTable(x)
  cdm <- omopgenerics::cdmReference(x)
  validateCDM(cdm)
  validateIndexDate(indexDate, x)

  cols <- unique(c("cohort_definition_id", "subject_id",
                   "cohort_start_date", "cohort_end_date",
                   indexDate))

  if (is.list(window)) {
    window_start <- window[[1]][1]
    window_end <- window[[1]][2]
  } else {
    window_start <- window[1]
    window_end <- window[2]
  }

  cdm <- omopgenerics::cdmReference(x)

  if (is.null(cdm[[tableName]])) {
    cli::cli_abort("{tableName} not found in cdm reference")
  }

  if (length(tableName) > 1) {
    cli::cli_abort("Currently just one table supported.")
  }

  subsetCohort <- x %>%
    dplyr::select(dplyr::all_of(.env$cols)) %>%
    PatientProfiles::addTableIntersectFlag(
      tableName = tableName,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect_table"
    )

  if (isFALSE(negate)) {
    subsetCohort <- subsetCohort %>%
      dplyr::filter(.data$intersect_table == 1) %>%
      dplyr::select(!"intersect_table")
    # attrition reason
    reason <- glue::glue("In table {tableName} between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else {
    # ie require absence instead of presence
    subsetCohort <- subsetCohort %>%
      dplyr::filter(.data$intersect_table != 1) %>%
      dplyr::select(!"intersect_table")
    # attrition reason
    reason <- glue::glue("Not in table {tableName} between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  }


  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }

  x <- x %>%
    dplyr::inner_join(subsetCohort,
                      by = c(cols)) %>%
    dplyr::compute(name = name, temporary = FALSE) %>%
    CDMConnector::recordCohortAttrition(reason = reason)

  return(x)
}
