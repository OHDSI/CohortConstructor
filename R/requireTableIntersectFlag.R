#' Require cohort subjects are present in another clinical table
#'
#' @description
#' `requireTableIntersect()` filters a cohort table based on a requirement
#' that an individual is seen (or not seen) to have a record (or no records) in
#' a clinical table in some time window around an index date.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param tableName Name of the table to check for intersect.
#' @param window Window to consider events over.
#' @param intersections A range indicating number of intersections for
#' criteria to be fulfilled. If a single number is passed, the number of
#' intersections must match this.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param targetStartDate Date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate Date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of x.
#' @param name Name of the new cohort with the future observation restriction.
#'
#' @return Cohort table with only those in the other table kept (or those that
#' are not in the table if negate = TRUE)
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(drugExposure = TRUE)
#' cdm$cohort1 |>
#'   requireTableIntersect(tableName = "drug_exposure",
#'                             indexDate = "cohort_start_date",
#'                             window = c(-Inf, 0))
#' }
requireTableIntersect <- function(cohort,
                                  tableName,
                                  window,
                                  intersections = c(1, Inf),
                                  cohortId = NULL,
                                  indexDate = "cohort_start_date",
                                  targetStartDate = startDateColumn(tableName),
                                  targetEndDate = endDateColumn(tableName),
                                  censorDate = NULL,
                                  name = tableName(cohort)) {
  # checks
  name <- validateName(name)
  assertCharacter(tableName)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  intersections <- validateIntersections(intersections)

  lower_limit <- as.integer(intersections[[1]])
  upper_limit <- intersections[[2]]
  upper_limit[is.infinite(upper_limit)] <- as.integer(999999)
  upper_limit <- as.integer(upper_limit)

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

  if (length(tableName) > 1) {
    cli::cli_abort("Currently just one table supported.")
  }

  subsetCohort <- cohort %>%
    dplyr::select(dplyr::all_of(.env$cols)) %>%
    PatientProfiles::addTableIntersectCount(
      tableName = tableName,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect_table"
    )

  subsetCohort <- subsetCohort %>%
    dplyr::mutate(lower_limit = .env$lower_limit,
                  upper_limit = .env$upper_limit) |>
    dplyr::filter((.data$intersect_table >= .data$lower_limit &
                     .data$intersect_table <= .data$upper_limit) |
                    (!.data$cohort_definition_id %in% .env$cohortId)) %>%
    dplyr::select(cols)

  # attrition reason
  if(all(intersections == 0)){
    reason <- glue::glue("Not in table {tableName} between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else if (intersections[[1]] != intersections[[2]]){
    reason <- glue::glue("In table {tableName} between {window_start} & ",
                         "{window_end} days relative to {indexDate} between ",
                         "{intersections[[1]]} and {intersections[[2]]}")
  } else {
    reason <- glue::glue("In table {tableName} between {window_start} & ",
                         "{window_end} days relative to {indexDate} ",
                         "{intersections[[1]]} times")
  }
  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }

  x <- cohort %>%
    dplyr::inner_join(subsetCohort,
                      by = c(cols)) %>%
    dplyr::compute(name = name, temporary = FALSE) %>%
    omopgenerics::newCohortTable(.softValidation = TRUE) %>%
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(x)
}
