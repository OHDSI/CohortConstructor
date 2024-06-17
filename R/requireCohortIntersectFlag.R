#' Require cohort subjects are present (or absence) in another cohort
#'
#' @description
#' `requireCohortIntersect()` filters a cohort table based on a requirement
#' that an individual is seen (or not seen) in another cohort in some time
#' window around an index date.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param targetCohortTable Name of the cohort that we want to check for
#' intersect.
#' @param window Window to consider events over.
#' @param intersections A range indicating number of intersections for
#' criteria to be fulfilled. If a single number is passed, the number of
#' intersections must match this.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param targetCohortId Vector of cohort definition ids to include.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param targetStartDate Date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate Date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of x.
#' @param name Name of the new cohort with the future observation restriction.
#'
#' @return Cohort table with only those isatisfying the criteria kept
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   requireCohortIntersect(targetCohortTable = "cohort2",
#'                              targetCohortId = 1,
#'                              indexDate = "cohort_start_date",
#'                              window = c(-Inf, 0))
#' }
requireCohortIntersect <- function(cohort,
                                   targetCohortTable,
                                   window,
                                   intersections = c(1, Inf),
                                   cohortId = NULL,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   targetStartDate = "cohort_start_date",
                                   targetEndDate = "cohort_end_date",
                                   censorDate = NULL,
                                   name = tableName(cohort)) {
  # checks
  name <- validateName(name)
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

  if(is.list(window)){
    window_start <- window[[1]][1]
    window_end <- window[[1]][2]
  } else {
    window_start <- window[1]
    window_end <- window[2]
  }

  if(length(targetCohortTable) > 1){
    cli::cli_abort("Only one target cohort table is currently supported")
  }

  if(length(targetCohortId) > 1){
    cli::cli_abort("Only one target cohort is currently supported")
  }

  if(is.null(targetCohortId)){
    targetCohortId <- omopgenerics::settings(cdm[[targetCohortTable]]) %>%
      dplyr::pull("cohort_definition_id")
  }

  target_name <- cdm[[targetCohortTable]] %>%
    omopgenerics::settings() %>%
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
    dplyr::pull("cohort_name")

  subsetCohort <- cohort %>%
    dplyr::select(dplyr::all_of(.env$cols)) %>%
    PatientProfiles::addCohortIntersectCount(
      targetCohortTable = targetCohortTable,
      targetCohortId = targetCohortId,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect_cohort"
    )

  subsetCohort <- subsetCohort %>%
      dplyr::mutate(lower_limit = .env$lower_limit,
                    upper_limit = .env$upper_limit) |>
      dplyr::filter((.data$intersect_cohort >= .data$lower_limit &
                    .data$intersect_cohort <= .data$upper_limit) |
                    (!.data$cohort_definition_id %in% .env$cohortId)) %>%
      dplyr::select(cols)

  # attrition reason
  if(all(intersections == 0)){
    reason <- glue::glue("Not in cohort {target_name} between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else if (intersections[[1]] != intersections[[2]]){
    reason <- glue::glue("In cohort {target_name} between {window_start} & ",
                         "{window_end} days relative to {indexDate} between ",
                         "{intersections[[1]]} and {intersections[[2]]} times")
  } else {
    reason <- glue::glue("In cohort {target_name} between {window_start} & ",
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

