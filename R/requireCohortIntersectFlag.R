

#' Require cohort subjects are present in another cohort
#'
#' @param x Cohort table
#' @param targetCohortTable name of the cohort that we want to check for overlap
#' @param targetCohortId vector of cohort definition ids to include
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param window window to consider events of
#'
#' @return Cohort table with only those in the other cohort kept
#' @export
#'
#' @examples
requireCohortIntersectFlag <- function(x,
                                       targetCohortTable,
                                       targetCohortId = NULL,
                                       indexDate = "cohort_start_date",
                                       targetStartDate = "cohort_start_date",
                                       targetEndDate = "cohort_end_date",
                                       window = list(c(0, Inf))){

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

cdm <- attr(x, "cdm_reference")

if(is.null(cdm[[targetCohortTable]])){
  cli::cli_abort("targetCohortTable not found in cdm reference")
}

if(is.null(targetCohortId)){
targetCohortId <- CDMConnector::cohortSet(cdm[[targetCohortTable]]) %>%
    dplyr::pull("cohort_definition_id")
}

if(length(targetCohortId) > 1){
  cli::cli_abort("Only one target cohort is currently supported")
}

target_name <- CDMConnector::cohort_set(cdm[[targetCohortTable]]) %>%
  dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
  dplyr::pull("cohort_name")

subsetCohort <- x %>%
  dplyr::select(dplyr::all_of(.env$cols)) %>%
  PatientProfiles::addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = targetCohortTable,
    targetCohortId = targetCohortId,
    indexDate = indexDate,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    window = window,
    nameStyle = "intersect_cohort"
  ) %>%
  dplyr::filter(.data$intersect_cohort == 1) %>%
  dplyr::select(!"intersect_cohort")

x %>%
  dplyr::inner_join(subsetCohort,
                    by = c(cols)) %>%
  CDMConnector::recordCohortAttrition(reason =
                                        glue::glue("In cohort {target_name} between ",
                                                   "{window_start} and ",
                                                   "{window_end} days relative to ",
                                                   "{indexDate}"))

}


