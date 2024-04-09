#' Require cohort subjects are present in another cohort
#'
#' @param x Cohort table.
#' @param targetCohortTable Name of the cohort that we want to check for overlap.
#' @param targetCohortId Vector of cohort definition ids to include.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param targetStartDate Date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate Date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param window Window to consider events over.
#' @param negate If set as TRUE, criteria will be applied as exclusion
#' rather than inclusion (i.e. require absence in another cohort).
#'
#' @return Cohort table with only those in the other cohort kept
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   requireCohortIntersectFlag(targetCohortTable = "cohort2",
#'                              targetCohortId = 1,
#'                              indexDate = "cohort_start_date",
#'                              window = c(-Inf, 0))
requireCohortIntersectFlag <- function(x,
                                       targetCohortTable,
                                       targetCohortId = NULL,
                                       indexDate = "cohort_start_date",
                                       targetStartDate = "cohort_start_date",
                                       targetEndDate = "cohort_end_date",
                                       window = list(c(0, Inf)),
                                       negate = FALSE){

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
targetCohortId <- CDMConnector::settings(cdm[[targetCohortTable]]) %>%
    dplyr::pull("cohort_definition_id")
}

if(length(targetCohortId) > 1){
  cli::cli_abort("Only one target cohort is currently supported")
}

target_name <- cdm[[targetCohortTable]] %>%
  omopgenerics::settings() %>%
  dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
  dplyr::pull("cohort_name")

subsetCohort <- x %>%
  dplyr::select(dplyr::all_of(.env$cols)) %>%
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = targetCohortTable,
    targetCohortId = targetCohortId,
    indexDate = indexDate,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    window = window,
    nameStyle = "intersect_cohort"
  )

if(isFALSE(negate)){
  subsetCohort <- subsetCohort %>%
    dplyr::filter(.data$intersect_cohort == 1) %>%
    dplyr::select(!"intersect_cohort")
} else {
  # ie require absence instead of presence
  subsetCohort <- subsetCohort %>%
    dplyr::filter(.data$intersect_cohort != 1) %>%
    dplyr::select(!"intersect_cohort")
}


x %>%
  dplyr::inner_join(subsetCohort,
                    by = c(cols)) %>%
  CDMConnector::recordCohortAttrition(reason =
                                        glue::glue("In cohort {target_name} between ",
                                                   "{window_start} & ",
                                                   "{window_end} days relative to ",
                                                   "{indexDate}"))

}



