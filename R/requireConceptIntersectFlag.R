#' Require cohort subjects are present in another cohort
#'
#' @param x Cohort table.
#' @param targetTable Name of the table that we want to check for intersect.
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
requireConceptIntersectFlag <- function(x,
                                        conceptSet,
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
  validateCohortTable(x)
  cdm <- omopgenerics::cdmReference(x)
  validateCDM(cdm)
  validateIndexDate(indexDate, x)

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

  cdm <- omopgenerics::cdmReference(x)

  subsetCohort <- x %>%
    dplyr::select(dplyr::all_of(.env$cols)) %>%
    PatientProfiles::addConceptIntersectFlag(
      conceptSet = conceptSet,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect_concept"
    )



  if(isFALSE(negate)){
    subsetCohort <- subsetCohort %>%
      dplyr::filter(.data$intersect_concept == 1) %>%
      dplyr::select(!"intersect_concept")
    # attrition reason
    reason <- glue::glue("Concept {conceptSet} between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else {
    # ie require absence instead of presence
    subsetCohort <- subsetCohort %>%
      dplyr::filter(.data$intersect_concept != 1) %>%
      dplyr::select(!"intersect_concept")
    # attrition reason
    reason <- glue::glue("Not in concept {conceptSet} between {window_start} & ",
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
