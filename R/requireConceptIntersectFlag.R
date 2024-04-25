#' Require cohort subjects to have events of a concept list
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param conceptSet Concept set list.
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
#' @return Cohort table with only those  with the events in the concept list
#' kept (or those without the event if nagate = TRUE)
#'
#' @export
#'
#' @examples
#' library(PatientProfiles)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm <- CDMConnector::insertTable(cdm, name = "concept",
#'                                  table = dplyr::tibble(
#'                                    "concept_id" = 1,
#'                                    "concept_name" = "my concept",
#'                                    "domain_id" = "Drug",
#'                                    "vocabulary_id" = NA,
#'                                    "concept_class_id" = NA,
#'                                    "concept_code" = NA,
#'                                    "valid_start_date" = NA,
#'                                    "valid_end_date" = NA
#'                                   ))
#' cdm$cohort2 <-  requireConceptIntersectFlag(
#'   cohort = cdm$cohort1,
#'   conceptSet = list(a = 1),
#'   window = c(-Inf, 0),
#'   name = "cohort2")
requireConceptIntersectFlag <- function(cohort,
                                        cohortId = NULL,
                                        conceptSet,
                                        indexDate = "cohort_start_date",
                                        targetStartDate = "event_start_date",
                                        targetEndDate = "event_end_date",
                                        censorDate = NULL,
                                        window = list(c(0, Inf)),
                                        negate = FALSE,
                                        name = omopgenerics::tableName(cohort)){
  # checks
  name <- validateName(name)
  assertLogical(negate, length = 1)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
  assertList(conceptSet)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)

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

  if (length(conceptSet) > 1) {
    cli::cli_abort("We currently suport 1 concept set.")
  }

  if (length(conceptSet) == 0) {
    cli::cli_inform(c("i" = "Empty codelist provided, returning input cohort"))
  } else {
    subsetCohort <- cohort %>%
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
        dplyr::filter(
          .data$intersect_concept == 1  |
            (!.data$cohort_definition_id %in% .env$cohortId)
        ) %>%
        dplyr::select(!"intersect_concept")
      # attrition reason
      reason <- glue::glue("Concept {names(conceptSet)} between {window_start} & ",
                           "{window_end} days relative to {indexDate}")
    } else {
      # ie require absence instead of presence
      subsetCohort <- subsetCohort %>%
        dplyr::filter(
          .data$intersect_concept != 1 |
            (!.data$cohort_definition_id %in% .env$cohortId)
        ) %>%
        dplyr::select(!"intersect_concept")
      # attrition reason
      reason <- glue::glue("Not in concept {names(conceptSet)} between {window_start} & ",
                           "{window_end} days relative to {indexDate}")
    }
    if (!is.null(censorDate)) {
      reason <- glue::glue("{reason}, censoring at {censorDate}")
    }
    x <- cohort %>%
      dplyr::inner_join(subsetCohort,
                        by = c(cols)) %>%
      dplyr::compute(name = name, temporary = FALSE) %>%
      CDMConnector::recordCohortAttrition(reason = reason, cohortId = cohortId)
  }

  return(x)
}
