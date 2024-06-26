#' Require cohort subjects have (or do not have) a death record
#'
#' @description
#' `requireDeathFlag()` filters a cohort table based on a requirement
#' that an individual is seen (or not seen) to have a death in some time
#' window around an index date.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param window Window to consider events over.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of x.
#' @param negate If set as TRUE, criteria will be applied as exclusion
#' rather than inclusion (i.e. require absence in another cohort).
#' @param name Name of the new cohort with the future observation restriction.
#'
#' @return Cohort table with only those with a death event kept (or without
#' if negate = TRUE)
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(death = TRUE)
#' cdm$cohort1 <- cdm$cohort1 |> requireDeathFlag(window = list(c(0, Inf)))
#' attrition(cdm$cohort1)
#' }
requireDeathFlag <- function(cohort,
                             window,
                             cohortId = NULL,
                             indexDate = "cohort_start_date",
                             censorDate = NULL,
                             negate = FALSE,
                             name = tableName(cohort)) {
  # checks
  name <- validateName(name)
  assertLogical(negate, length = 1)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
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

  subsetCohort <- cohort %>%
    dplyr::select(dplyr::all_of(.env$cols)) %>%
    PatientProfiles::addDeathFlag(
      indexDate = indexDate,
      censorDate = censorDate,
      window = window,
      deathFlagName = "death"
    )

  if(isFALSE(negate)){
    subsetCohort <- subsetCohort %>%
      dplyr::filter(
        .data$death == 1 |
          (!.data$cohort_definition_id %in% cohortId)
      ) %>%
      dplyr::select(!"death")
    # attrition reason
    reason <- glue::glue("Death between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else {
    # ie require absence instead of presence
    subsetCohort <- subsetCohort %>%
      dplyr::filter(
        .data$death != 1 |
          (!.data$cohort_definition_id %in% cohortId)
      ) %>%
      dplyr::select(!"death")
    # attrition reason
    reason <- glue::glue("Alive between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
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
