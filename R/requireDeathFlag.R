#' Require cohort subjects' death at a certain time window
#'
#' @param x Cohort table.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of x.
#' @param window Window to consider events over.
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
#' library(PatientProfiles)
#' library(CDMConnector)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm <- insertTable(cdm, "death",
#'                    table = dplyr::tibble(
#'                      person_id = 1,
#'                      death_date = as.Date("2020-05-01"),
#'                      death_type_concept_id = NA))
#' cdm$cohort1 <- cdm$cohort1 %>% requireDeathFlag()
#' attrition(cdm$cohort1)


requireDeathFlag <- function(x,
                             indexDate = "cohort_start_date",
                             censorDate = NULL,
                             window = list(c(0, Inf)),
                             negate = FALSE,
                             name = omopgenerics::tableName(x)) {
  # checks
  name <- validateName(name)
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
    PatientProfiles::addDeathFlag(
      indexDate = indexDate,
      censorDate = censorDate,
      window = window,
      deathFlagName = "death"
    )

  if(isFALSE(negate)){
    subsetCohort <- subsetCohort %>%
      dplyr::filter(.data$death == 1) %>%
      dplyr::select(!"death")
    # attrition reason
    reason <- glue::glue("Death between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else {
    # ie require absence instead of presence
    subsetCohort <- subsetCohort %>%
      dplyr::filter(.data$death != 1) %>%
      dplyr::select(!"death")
    # attrition reason
    reason <- glue::glue("Alive between {window_start} & ",
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
