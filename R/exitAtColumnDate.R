#' Set cohort end date to the first of a set of column dates
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateColumns Date columns in the cohort table to consider.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param outOfObservation Strategy to handle end dates outside observation period:
#' "asis" to keep the original cohort end date, "drop" to drop those entries,
#' and "end" to set cohort end at observation end date.
#' @param na.rm If TRUE empty dates will not be used to compute the
#' end date, but if FALSE cohort end date will be NA when one of the
#' dates is empty.
#' @param name Name of the new cohort with the restriction.
#'
#' @return The cohort table.
#'
#'
#' @export
#'
#' @examples
#' library(PatientProfiles)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% exitAtObservationEnd()

exitAtFirstDate <- function(cohort,
                            dateColumns,
                            cohortId = NULL,
                            outOfObservation = "drop",
                            na.rm = FALSE,
                            name = omopgenerics::tableName(cohort)) {
  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertChoice(outOfObservation, c("asis", "drop", "end"), length = 1)
  assertLogical(na.rm, length = 1)
  validateCohortColumn(dateColumns, cohort, "Date")

  cohort |>
    # rand name to new end date to make sure it is not a column in the cohort
    dplyr::mutate(
      "new_end_date_1234567890" = pmin(!!!rlang::syms(dateColumns))
    )

}


#' Set cohort end date to the first of a set of column dates
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateColumns description
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param outOfObservation Strategy to handle end dates outside observation period:
#' "asis" to keep the original cohort end date, "drop" to drop those entries,
#' and "end" to set cohort end at observation end date.
#' @param na.rm If TRUE empty dates will not be used to compute the
#' end date, but if FALSE cohort end date will be NA when one of the
#' dates is empty.
#' @param name Name of the new cohort with the restriction.
#'
#' @return The cohort table.
#'
#'
#' @export
#'
#' @examples
#' library(PatientProfiles)
#' library(CohortConstructor)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% exitAtObservationEnd()

exitAtLastDate <- function(cohort,
                           dates,
                           cohortId = NULL,
                           outOfObservation = "drop",
                           na.rm = FALSE,
                           name = omopgenerics::tableName(cohort)) {

}
