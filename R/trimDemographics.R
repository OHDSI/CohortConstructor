#' Restrict cohort on patient demographics
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param ageRange A list of minimum and maximum age.
#' @param sex Can be "Both", "Male" or "Female". If one of the latter, only
#' those with that sex will be included.
#' @param minPriorObservation A minimum number of prior observation days in
#' the database.
#' @param minFutureObservation A minimum number of future observation days in
#' the database.
#' @param order Order of restrictions.
#' @param name Name of the new cohort with the demographic requirements.
#'
#' @return The cohort table with only records for individuals satisfying the
#' demographic requirements
#'
#' @export
#'
trimDemographics <- function(cohort,
                             cohortId = NULL,
                             ageRange = NULL,
                             sex = NULL,
                             minPriorObservation = NULL,
                             minFutureObservation = NULL,
                             order = c("sex", "age", "prior_observation", "future_observation"),
                             name = tableName(cohort)) {
  # initial validation
  cohort <- validateCohortTable(cohort)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  ageRange <- validateAgeRange(ageRange)
  sex <- validateSex(sex)
  minPriorObservation <- validateMinPriorObservation(minPriorObservation)
  minFutureObservation <- validateMinFutureObservation(minFutureObservation)
  order <- validateOrder(order)
  name <- validateName(name)

  originalCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  cohort <- originalCohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    )
  if (!is.null(ageRange)) {
    qAge <- datesAgeRange(ageRange)
    cohort <- cohort |>
      PatientProfiles::addDateOfBirth(name = "date_0") |>
      dplyr::mutate(!!!qAge)
  }


    PatientProfiles::addDemographics(indexDate = indexDate, age = F)


}

datesAgeRange <- function(ageRange) {
  qA <- list()
  values <- lapply(ageRange, function(x) {
    x[2] <- x[2] + 1
    return(x)
  }) |>
    unlist() |>
    unique()
  values <- values[!is.infinite(values)]
  values <- values[values != 0]
  for (val in values) {
    qA[[paste0("date_", val)]] <- glue::glue(
      "as.Date(local(CDMConnector::dateadd('date_of_birth', {val}, interval = 'year')))"
    )
  }
  return(qA)
}
