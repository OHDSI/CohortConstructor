

#' Restrict cohort on patient demographics
#'
#' @param cohort A cohort table in a cdm reference
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param ageRange A list of minimum and maximum age
#' @param sex Can be "Both", "Male" or "Female". If one of the latter, only
#' those with that sex will be included.
#' @param minPriorObservation A mimimum number of prior observation days in
#' the database.
#' @param minFutureObservation A minimum number of future observation days in
#' the database.
#'
#' @return
#' @export
#'
#' @examples
requireDemographics <- function(cohort,
                                indexDate = "cohort_start_date",
                                ageRange = list(c(0, 150)),
                                sex = c("Both"),
                                minPriorObservation = 0,
                                minFutureObservation = 0){

  cohort <-  demographicsFilter(cohort = cohort,
                     indexDate = indexDate,
                     ageRange = ageRange,
                     sex = sex,
                     minPriorObservation = minPriorObservation,
                     minFutureObservation = minFutureObservation)

  cohort <- cohort %>%
    CDMConnector::recordCohortAttrition(reason = "Demographic requirements")

  cohort
}

#' Restrict cohort on age
#'
#' @param cohort A cohort table in a cdm reference
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param ageRange A list of minimum and maximum age
#'
#' @return
#' @export
#'
#' @examples
requireAge <- function(cohort,
                       indexDate = "cohort_start_date",
                       ageRange = list(c(0, 150))){

  cohort <-  demographicsFilter(cohort = cohort,
                                indexDate = indexDate,
                                ageRange = ageRange,
                                sex = "Both",
                                minPriorObservation = 0,
                                minFutureObservation = 0)

  cohort <- cohort %>%
    CDMConnector::recordCohortAttrition(reason =
                                          glue::glue("Age requirement: {ageRange[[1]][1]} to {ageRange[[1]][2]}"))

  cohort

}

#' Restrict cohort on sex
#'
#' @param cohort A cohort table in a cdm reference
#' @param sex Can be "Both", "Male" or "Female". If one of the latter, only
#' those with that sex will be included.
#'
#' @return
#' @export
#'
#' @examples
requireSex <- function(cohort,
                       sex = c("Both")){

  cohort <-  demographicsFilter(cohort = cohort,
                                indexDate = "cohort_start_date",
                                ageRange = list(c(0, 150)),
                                sex = sex,
                                minPriorObservation = 0,
                                minFutureObservation = 0)

  cohort <- cohort %>%
    CDMConnector::recordCohortAttrition(reason =
        glue::glue("Sex requirement: {sex}"))


  cohort

}

#' Restrict cohort on prior observation
#'
#' @param cohort A cohort table in a cdm reference
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param minPriorObservation A mimimum number of prior observation days in
#' the database.
#'
#' @return
#' @export
#'
#' @examples
requirePriorObservation <- function(cohort,
                                    indexDate = "cohort_start_date",
                                    minPriorObservation = 0){

  cohort <-  demographicsFilter(cohort = cohort,
                                indexDate = indexDate,
                                ageRange = list(c(0, 150)),
                                sex = "Both",
                                minPriorObservation = minPriorObservation,
                                minFutureObservation = 0)

  cohort <- cohort %>%
    CDMConnector::recordCohortAttrition(reason =
        glue::glue("Prior observation requirement: {minPriorObservation} days"))

  cohort

}

#' Restrict cohort on future observation
#'
#' @param cohort A cohort table in a cdm reference
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param minFutureObservation A minimum number of future observation days in
#' the database.
#'
#' @return
#' @export
#'
#' @examples
requireFutureObservation <- function(cohort,
                                     indexDate = "cohort_start_date",
                                     minFutureObservation = 0){

  cohort <-  demographicsFilter(cohort = cohort,
                                indexDate = indexDate,
                                ageRange = list(c(0, 150)),
                                sex = "Both",
                                minPriorObservation = 0,
                                minFutureObservation = minFutureObservation)

  cohort <- cohort %>%
    CDMConnector::recordCohortAttrition(reason =
    glue::glue("Future observation requirement: {minFutureObservation} days"))

  cohort

}

demographicsFilter <- function(cohort,
                   indexDate,
                   ageRange,
                   sex,
                   minPriorObservation,
                   minFutureObservation){

  cdm <- attr(cohort, "cdm_reference")

  # validate inputs
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cohort must be part of a cdm reference")
  }
  if(!"GeneratedCohortSet" %in% class(cohort) ||
     !all(c("cohort_definition_id", "subject_id",
            "cohort_start_date", "cohort_end_date") %in%
          colnames(cohort))){
    cli::cli_abort("cohort must be a GeneratedCohortSet")
  }
  if(!indexDate %in% colnames(cohort)){
    cli::cli_abort("indexDate must be a date column in the cohort table")
  }

  if(!is.list(ageRange)){
    cli::cli_abort("ageRange must be a list")
  }
  if(length(ageRange[[1]]) != 2 ||
     !is.numeric(ageRange[[1]]) ||
     !ageRange[[1]][2] >= ageRange[[1]][1] ||
     !ageRange[[1]][1]>=0){
    cli::cli_abort("ageRange only contain a vector of length two, with the
                   second number greater or equal to the first")
  }
  if(length(ageRange) != 1){
    cli::cli_abort("Only a single ageRange is currently supported")
  }
  if(!all(sex %in% c("Both", "Male", "Female"))){
    cli::cli_abort("sex must be Both, Male, or Female")
  }
  if(length(sex) != 1){
    cli::cli_abort("Only a single sex option is currently supported")
  }
  if(!is.numeric(minPriorObservation) ||
     length(minPriorObservation) != 1 ||
     !minPriorObservation >= 0){
    cli::cli_abort("minPriorObservation must be a positive number")
  }
  if(!is.numeric(minFutureObservation) ||
     length(minFutureObservation) != 1 ||
     !minFutureObservation >= 0){
    cli::cli_abort("minFutureObservation must be a positive number")
  }

  minAge <- ageRange[[1]][1]
  maxAge <- ageRange[[1]][2]
  if(sex == "Both"){
    sex <- c("Male", "Female")
  }

  # because the cohort table passed to the function might have extra columns
  # that would conflict with ones we'll add, we'll take the core table first
  # join later

  working_cohort <- cohort %>%
    dplyr::select(c("cohort_definition_id", "subject_id",
                    "cohort_start_date", "cohort_end_date",
                    indexDate)) %>%
    PatientProfiles::addDemographics(indexDate = indexDate) %>%
    dplyr::filter(.data$age >= .env$minAge,
                  .data$age <= .env$maxAge,
                  .data$sex %in% .env$sex,
                  .data$prior_observation >= .env$minPriorObservation,
                  .data$future_observation >= .env$minFutureObservation)

  cohort <- cohort %>%
    dplyr::inner_join(working_cohort  %>%
                        dplyr::select(c("cohort_definition_id", "subject_id",
                                        "cohort_start_date", "cohort_end_date")),
                      by = c("cohort_definition_id", "subject_id",
                             "cohort_start_date", "cohort_end_date"))
  cohort
}