#' Restrict cohort on patient demographics
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param ageRange A list of minimum and maximum age.
#' @param sex Can be "Both", "Male" or "Female". If one of the latter, only
#' those with that sex will be included.
#' @param minPriorObservation A minimum number of prior observation days in
#' the database.
#' @param minFutureObservation A minimum number of future observation days in
#' the database.
#' @param name Name of the new cohort with the demographic requirements.
#'
#' @return The cohort table with only records for individuals satisfying the
#' demographic requirements
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   requireDemographics(indexDate = "cohort_start_date",
#'                       ageRange = list(c(18, 65)),
#'                       sex = "Female",
#'                       minPriorObservation = 365)
#'
requireDemographics <- function(cohort,
                                cohortId = NULL,
                                indexDate = "cohort_start_date",
                                ageRange = list(c(0, 150)),
                                sex = c("Both"),
                                minPriorObservation = 0,
                                minFutureObservation = 0,
                                name = omopgenerics::tableName(cohort)) {

  cohort <- demographicsFilter(
    cohort = cohort,
    cohortId = cohortId,
    indexDate = indexDate,
    ageRange = ageRange,
    sex = sex,
    minPriorObservation = minPriorObservation,
    minFutureObservation = minFutureObservation,
    name = name,
    attritionAge = TRUE,
    attritionSex = TRUE,
    attritionPriorObservation = TRUE,
    attritionFutureObservation = TRUE
  )

  return(cohort)
}

#' Restrict cohort on age
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param ageRange A list of minimum and maximum age.
#' @param name Name of the new cohort with the age requirement.
#'
#' @return The cohort table with only records for individuals satisfying the
#' age requirement
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   requireAge(indexDate = "cohort_start_date",
#'              ageRange = list(c(18, 65)))
requireAge <- function(cohort,
                       cohortId = NULL,
                       ageRange = list(c(0, 150)),
                       indexDate = "cohort_start_date",
                       name = omopgenerics::tableName(cohort)) {

  cohort <- demographicsFilter(
    cohort = cohort,
    cohortId = cohortId,
    indexDate = indexDate,
    ageRange = ageRange,
    sex = "Both",
    minPriorObservation = 0,
    minFutureObservation = 0,
    name = name,
    attritionAge = TRUE,
    attritionSex = FALSE,
    attritionPriorObservation = FALSE,
    attritionFutureObservation = FALSE
  )

  return(cohort)
}

#' Restrict cohort on sex
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param sex Can be "Both", "Male" or "Female". If one of the latter, only
#' those with that sex will be included.
#' @param name Name of the new cohort with the sex requirements.
#'
#' @return The cohort table with only records for individuals satisfying the
#' sex requirement
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   requireSex(sex = "Female")
requireSex <- function(cohort,
                       cohortId = NULL,
                       sex = c("Both"),
                       name = omopgenerics::tableName(cohort)) {

  cohort <- demographicsFilter(
    cohort = cohort,
    cohortId = cohortId,
    indexDate = "cohort_start_date",
    ageRange = list(c(0, 150)),
    sex = sex,
    minPriorObservation = 0,
    minFutureObservation = 0,
    name = name,
    attritionAge = FALSE,
    attritionSex = TRUE,
    attritionPriorObservation = FALSE,
    attritionFutureObservation = FALSE
  )

  return(cohort)
}

#' Restrict cohort on prior observation
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param minPriorObservation A minimum number of prior observation days in
#' the database.
#' @param name Name of the new cohort with the prior observation restriction.
#'
#' @return The cohort table with only records for individuals satisfying the
#' prior observation requirement
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   requirePriorObservation(indexDate = "cohort_start_date",
#'                           minPriorObservation = 365)
requirePriorObservation <- function(cohort,
                                    cohortId = NULL,
                                    indexDate = "cohort_start_date",
                                    minPriorObservation = 0,
                                    name = omopgenerics::tableName(cohort)) {
  cohort <- demographicsFilter(
    cohort = cohort,
    cohortId = cohortId,
    indexDate = indexDate,
    ageRange = list(c(0, 150)),
    sex = "Both",
    minPriorObservation = minPriorObservation,
    minFutureObservation = 0,
    name = name,
    attritionAge = FALSE,
    attritionSex = FALSE,
    attritionPriorObservation = TRUE,
    attritionFutureObservation = FALSE
  )

  return(cohort)
}

#' Restrict cohort on future observation
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param minFutureObservation A minimum number of future observation days in
#' the database.
#' @param name Name of the new cohort with the future observation restriction.
#'
#' @return The cohort table with only records for individuals satisfying the
#' future observation requirement
#'
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   requireFutureObservation(indexDate = "cohort_start_date",
#'                            minFutureObservation = 30)
requireFutureObservation <- function(cohort,
                                     cohortId = NULL,
                                     indexDate = "cohort_start_date",
                                     minFutureObservation = 0,
                                     name = omopgenerics::tableName(cohort)) {
  cohort <- demographicsFilter(
    cohort = cohort,
    cohortId = cohortId,
    indexDate = indexDate,
    ageRange = list(c(0, 150)),
    sex = "Both",
    minPriorObservation = 0,
    minFutureObservation = minFutureObservation,
    name = name,
    attritionAge = FALSE,
    attritionSex = FALSE,
    attritionPriorObservation = FALSE,
    attritionFutureObservation = TRUE
  )

  return(cohort)
}

demographicsFilter <- function(cohort,
                               cohortId,
                               indexDate,
                               ageRange,
                               sex,
                               minPriorObservation,
                               minFutureObservation,
                               name,
                               attritionAge,
                               attritionSex,
                               attritionPriorObservation,
                               attritionFutureObservation
                               ) {
  # checks
  name <- validateName(name)
  assertChoice(sex, choices = c("Both", "Male", "Female"), length = 1)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  # age range
  if (!is.list(ageRange)) {
    cli::cli_abort("ageRange must be a list")
  }
  if (length(ageRange[[1]]) != 2 ||
      !is.numeric(ageRange[[1]]) ||
      !ageRange[[1]][2] >= ageRange[[1]][1] ||
      !ageRange[[1]][1] >= 0) {
    cli::cli_abort("ageRange only contain a vector of length two, with the
                   second number greater or equal to the first")
  }
  if (length(ageRange) != 1) {
    cli::cli_abort("Only a single ageRange is currently supported")
  }
  # minPriorObservation
  if (!is.numeric(minPriorObservation) ||
    length(minPriorObservation) != 1 ||
    !minPriorObservation >= 0) {
    cli::cli_abort("minPriorObservation must be a positive number")
  }
  # minFutureObservation
  if (!is.numeric(minFutureObservation) ||
    length(minFutureObservation) != 1 ||
    !minFutureObservation >= 0) {
    cli::cli_abort("minFutureObservation must be a positive number")
  }

  minAge <- ageRange[[1]][1]
  maxAge <- ageRange[[1]][2]
  if (sex == "Both") {
    sex <- c("Male", "Female")
  }
  noRequirementsIds <- ids[!ids %in% cohortId]

  # because the cohort table passed to the function might have extra columns
  # that would conflict with ones we'll add, we'll take the core table first
  # join later
  workingName <- omopgenerics::uniqueTableName()
  workingTable <- cohort |>
    dplyr::select(dplyr::all_of(c(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      indexDate
    ))) %>%
    PatientProfiles::addDemographics(indexDate = indexDate) |>
    dplyr::compute(name = workingName, temporary = FALSE)

  # filter and attritions
  if (attritionAge) {
    workingTable <- workingTable |>
      dplyr::filter(
        (.data$age >= .env$minAge & .data$age <= .env$maxAge) |
          .data$cohort_definition_id %in% noRequirementsIds
      ) |>
      dplyr::compute(name = workingName, temporary = FALSE) |>
      CDMConnector::recordCohortAttrition(
        reason = glue::glue("Age requirement: {minAge} to {maxAge}"),
        cohortId = cohortId
      )
  }
  if (attritionSex) {
    if (all(c("Male", "Female") %in% sex)) {
      sexAttr <- "Both"
    } else {
      sexAttr <- sex
    }
    workingTable <- workingTable |>
      dplyr::filter(
        .data$sex %in% .env$sex |
          .data$cohort_definition_id %in% noRequirementsIds
      ) |>
      dplyr::compute(name = workingName, temporary = FALSE) |>
      CDMConnector::recordCohortAttrition(
        reason = glue::glue("Sex requirement: {sexAttr}"),
        cohortId = cohortId
      )
  }
  if (attritionPriorObservation) {
    workingTable <- workingTable |>
      dplyr::filter(
        .data$prior_observation >= .env$minPriorObservation |
          .data$cohort_definition_id %in% noRequirementsIds
      ) |>
      dplyr::compute(name = workingName, temporary = FALSE) |>
      CDMConnector::recordCohortAttrition(
        reason = glue::glue("Prior observation requirement: {minPriorObservation} days"),
        cohortId = cohortId
      )
  }
  if (attritionFutureObservation) {
    workingTable <- workingTable |>
      dplyr::filter(
        .data$future_observation >= .env$minFutureObservation |
          .data$cohort_definition_id %in% noRequirementsIds) |>
      dplyr::compute(name = workingName, temporary = FALSE) |>
      CDMConnector::recordCohortAttrition(
        reason = glue::glue("Future observation requirement: {minFutureObservation} days"),
        cohortId = cohortId)
  }

  # get original columns
  cohort <- cohort |>
    dplyr::inner_join(
      workingTable |>
        dplyr::select(
          c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
        ),
      by = c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
    ) |>
    dplyr::compute(name = name, temporary = FALSE) %>%
    omopgenerics::newCohortTable(
      cohortSetRef = omopgenerics::settings(workingTable),
      cohortAttritionRef = omopgenerics::attrition(workingTable)
    )

  # drop working table and its attributes
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(workingName))

  return(cohort)
}
