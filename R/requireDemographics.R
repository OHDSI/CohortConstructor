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
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(nPerson = 100)
#' cdm$cohort1 |>
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
    reqAge = TRUE,
    reqSex = TRUE,
    reqPriorObservation = TRUE,
    reqFutureObservation = TRUE
  )

  return(cohort)
}

#' Restrict cohort on age
#'
#' @param cohort A cohort table in a cdm reference.
#' @param ageRange A list of minimum and maximum age.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
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
                       ageRange,
                       cohortId = NULL,
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
    reqAge = TRUE,
    reqSex = FALSE,
    reqPriorObservation = FALSE,
    reqFutureObservation = FALSE
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
                       sex,
                       cohortId = NULL,
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
    reqAge = FALSE,
    reqSex = TRUE,
    reqPriorObservation = FALSE,
    reqFutureObservation = FALSE
  )

  return(cohort)
}

#' Restrict cohort on prior observation
#'
#' @param cohort A cohort table in a cdm reference.
#' @param minPriorObservation A minimum number of prior observation days in
#' the database.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
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
                                    minPriorObservation,
                                    cohortId = NULL,
                                    indexDate = "cohort_start_date",
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
    reqAge = FALSE,
    reqSex = FALSE,
    reqPriorObservation = TRUE,
    reqFutureObservation = FALSE
  )

  return(cohort)
}

#' Restrict cohort on future observation
#'
#' @param cohort A cohort table in a cdm reference.
#' @param minFutureObservation A minimum number of future observation days in
#' the database.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
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
                                     minFutureObservation,
                                     cohortId = NULL,
                                     indexDate = "cohort_start_date",
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
    reqAge = FALSE,
    reqSex = FALSE,
    reqPriorObservation = FALSE,
    reqFutureObservation = TRUE
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
                               reqAge,
                               reqSex,
                               reqPriorObservation,
                               reqFutureObservation) {
  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  validateDemographicRequirements(ageRange, sex, minPriorObservation,
                                  minFutureObservation)

  # output cohort attributes ----
  reqCols <- c("age_range", "sex", "min_prior_observation",
               "min_future_observation")[c(
                 reqAge, reqSex, reqPriorObservation, reqFutureObservation)]

  newSet <- reqDemographicsCohortSet(
    omopgenerics::settings(cohort), cohortId, ageRange, sex,
    minPriorObservation, minFutureObservation
  )

  tempSetName <- omopgenerics::uniqueTableName()
  cdm <- CDMConnector::insertTable(cdm, name = tempSetName, newSet)
  newAtt <- newAttribute(newSet, omopgenerics::attrition(cohort), cohortId)
  newCod <- newAttribute(cdm[[tempSetName]], attr(cohort, "cohort_codelist"), cohortId)

  # cohort table ----
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
    PatientProfiles::addDemographics(
      indexDate = indexDate,
      age  = reqAge,
      sex = reqSex,
      priorObservation = reqPriorObservation,
      futureObservation = reqFutureObservation
    ) |>
    dplyr::compute(name = workingName, temporary = FALSE)

  # all output cohorts in one table to filter all at the same time:
  workingTable <- workingTable |>
    dplyr::rename("target_cohort_rand01" = "cohort_definition_id") |>
    dplyr::left_join(
      cdm[[tempSetName]] |>
        dplyr::mutate(
          target_cohort_rand01 = dplyr::if_else(
            is.na(.data$target_cohort_rand01), .data$cohort_definition_id, .data$target_cohort_rand01
          )
        ) |>
        dplyr::rename("sex_req" = "sex"),
      by = "target_cohort_rand01",
      relationship = "many-to-many"
    ) |>
    dplyr::compute(name = workingName, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = newAtt,
      cohortCodelistRef = newCod,
      .softValidation = TRUE
    )

  # filter + record attrition ----
  # age
  if (reqAge) {
    # filter
    workingTable <- workingTable |>
      dplyr::filter(.data$age >= .data$min_age & .data$age <= .data$max_age) |>
      dplyr::compute(name = workingName, temporary = FALSE)
    # attrition
    uniqueRanges <- unique(newSet$age_range)
    for (ii in seq_along(uniqueRanges)) {
      ids <- newSet |>
        dplyr::filter(.data$age_range == .env$uniqueRanges[ii]) |>
        dplyr::filter(.data$requirements)
      minAge <- unique(ids$min_age)
      maxAge <- unique(ids$max_age)
      ids <- ids$cohort_definition_id
      if (length(ids) > 0) {
        workingTable <- workingTable |>
          omopgenerics::recordCohortAttrition(
            reason = glue::glue("Age requirement: {minAge} to {maxAge}"),
            cohortId = ids
          )
      }
    }
  }
  # sex
  if (reqSex) {
    workingTable <- workingTable |>
      dplyr::filter(.data$sex == .data$sex_req | .data$sex_req == "Both") |>
      dplyr::compute(name = workingName, temporary = FALSE)
    # attrition
    uniqueSex <- unique(newSet$sex)
    for (ii in seq_along(uniqueSex)) {
      ids <- newSet |>
        dplyr::filter(.data$sex == .env$uniqueSex[ii]) |>
        dplyr::filter(.data$requirements)
      sex <- unique(ids$sex)
      ids <- ids$cohort_definition_id
      if (length(ids) > 0) {
        workingTable <- workingTable |>
          omopgenerics::recordCohortAttrition(
            reason = glue::glue("Sex requirement: {sex}"),
            cohortId = ids
          )
      }
    }
  }
  # prior observation
  if (reqPriorObservation) {
    workingTable <- workingTable |>
      dplyr::filter(.data$prior_observation >= .data$min_prior_observation) |>
      dplyr::compute(name = workingName, temporary = FALSE)
    # attrition
    uniquePrior <- unique(newSet$min_prior_observation)
    for (ii in seq_along(uniquePrior)) {
      ids <- newSet |>
        dplyr::filter(.data$min_prior_observation == .env$uniquePrior[ii]) |>
        dplyr::filter(.data$requirements)
      minPriorObservation <- unique(ids$min_prior_observation)
      ids <- ids$cohort_definition_id
      if (length(ids) > 0) {
        workingTable <- workingTable |>
          omopgenerics::recordCohortAttrition(
            reason = glue::glue("Prior observation requirement: {minPriorObservation} days"),
            cohortId = ids
          )
      }
    }
  }
  # future observation
  if (reqFutureObservation) {
    workingTable <- workingTable |>
      dplyr::filter(.data$future_observation >= .data$min_future_observation) |>
      dplyr::compute(name = workingName, temporary = FALSE)
    # attrition
    uniqueFuture <- unique(newSet$min_future_observation)
    for (ii in seq_along(uniqueFuture)) {
      ids <- newSet |>
        dplyr::filter(.data$min_future_observation == .env$uniqueFuture[ii]) |>
        dplyr::filter(.data$requirements)
      minFutureObservation <- unique(ids$min_future_observation)
      ids <- ids$cohort_definition_id
      if (length(ids) > 0) {
        workingTable <- workingTable |>
          omopgenerics::recordCohortAttrition(
            reason = glue::glue("Future observation requirement: {minFutureObservation} days"),
            cohortId = ids
          )
      }
    }
  }

  # get original columns in settings
  newSet <- newSet |>
    dplyr::select(dplyr::all_of(c("target_cohort_rand01", "cohort_definition_id", "cohort_name", reqCols))) |>
    dplyr::mutate(target_cohort_rand01 = dplyr::if_else(
      is.na(.data$target_cohort_rand01), .data$cohort_definition_id, .data$target_cohort_rand01)
    ) |>
    dplyr::left_join(
      settings(cohort) |>
        dplyr::select(!dplyr::any_of(c(reqCols, "cohort_name"))) |>
        dplyr::rename("target_cohort_rand01" = "cohort_definition_id"),
      by = "target_cohort_rand01"
    ) |>
    dplyr::select(!"target_cohort_rand01")

  # get original columns in cohort
  newCohort <- cohort |>
    dplyr::rename("target_cohort_rand01" = "cohort_definition_id") |>
    dplyr::inner_join(
      workingTable |>
        dplyr::select(dplyr::all_of(c(
          "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date",
          "target_cohort_rand01", indexDate
        ))),
      by = unique(c("target_cohort_rand01", "subject_id", "cohort_start_date", "cohort_end_date", indexDate))) |>
    dplyr::select(!"target_cohort_rand01") |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = attrition(workingTable),
      cohortCodelistRef = newCod
    )

  # drop working tables and their attributes
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(workingName))
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tempSetName))

  return(newCohort)
}

reqDemographicsCohortSet <- function(set,
                                     targetIds,
                                     ageRange,
                                     sex,
                                     minPriorObservation,
                                     minFutureObservation) {

  combinations <- tidyr::expand_grid(
    requirements = TRUE,
    target_cohort_rand01 = targetIds,
    age_range = lapply(ageRange, function(x){paste0(x[1], "_", x[2])}) |> unlist(),
    sex = sex,
    min_prior_observation = minPriorObservation,
    min_future_observation = minFutureObservation
  ) |>
    dplyr::mutate(cohort_definition_id = .data$target_cohort_rand01) |>
    dplyr::arrange(.data$cohort_definition_id, .data$age_range, .data$sex, .data$min_prior_observation, .data$min_future_observation) |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::mutate(group_id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      set |> dplyr::select("cohort_definition_id", "cohort_name"),
      by = "cohort_definition_id"
    ) |>
    dplyr::bind_rows(
      set |>
        dplyr::select("cohort_definition_id", "cohort_name") |>
        dplyr::filter(!.data$cohort_definition_id %in% targetIds) |>
        dplyr::mutate(
          age_range = "0_150",
          sex = "Both",
          min_prior_observation  = 0,
          min_future_observation = 0,
          group_id = 1,
          requirements = FALSE
        )
    ) |>
    # correct ids
    dplyr::arrange(.data$group_id, .data$cohort_definition_id) |>
    dplyr::mutate(
      cohort_definition_id = dplyr::if_else(
        .data$group_id == 1,
        .data$cohort_definition_id,
        dplyr::row_number()
      )
    )
  # correct names
  if (length(ageRange) > 1 | length(sex) > 1 | length(minPriorObservation) > 1 |
      length(minFutureObservation) > 1) {
    combinations <- combinations |>
      dplyr::mutate(cohort_name = dplyr::if_else(
        .data$requirements, paste0(.data$cohort_name, "_", .data$group_id), .data$cohort_name
      ))
  }


  combinations <- combinations |>
    dplyr::mutate(
      min_age = as.numeric(sub("_.*", "", .data$age_range)),
      max_age = as.numeric(sub(".*_", "", .data$age_range))
    ) |>
    dplyr::select(!c("group_id"))

  # new cohort set
  return(combinations)
}

newAttribute <- function(newSet, att, cohortId) {
  newSet |>
    dplyr::select(c("cohort_definition_id", "target_cohort_rand01")) |>
    dplyr::inner_join(
      att |>
        dplyr::rename("target_cohort_rand01" = "cohort_definition_id"),
      by = "target_cohort_rand01",
      relationship = "many-to-many"
    ) |>
    dplyr::select(!"target_cohort_rand01") |>
    dplyr::union_all(
      att |> dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId)
    )
}
