#' Restrict cohort on patient demographics
#'
#' @description
#' `requireDemographics()` filters cohort records, keeping only records where
#' individuals satisfy the specified demographic criteria.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @inheritParams requireDemographicsDoc
#' @inheritParams atFirstDoc
#'
#' @return The cohort table with only records for individuals satisfying the
#' demographic requirements
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 |>
#'   requireDemographics(indexDate = "cohort_start_date",
#'                       ageRange = list(c(18, 65)),
#'                       sex = "Female",
#'                       minPriorObservation = 365)
#' }
#' }
requireDemographics <- function(cohort,
                                cohortId = NULL,
                                indexDate = "cohort_start_date",
                                ageRange = list(c(0, 150)),
                                sex = c("Both"),
                                minPriorObservation = 0,
                                minFutureObservation = 0,
                                atFirst = FALSE,
                                name = tableName(cohort)) {
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
    reqFutureObservation = TRUE,
    atFirst = atFirst
  )

  return(cohort)
}

#' Restrict cohort on age
#'
#' @description
#' `requireAge()` filters cohort records, keeping only records where individuals
#' satisfy the specified age criteria.
#'
#' @inheritParams requireDemographics
#'
#' @return The cohort table with only records for individuals satisfying the
#' age requirement
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   requireAge(indexDate = "cohort_start_date",
#'              ageRange = list(c(18, 65)))
#' }
#' }
requireAge <- function(cohort,
                       ageRange,
                       cohortId = NULL,
                       indexDate = "cohort_start_date",
                       atFirst = FALSE,
                       name = tableName(cohort)) {
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
    reqFutureObservation = FALSE,
    atFirst = atFirst
  )

  return(cohort)
}

#' Restrict cohort on sex
#'
#' @description
#' `requireSex()` filters cohort records, keeping only records where individuals
#' satisfy the specified sex criteria.
#'
#' @inheritParams requireDemographics
#'
#' @return The cohort table with only records for individuals satisfying the
#' sex requirement
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   requireSex(sex = "Female")
#' }
#' }
requireSex <- function(cohort,
                       sex,
                       cohortId = NULL,
                       atFirst = FALSE,
                       name = tableName(cohort)) {
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
    reqFutureObservation = FALSE,
    atFirst = atFirst
  )

  return(cohort)
}

#' Restrict cohort on prior observation
#'
#' @description
#' `requirePriorObservation()` filters cohort records, keeping only records
#' where individuals satisfy the specified prior observation criteria.
#'
#' @inheritParams requireDemographics
#'
#' @return The cohort table with only records for individuals satisfying the
#' prior observation requirement
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   requirePriorObservation(indexDate = "cohort_start_date",
#'                           minPriorObservation = 365)
#' }
#' }
requirePriorObservation <- function(cohort,
                                    minPriorObservation,
                                    cohortId = NULL,
                                    indexDate = "cohort_start_date",
                                    atFirst = FALSE,
                                    name = tableName(cohort)) {
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
    reqFutureObservation = FALSE,
    atFirst = atFirst
  )

  return(cohort)
}

#' Restrict cohort on future observation
#'
#' @description
#' `requireFutureObservation()` filters cohort records, keeping only records
#' where individuals satisfy the specified future observation criteria.
#'
#' @inheritParams requireDemographics
#'
#' @return The cohort table with only records for individuals satisfying the
#' future observation requirement
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   requireFutureObservation(indexDate = "cohort_start_date",
#'                            minFutureObservation = 30)
#' }
#' }
requireFutureObservation <- function(cohort,
                                     minFutureObservation,
                                     cohortId = NULL,
                                     indexDate = "cohort_start_date",
                                     atFirst = FALSE,
                                     name = tableName(cohort)) {
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
    reqFutureObservation = TRUE,
    atFirst = atFirst
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
                               reqFutureObservation,
                               atFirst) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  ageRange <- validateDemographicRequirements(ageRange, sex, minPriorObservation, minFutureObservation)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  omopgenerics::assertLogical(atFirst, length = 1)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |>
      dplyr::compute(
        name = name, temporary = FALSE,
        logPrefix = "CohortConstructor_demographicsFilter_entry_"
      )
    return(cdm[[name]])
  }

  # output cohort attributes ----
  reqCols <- c("age_range",  "sex", "min_prior_observation","min_future_observation")[c(
    reqAge, reqSex, reqPriorObservation, reqFutureObservation
  )]

  # new settings
  ind <- reqCols %in% colnames(settings(cohort))
  if (any(ind)) {
    # only if the setting is not NA (eg been applied to a different cohort)
    if(nrow(settings(cohort) |>
      dplyr::filter(dplyr::if_all(dplyr::any_of(reqCols), ~ !is.na(.))) |>
      dplyr::filter(.data$cohort_definition_id %in% cohortId)) > 0){
      cli::cli_warn("{reqCols[ind]} column{?s} are already in settings and will be overwritten")
    }
  }
  newSet <- settings(cohort) |>
    dplyr::select(!dplyr::any_of(reqCols)) |>
    dplyr::left_join(
      dplyr::tibble(
        "cohort_definition_id" = cohortId,
        "age_range" = paste0(ageRange[[1]][1], "_", ageRange[[1]][2]),
        "sex" = sex,
        "min_prior_observation" = minPriorObservation,
        "min_future_observation" = minFutureObservation,
        "at_first" = atFirst
      ) |>
        dplyr::select(dplyr::all_of(c("cohort_definition_id", reqCols))),
      by = "cohort_definition_id"
    )

  # cohort table ----
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)

  # because the cohort table passed to the function might have extra columns
  # that would conflict with ones we'll add, we'll take the core table first
  # join later
  newCols <- uniqueColumnName( cdm[[tmpNewCohort]], n = 4)
  cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
    PatientProfiles::addDemographics(
      indexDate = indexDate,
      age  = reqAge,
      ageName = newCols[1],
      sex = reqSex,
      sexName = newCols[2],
      priorObservation = reqPriorObservation,
      priorObservationName = newCols[3],
      futureObservation = reqFutureObservation,
      futureObservationName = newCols[4],
      name = tmpNewCohort
    )

  # atFirst
  min_age <- ageRange[[1]][1]
  max_age <- ageRange[[1]][2]
  if (is.infinite(min_age)) min_age <- 0
  if (is.infinite(max_age)) max_age <- 200
  filterAge <- glue::glue(".data${newCols[1]} >= {min_age} & .data${newCols[1]} <= {max_age}") |> rlang::parse_exprs()
  filterSex <- glue::glue(".data${newCols[2]} == '{sex}' | (.env$sex == 'Both' & .data${newCols[2]} %in% c('Female', 'Male'))") |> rlang::parse_exprs()
  filterPriorObservation <- glue::glue(".data${newCols[3]} >= {minPriorObservation}") |> rlang::parse_exprs()
  filterFutureObservation <- glue::glue(".data${newCols[4]} >= {minFutureObservation}") |> rlang::parse_exprs()
  atFirstReason <- NULL
  tmpNewCohortFirst <- NULL
  if (atFirst) {
    tmpNewCohortFirst <- paste0(tmpNewCohort, "_1")
    cdm[[tmpNewCohortFirst]] <- cdm[[tmpNewCohort]] |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::filter(.data$cohort_start_date == base::min(.data$cohort_start_date)) |>
      dplyr::ungroup() |>
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE,
                     logPrefix = "CohortConstructor_demographicsFilter_arrange_")
    atFirstReason <- ". Requirement applied to the first entry"
  }

  # filter + record attrition ----
  # age
  if (reqAge) {
    # filter
    cdm <- applyDemographicsFilter(cdm, atFirst, filterAge, tmpNewCohort, tmpNewCohortFirst)
    cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
      omopgenerics::recordCohortAttrition(
        reason = "Age requirement: {ageRange[[1]][1]} to {ageRange[[1]][2]}{atFirstReason}",
        cohortId = cohortId
      )
  }
  # sex
  if (reqSex) {
    cdm <- applyDemographicsFilter(cdm, atFirst, filterSex, tmpNewCohort, tmpNewCohortFirst, sex = sex)
    cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
      omopgenerics::recordCohortAttrition(
        reason = "Sex requirement: {sex}{atFirstReason}",
        cohortId = cohortId
      )
  }
  # prior observation
  if (reqPriorObservation) {
    cdm <- applyDemographicsFilter(cdm, atFirst, filterPriorObservation, tmpNewCohort, tmpNewCohortFirst)
    cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
      omopgenerics::recordCohortAttrition(
        reason = "Prior observation requirement: {minPriorObservation} days{atFirstReason}",
        cohortId = cohortId
      )
  }
  # future observation
  if (reqFutureObservation) {
    cdm <- applyDemographicsFilter(cdm, atFirst, filterFutureObservation, tmpNewCohort, tmpNewCohortFirst)
    cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
      omopgenerics::recordCohortAttrition(
        reason = "Future observation requirement: {minFutureObservation} days{atFirstReason}",
        cohortId = cohortId
      )
  }

  cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
    dplyr::select(!dplyr::any_of(c(newCols))) |>
    dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                   logPrefix = "CohortConstructor_demographicsFilter_select_")

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
      # join non modified cohorts
      dplyr::union_all(cdm[[tmpUnchanged]]) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_demographicsFilter_union_")
  }

  cdm[[name]] <- cdm[[tmpNewCohort]] |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_demographicsFilter_name_") |>
    omopgenerics::newCohortTable(
      .softValidation = TRUE, cohortSetRef = newSet
    )

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cdm[[name]])
}

reqDemographicsCohortSet <- function(set,
                                     targetIds,
                                     ageRange,
                                     sex,
                                     minPriorObservation,
                                     minFutureObservation,
                                     requirementInteractions) {
  if (is.null(ageRange)) {
    ageRange <- list(c(0, 150))
  }
  if (is.null(sex)) {
    sex <- "Both"
  }
  if (is.null(minPriorObservation)) {
    minPriorObservation <- 0
  }
  if (is.null(minFutureObservation)) {
    minFutureObservation <- 0
  }

  minPriorObservation <- as.integer(minPriorObservation)
  minFutureObservation <- as.integer(minFutureObservation)
  if (isTRUE(requirementInteractions)) {
    combinations <- tidyr::expand_grid(
      requirements = TRUE,
      target_cohort_rand01 = targetIds,
      age_range = lapply(ageRange, function(x) {
        paste0(x[1], "_", x[2])
      }) |> unlist(),
      sex = sex,
      min_prior_observation = minPriorObservation,
      min_future_observation = minFutureObservation
    )
  } else {
    ageRangeFormatted <- unlist(lapply(ageRange, function(x) {
      paste0(x[1], "_", x[2])
    }))
    combinations <- dplyr::bind_rows(
      dplyr::tibble(
        age_range = .env$ageRangeFormatted,
        sex = .env$sex[1],
        min_prior_observation = .env$minPriorObservation[1],
        min_future_observation = .env$minFutureObservation[1]
      ),
      dplyr::tibble(
        age_range = ageRangeFormatted[1],
        sex = .env$sex,
        min_prior_observation = .env$minPriorObservation[1],
        min_future_observation = .env$minFutureObservation[1]
      ),
      dplyr::tibble(
        age_range = ageRangeFormatted[1],
        sex = .env$sex[1],
        min_prior_observation = .env$minPriorObservation,
        min_future_observation = .env$minFutureObservation[1]
      ),
      dplyr::tibble(
        age_range = ageRangeFormatted[1],
        sex = .env$sex[1],
        min_prior_observation = .env$minPriorObservation[1],
        min_future_observation = .env$minFutureObservation
      )
    ) |>
      dplyr::cross_join(dplyr::tibble(target_cohort_rand01 = targetIds)) |>
      dplyr::mutate(requirements = TRUE) |>
      dplyr::distinct()
  }

  combinations <- combinations |>
    dplyr::mutate(cohort_definition_id = .data$target_cohort_rand01) |>
    dplyr::arrange(
      .data$cohort_definition_id,
      .data$age_range,
      .data$sex,
      .data$min_prior_observation,
      .data$min_future_observation
    ) |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::mutate(group_id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::left_join(set |> dplyr::select("cohort_definition_id", "cohort_name"),
                     by = "cohort_definition_id") |>
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
  if (length(ageRange) > 1 ||
      length(sex) > 1 || length(minPriorObservation) > 1 ||
      length(minFutureObservation) > 1) {
    combinations <- combinations |>
      dplyr::mutate(cohort_name = dplyr::if_else(
        .data$requirements,
        paste0(.data$cohort_name, "_", .data$group_id),
        .data$cohort_name
      ))
  }
  combinations <- combinations |>
    dplyr::mutate(min_age = as.integer(sub("_.*", "", .data$age_range)),
                  max_age = sub(".*_", "", .data$age_range)) |>
    dplyr::mutate(max_age = stringr::str_replace(.data$max_age, "Inf", "999")) |>
    dplyr::mutate(max_age = as.integer(.data$max_age)) |>
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
    dplyr::union_all(att |> dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId))
}

applyDemographicsFilter <- function(cdm, atFirst, filt, tmpNewCohort, tmpNewCohortFirst, sex = NULL) {
  if (atFirst) {
    cdm[[tmpNewCohortFirst]] <- cdm[[tmpNewCohortFirst]] |>
      dplyr::filter(!!!filt) |>
      dplyr::compute(
        name = tmpNewCohortFirst, temporary = FALSE,
        logPrefix = "CohortConstructor_demographicsFilter_reqAge1_"
      )
    cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
      dplyr::inner_join(
        cdm[[tmpNewCohortFirst]] |>
          dplyr::select(dplyr::all_of(c("cohort_definition_id", "subject_id"))),
        by = c("cohort_definition_id", "subject_id")
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_demographicsFilter_reqAge2_")
  } else {
    cdm[[tmpNewCohort]] <- cdm[[tmpNewCohort]] |>
      dplyr::filter(!!!filt) |>
      dplyr::compute(
        name = tmpNewCohort, temporary = FALSE,
        logPrefix = "CohortConstructor_demographicsFilter_reqAge_"
      )
  }
  return(cdm)
}
