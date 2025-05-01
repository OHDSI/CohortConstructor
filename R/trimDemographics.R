#' Trim cohort on patient demographics
#'
#' @description
#' `trimDemographics()` resets the cohort start and end date based on the
#' specified demographic criteria is satisfied.
#'
#' @inheritParams requireDemographics
#' @inheritParams cohortIdModifyDoc
#' @inheritParams softValidationDoc
#'
#' @return The cohort table with only records for individuals satisfying the
#' demographic requirements
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort1 |> trimDemographics(ageRange = list(c(10, 30)))
#' }
trimDemographics <- function(cohort,
                             cohortId = NULL,
                             ageRange = NULL,
                             sex = NULL,
                             minPriorObservation = NULL,
                             minFutureObservation = NULL,
                             name = tableName(cohort),
                             .softValidation = TRUE) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  ageRange <- validateDemographicRequirements(
    ageRange = ageRange,
    sex = sex,
    minPriorObservation = minPriorObservation,
    minFutureObservation = minFutureObservation,
    null = TRUE,
    length = NULL
  )
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning empty cohort as `cohortId` is not valid.")
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  ids <- settings(cohort)$cohort_definition_id

  # replace age Inf to avoid potential sql issues
  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpName <- omopgenerics::uniqueTableName(tablePrefix)
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)

  cli::cli_inform(c("i" = "Building new trimmed cohort"))

  newCohort <- cohort |>
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                   logPrefix = "CohortConstructor_trimDemographics_trimmed_") |>
    omopgenerics::newCohortTable(.softValidation = .softValidation)

  if (!is.null(ageRange) ||
      !is.null(minPriorObservation) ||
      !is.null(minFutureObservation) ||
      !is.null(sex)) {
    cli::cli_inform(c("Adding demographics information"))
    newCohort <- newCohort |>
      PatientProfiles::addDemographics(
        age = FALSE,
        sex = !is.null(sex),
        priorObservation = !is.null(minPriorObservation),
        priorObservationType = "date",
        futureObservation = !is.null(minFutureObservation),
        futureObservationType = "date",
        dateOfBirth = TRUE,
        dateOfBirthName = "date_0",
        name = tmpNewCohort
      )
  }

  newSet <- reqDemographicsCohortSet(
    set = settings(cohort),
    targetIds = as.integer(cohortId),
    ageRange = ageRange,
    sex = sex,
    minPriorObservation = minPriorObservation,
    minFutureObservation = minFutureObservation,
    requirementInteractions = TRUE
  )

  # insert settings
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = tmpName,
    table = newSet
  )
  newAtt <- newAttribute(newSet, omopgenerics::attrition(cohort), cohortId)
  newCod <- newAttribute(cdm[[tmpName]], attr(cohort, "cohort_codelist"), cohortId)

  cli::cli_inform(c("Creating initial cohort"))
  newCohort <- newCohort |>
    dplyr::rename("target_cohort_rand01" = "cohort_definition_id") |>
    dplyr::left_join(
      cdm[[tmpName]] |>
        dplyr::mutate(target_cohort_rand01 = as.integer(
          dplyr::if_else(
            is.na(.data$target_cohort_rand01),
            .data$cohort_definition_id,
            .data$target_cohort_rand01
          )
        )) |>
        dplyr::rename("sex_req" = "sex"),
      by = "target_cohort_rand01",
      relationship = "many-to-many"
    ) |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                   logPrefix = "CohortConstructor_trimDemographics_initial_")

  newCohort <- newCohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = newAtt,
      cohortCodelistRef = newCod,
      .softValidation = TRUE
    )

  if (!is.null(sex)) {
    cli::cli_inform(c("Trim sex"))
    newCohort <- newCohort |>
      dplyr::filter(.data$sex == .data$sex_req |
                      .data$sex_req == "Both") |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_trimDemographics_sex_")
    # attrition
    uniqueSex <- unique(newSet$sex)
    for (ii in seq_along(uniqueSex)) {
      ids <- newSet |>
        dplyr::filter(.data$sex == .env$uniqueSex[ii]) |>
        dplyr::filter(.data$requirements)
      sex <- unique(ids$sex)
      ids <- ids$cohort_definition_id
      if (length(ids) > 0) {
        newCohort <- newCohort |>
          omopgenerics::recordCohortAttrition(
            reason = glue::glue("Sex requirement: {sex}"),
            cohortId = ids
          )
      }
    }
  }
  if (!is.null(ageRange)) {
    cli::cli_inform(c("Trim age"))

    for (j in seq_along(ageRange)) {
      ageRange[[j]][is.infinite(ageRange[[j]])] <- 999L
    }

    newCohort <- newCohort |>
      dplyr::mutate(!!!datesAgeRange(ageRange)) |>
      dplyr::mutate(
        !!!caseAge(ageRange),
        "cohort_start_date" = dplyr::if_else(
          .data$cohort_start_date <= .data$new_cohort_start_date &
            .data$requirements == TRUE,
          .data$new_cohort_start_date,
          .data$cohort_start_date
        ),
        "cohort_end_date" = dplyr::case_when(
          .data$cohort_end_date <= .data$new_cohort_end_date &
            .data$requirements == TRUE ~ .data$cohort_end_date,
          .data$cohort_end_date >= .data$new_cohort_end_date &
            .data$requirements == TRUE ~ .data$new_cohort_end_date,
          .default = .data$cohort_end_date
        )
      ) |>
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_trimDemographics_dates_")
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
        newCohort <- newCohort |>
          omopgenerics::recordCohortAttrition(
            reason = glue::glue("Age requirement: {minAge} to {maxAge}"),
            cohortId = ids
          )
      }
    }
  }
  if (!is.null(minPriorObservation)) {
    cli::cli_inform(c("Trim prior observation"))
    newCohort <- newCohort |>
      dplyr::mutate("min_prior_observation" = as.integer(.data$min_prior_observation)) %>%
      dplyr::mutate(
        "new_cohort_start_date" = as.Date(
          !!CDMConnector::dateadd(
            date = "prior_observation",
            number = "min_prior_observation",
            interval = "day"
          )
        ),
        "cohort_start_date" = dplyr::if_else(
          .data$new_cohort_start_date >= .data$cohort_start_date,
          .data$new_cohort_start_date,
          .data$cohort_start_date
        )
      ) |>
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_trimDemographics_priorObs_")
    # attrition
    uniquePrior <- unique(newSet$min_prior_observation)
    for (ii in seq_along(uniquePrior)) {
      ids <- newSet |>
        dplyr::filter(.data$min_prior_observation == .env$uniquePrior[ii]) |>
        dplyr::filter(.data$requirements)
      minPriorObservation <- unique(ids$min_prior_observation)
      ids <- ids$cohort_definition_id
      if (length(ids) > 0) {
        newCohort <- newCohort |>
          omopgenerics::recordCohortAttrition(
            reason = glue::glue(
              "Prior observation requirement: {minPriorObservation} days"
            ),
            cohortId = ids
          )
      }
    }
  }
  if (!is.null(minFutureObservation)) {
    cli::cli_inform(c("Trim future observation"))
    newCohort <- newCohort %>%
      dplyr::filter(
        !!CDMConnector::datediff("cohort_start_date", "future_observation") >=
          .data$min_future_observation
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_trimDemographics_futureObs_")
    # attrition
    uniqueFuture <- unique(newSet$min_future_observation)
    for (ii in seq_along(uniqueFuture)) {
      ids <- newSet |>
        dplyr::filter(.data$min_future_observation == .env$uniqueFuture[ii]) |>
        dplyr::filter(.data$requirements)
      minFutureObservation <- unique(ids$min_future_observation)
      ids <- ids$cohort_definition_id
      if (length(ids) > 0) {
        newCohort <- newCohort |>
          omopgenerics::recordCohortAttrition(
            reason = glue::glue(
              "Future observation requirement: {minFutureObservation} days"
            ),
            cohortId = ids
          )
      }
    }
  }

  # get original columns in settings
  trimCols <- c(
    "age_range",
    "sex",
    "min_prior_observation",
    "min_future_observation"
  )[!c(
    is.null(ageRange),
    is.null(sex),
    is.null(minPriorObservation),
    is.null(minFutureObservation)
  )]

  newSet <- newSet |>
    dplyr::select(dplyr::all_of(
      c(
        "target_cohort_rand01",
        "cohort_definition_id",
        "cohort_name",
        trimCols
      )
    )) |>
    dplyr::mutate(
      target_cohort_rand01 = dplyr::if_else(
        is.na(.data$target_cohort_rand01),
        .data$cohort_definition_id,
        .data$target_cohort_rand01
      )
    ) |>
    dplyr::left_join(
      settings(cohort) |>
        dplyr::select(!dplyr::any_of(c(
          trimCols, "cohort_name"
        ))) |>
        dplyr::rename("target_cohort_rand01" = "cohort_definition_id"),
      by = "target_cohort_rand01"
    ) |>
    dplyr::select(!"target_cohort_rand01")

  # get original columns in cohort
  newCohort <- cohort |>
    dplyr::select(!c("cohort_start_date", "cohort_end_date")) |>
    dplyr::distinct() |>
    dplyr::rename("target_cohort_rand01" = "cohort_definition_id") |>
    dplyr::inner_join(newCohort |>
                        dplyr::select(dplyr::all_of(
                          c(
                            "cohort_definition_id",
                            "subject_id",
                            "cohort_start_date",
                            "cohort_end_date",
                            "target_cohort_rand01"
                          )
                        )), by = unique(c("target_cohort_rand01", "subject_id"))) |>
    dplyr::select(!"target_cohort_rand01") |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE,
                   logPrefix = "CohortConstructor_trimDemographics_original_") |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = attrition(newCohort),
      cohortCodelistRef = newCod,
      .softValidation = FALSE
    )

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  cli::cli_inform(c("v" = "Cohort trimmed"))
  return(newCohort)
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
  values <- values[values != 0] |> as.integer()
  glue::glue("as.Date(clock::add_years(.data$date_0, {values}))") |>
    rlang::parse_exprs() |>
    rlang::set_names(glue::glue("date_{values}"))
}

caseAge <- function(age) {
  ageMin <- lapply(age, function(x) {
    x[1]
  }) |>
    prepareColStart("min_age")
  ageMax <- lapply(age, function(x) {
    x[2]
  }) |>
    prepareColEnd("max_age")
  c(ageMin, ageMax)
}

prepareColStart <- function(x, col) {
  num <- x |>
    unlist() |>
    unique() |>
    as.character() |>
    tolower()
  x <- paste0("date_", num)
  x <- paste0(".data$", col, " == ", num, " ~ .data$", x) |>
    paste0(collapse = ",")
  x <- paste0("dplyr::case_when(", x, ")") |>
    rlang::parse_exprs() |>
    rlang::set_names(c("new_cohort_start_date"))
  return(x)
}

prepareColEnd <- function(x, col) {
  num <- unique(unlist(x))
  infFlag <- any(is.infinite(num))
  num <- num[!is.infinite(num)]
  x <- paste0(
    ".data$",
    col,
    " == ",
    as.character(num),
    " ~ as.Date(clock::add_days(x = .data$date_",
    as.character(num + 1),
    ", n = -1L))"
  )
  if (infFlag) {
    x <- c(
      x,
      paste0("is.infinite(.data$", col, ") ~ .data$cohort_end_date")
    )
  }
  x <- paste0(x, collapse = ", ")
  x <- paste0("dplyr::case_when(", x, ")") |>
    rlang::parse_exprs() |>
    rlang::set_names("new_cohort_end_date")
  return(x)
}
