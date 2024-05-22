#' Restrict cohort on patient demographics
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
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
#'
#' @export
#'
#' @examples
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort1 |> trimDemographics(ageRange = list(c(10, 30)))
trimDemographics <- function(cohort,
                             cohortId = NULL,
                             ageRange = NULL,
                             sex = NULL,
                             minPriorObservation = NULL,
                             minFutureObservation = NULL,
                             name = tableName(cohort)) {
  # initial validation
  cohort <- validateCohortTable(cohort, FALSE)
  ids <- settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  ageRange <- validateDemographicRequirements(
    ageRange, sex, minPriorObservation, minFutureObservation, null = TRUE
  )
  name <- validateName(name)

  cdm <- omopgenerics::cdmReference(cohort)

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpName <- omopgenerics::uniqueTableName(tablePrefix)
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)

  cli::cli_inform(c("i" = "Building new trimmed cohort"))

  newCohort <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
    ) |>
    dplyr::compute(name = tmpNewCohort, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE)


  if (!is.null(ageRange)) {
    cli::cli_inform(c("Adding birth date"))
    newCohort <- newCohort |>
      PatientProfiles::addDateOfBirth(name = "date_0") %>%
      dplyr::mutate(
        !!!datesAgeRange(ageRange)
      ) %>%
      # correct dates for those born on the 29th february
      correctAgeDates() |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE)
  }
  if (!is.null(minPriorObservation) |
      !is.null(minFutureObservation) |
      !is.null(sex)) {
    cli::cli_inform(c("Adding demographics information"))
    newCohort <- newCohort |>
      PatientProfiles::addDemographics(
        age = FALSE,
        sex = !is.null(sex),
        priorObservation = !is.null(minPriorObservation),
        priorObservationType = "date",
        futureObservation = !is.null(minFutureObservation),
        futureObservationType = "date"
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE)
  }

  newSet <- reqDemographicsCohortSet(
    settings(cohort), cohortId, ageRange, sex,
    minPriorObservation, minFutureObservation, TRUE
  )
  # insert settings
  cdm <- omopgenerics::insertTable(cdm = cdm, name = tmpName, table = newSet)
  newAtt <- newAttribute(newSet, omopgenerics::attrition(cohort), cohortId)
  newCod <- newAttribute(cdm[[tmpName]], attr(cohort, "cohort_codelist"), cohortId)

  cli::cli_inform(c("Creating initial cohort"))
  newCohort <- newCohort |>
    dplyr::rename("target_cohort_rand01" = "cohort_definition_id") |>
    dplyr::left_join(
      cdm[[tmpName]] |>
        dplyr::mutate(
          target_cohort_rand01 = dplyr::if_else(
            is.na(.data$target_cohort_rand01), .data$cohort_definition_id, .data$target_cohort_rand01
          )
        ) |>
        dplyr::rename("sex_req" = "sex"),
      by = "target_cohort_rand01",
      relationship = "many-to-many"
    ) |>
    dplyr::compute(name = tmpNewCohort, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = newAtt,
      cohortCodelistRef = newCod,
      .softValidation = TRUE
    )

  if (!is.null(sex)) {
    cli::cli_inform(c("Trim sex"))
    newCohort <- newCohort |>
      dplyr::filter(
        .data$sex == .data$sex_req | .data$sex_req == "Both"
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE)
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
    newCohort <- newCohort %>%
      dplyr::mutate(
        !!!caseAge(ageRange),
        "cohort_start_date" = dplyr::if_else(
          .data$cohort_start_date <= .data$new_cohort_start_date & .data$requirements == TRUE,
          .data$new_cohort_start_date,
          .data$cohort_start_date
        ),
        "cohort_end_date" = dplyr::case_when(
          .data$cohort_end_date <= .data$new_cohort_end_date & .data$requirements == TRUE ~ .data$cohort_end_date,
          .data$cohort_end_date >= .data$new_cohort_end_date & .data$requirements == TRUE ~ .data$new_cohort_end_date,
          .default = .data$cohort_end_date
        )
      ) |>
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE)
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
    newCohort <- newCohort %>%
      dplyr::mutate(
        "min_prior_observation" = as.integer(.data$min_prior_observation)
      ) %>%
      dplyr::mutate(
        "new_cohort_start_date" = as.Date(!!CDMConnector::dateadd(
          date = "prior_observation",
          number = "min_prior_observation",
          interval = "day"
        )),
        "cohort_start_date" = dplyr::if_else(
          .data$new_cohort_start_date >= .data$cohort_start_date,
          .data$new_cohort_start_date,
          .data$cohort_start_date
        )
      ) |>
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE)
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
            reason = glue::glue("Prior observation requirement: {minPriorObservation} days"),
            cohortId = ids
          )
      }
    }
  }
  if (!is.null(minFutureObservation)) {
    cli::cli_inform(c("Trim future observation"))
    newCohort <- newCohort %>%
      dplyr::filter(
        !!CDMConnector::datediff(
          start = "cohort_start_date",
          end = "future_observation",
          interval = "day"
        ) >=
          .data$min_future_observation
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE)
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
            reason = glue::glue("Future observation requirement: {minFutureObservation} days"),
            cohortId = ids
          )
      }
    }
  }

  # get original columns in settings
  trimCols <- c(
    "age_range", "sex", "min_prior_observation","min_future_observation")[!c(
      is.null(ageRange), is.null(sex), is.null(minPriorObservation), is.null(minFutureObservation))]

  newSet <- newSet |>
    dplyr::select(dplyr::all_of(c("target_cohort_rand01", "cohort_definition_id", "cohort_name", trimCols))) |>
    dplyr::mutate(target_cohort_rand01 = dplyr::if_else(
      is.na(.data$target_cohort_rand01), .data$cohort_definition_id, .data$target_cohort_rand01)
    ) |>
    dplyr::left_join(
      settings(cohort) |>
        dplyr::select(!dplyr::any_of(c(trimCols, "cohort_name"))) |>
        dplyr::rename("target_cohort_rand01" = "cohort_definition_id"),
      by = "target_cohort_rand01"
    ) |>
    dplyr::select(!"target_cohort_rand01")

  # get original columns in cohort
  newCohort <- cohort |>
    dplyr::select(!c("cohort_start_date", "cohort_end_date")) |>
    dplyr::distinct() |>
    dplyr::rename("target_cohort_rand01" = "cohort_definition_id") |>
    dplyr::inner_join(
      newCohort |>
        dplyr::select(dplyr::all_of(c(
          "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date",
          "target_cohort_rand01"
        ))),
      by = unique(c("target_cohort_rand01", "subject_id"))) |>
    dplyr::select(!"target_cohort_rand01") |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = attrition(newCohort),
      cohortCodelistRef = newCod,
      .softValidation = TRUE
    )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

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
  values <- values[values != 0]
  glue::glue("as.Date(local(CDMConnector::dateadd('date_0', {values}, interval = 'year')))") |>
    rlang::parse_exprs() |>
    rlang::set_names(glue::glue("date_{values}"))
}

caseAge <- function(age) {
  prepareColStart <- function(x, col) {
    num <- x |> unlist() |> unique() |> as.character() |> tolower()
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
    x <- paste0(".data$", col, " == ", as.character(num), " ~ as.Date(local(CDMConnector::dateadd(date = 'date_", as.character(num+1) ,"', number = -1, interval = 'day')))")
    if (infFlag) {
      x <- c(x, paste0("is.infinite(.data$", col, ") ~ .data$cohort_end_date"))
    }
    x <- paste0(x, collapse = ", ")
    x <- paste0("dplyr::case_when(", x, ")") |>
      rlang::parse_exprs() |>
      rlang::set_names("new_cohort_end_date")
    return(x)
  }
  ageMin <- lapply(age, function(x){x[1]}) |>
    prepareColStart("min_age")
  ageMax <- lapply(age, function(x){x[2]}) |>
    prepareColEnd("max_age")
  c(ageMin, ageMax)
}

correctAgeDates <- function(x) {
  dateCols <- colnames(x)
  dateCols <- dateCols[grepl("date_", dateCols) & dateCols != "date_0"]
  fixDates <- lapply(dateCols, function(x) {
    glue::glue("dplyr::if_else(month(.data$date_0) == 2 & day(.data$date_0) == 29 & day(.data${x}) == 28,
           as.Date(local(CDMConnector::dateadd(date = '{x}', number = 1))),
           .data${x})") |>
      rlang::parse_expr()
  })
  names(fixDates) <- dateCols
  x <- x %>% dplyr::mutate(!!!fixDates)
  return(x)
}
