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
  cohort <- validateCohortTable(cohort, TRUE)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  ageRange <- validateAgeRange(ageRange)
  sex <- validateSex(sex)
  minPriorObservation <- validateMinPriorObservation(minPriorObservation)
  minFutureObservation <- validateMinFutureObservation(minFutureObservation)
  order <- validateOrder(order)
  name <- validateName(name)

  cdm <- omopgenerics::cdmReference(cohort)
  tablePrefix <- omopgenerics::tmpPrefix()

  cli::cli_inform(c("i" = "Building new trimmed cohort"))

  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  if (!is.null(ageRange)) {
    cli::cli_inform(c("Adding birth date"))
    cohort <- cohort |>
      PatientProfiles::addDateOfBirth(name = "date_0") %>%
      dplyr::mutate(!!!datesAgeRange(ageRange))
  }
  if (!is.null(minPriorObservation) |
      !is.null(minFutureObservation) |
      !is.null(sex)) {
    cli::cli_inform(c("Adding demographics information"))
    cohort <- cohort |>
      PatientProfiles::addDemographics(
        age = FALSE,
        sex = !is.null(sex),
        priorObservation = !is.null(minPriorObservation),
        priorObservationType = "date",
        futureObservation = !is.null(minFutureObservation),
        futureObservationType = "date"
      )
  }

  newSettings <- settings(cohort) |>
    getNewSettings(
      cohortId, ageRange, sex, minPriorObservation, minFutureObservation
    )

  # insert settings
  nm <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = nm,
    table = newSettings |> dplyr::select(dplyr::any_of(c(
      "cohort_definition_id", "require_min_age", "require_max_age",
      "require_sex", "require_min_prior_observation",
      "require_min_future_observation", "new_cohort_definition_id"
    )))
  )

  cli::cli_inform(c("Creating initial cohort"))
  cohort <- cohort |>
    dplyr::inner_join(cdm[[nm]], by = "cohort_definition_id") |>
    dplyr::select(-"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSettings |>
        dplyr::select(-"cohort_definition_id") |>
        dplyr::rename("cohort_definition_id" = "new_cohort_definition_id"),
      cohortAttritionRef = attrition(cohort) |>
        dplyr::inner_join(
          newSettings |>
            dplyr::select("cohort_definition_id", "new_cohort_definition_id"),
          by = "cohort_definition_id"
        ) |>
        dplyr::select(-"cohort_definition_id") |>
        dplyr::rename("cohort_definition_id" = "new_cohort_definition_id"),
      cohortCodelistRef = attr(cohort, "cohort_codelist") |>
        dplyr::collect() |>
        dplyr::inner_join(
          newSettings |>
            dplyr::select("cohort_definition_id", "new_cohort_definition_id"),
          by = "cohort_definition_id"
        ) |>
        dplyr::select(-"cohort_definition_id") |>
        dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")
    )

  for (cond in order) {
    if (cond == "sex") {
      cli::cli_inform(c("Trim sex"))
      cohort <- cohort |>
        dplyr::filter(
          tolower(.data$sex) == .data$require_sex |
            tolower(.data$require_sex) == "both"
        ) |>
        dplyr::select(-c("sex", "require_sex")) |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::recordCohortAttrition("Restrict sex")
    } else if (cond == "age") {
      cli::cli_inform(c("Trim age"))
      cohort <- cohort %>%
        dplyr::mutate(
          !!!caseAge(ageRange),
          "cohort_start_date" = dplyr::if_else(
            .data$cohort_start_date <= .data$new_cohort_start_date,
            .data$new_cohort_start_date,
            .data$cohort_start_date
          ),
          "cohort_end_date" = dplyr::if_else(
            .data$cohort_end_date <= .data$new_cohort_end_date,
            .data$cohort_end_date,
            .data$new_cohort_end_date
          )
        ) |>
        dplyr::select(-c(
          dplyr::starts_with("date_"), "require_min_age", "require_max_age",
          "new_cohort_start_date", "new_cohort_end_date"
        )) |>
        dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::recordCohortAttrition("Trim age_group")
    } else if (cond == "prior_observation") {
      cli::cli_inform(c("Trim prior observation"))
      cohort <- cohort %>%
        dplyr::mutate(
          "new_cohort_start_date" = as.Date(!!CDMConnector::dateadd(
            date = "prior_observation",
            number = "require_min_prior_observation",
            interval = "day"
          )),
          "cohort_start_date" = dplyr::if_else(
            .data$new_cohort_start_date >= .data$cohort_start_date,
            .data$new_cohort_start_date,
            .data$cohort_start_date
          )
        ) |>
        dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
        dplyr::select(-c("require_min_prior_observation", "prior_observation", "new_cohort_start_date")) |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::recordCohortAttrition("Trim prior_observation")
    } else if (cond == "future_observation") {
      cli::cli_inform(c("Trim future observation"))
      cohort <- cohort %>%
        dplyr::filter(
          !!CDMConnector::datediff(
            start = "cohort_start_date",
            end = "future_observation",
            interval = "day"
          ) >=
            .data$require_min_future_observation
        ) |>
        dplyr::select(-c("require_min_future_observation", "future_observation")) |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::recordCohortAttrition("Require future_observation")
    }
  }

  # TODO update attrition names to be more coherent with the age groups, sex and so

  cli::cli_inform(c("v" = "Cohort trimmed"))
  return(cohort)
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
getNewSettings <- function(set, cohortId, age, sex, prior, future) {
  if (length(age) == 0) {
    ageId <- NULL
  } else {
    ageId <- seq_along(age)
  }
  if (length(prior) == 0) {
    prior <- NULL
  } else {
    prior <- as.integer(prior)
  }
  if (length(future) == 0) {
    future <- NULL
  } else {
    future <- as.integer(future)
  }
  sets <- tidyr::expand_grid(
    "cohort_definition_id" = cohortId,
    "require_age" = ageId,
    "require_sex" = sex,
    "require_min_prior_observation" = prior,
    "require_min_future_observation" = future
  )
  if (!is.null(ageId)) {
    ageMin <- lapply(age, function(x) {x[1]}) |> unlist()
    ageMax <- lapply(age, function(x) {x[2]}) |> unlist()
    sets <- sets |>
      dplyr::inner_join(
        dplyr::tibble(
          "require_age" = ageId,
          "require_min_age" = ageMin,
          "require_max_age" = ageMax
        ),
        by = "require_age"
      ) |>
      dplyr::select(-"require_age")
  }
  sets <- sets |>
    dplyr::select(dplyr::any_of(c(
      "cohort_definition_id", "require_min_age", "require_max_age",
      "require_sex", "require_min_prior_observation",
      "require_min_future_observation"
    )))
  sets <- set |>
    dplyr::inner_join(
      sets, by = "cohort_definition_id", suffix = c(".original", "")
    ) |>
    dplyr::mutate("new_cohort_definition_id" = dplyr::row_number())
  if (!is.null(age)) {
    sets <- sets |>
      dplyr::mutate("cohort_name" = paste0(
        .data$cohort_name, "_", .data$require_min_age, "_",
        .data$require_max_age
      ))
  }
  if (!is.null(sex)) {
    sets <- sets |>
      dplyr::mutate("cohort_name" = paste0(
        .data$cohort_name, "_", .data$require_sex
      ))
  }
  if (!is.null(prior)) {
    sets <- sets |>
      dplyr::mutate("cohort_name" = paste0(
        .data$cohort_name, "_", .data$require_min_prior_observation
      ))
  }
  if (!is.null(future)) {
    sets <- sets |>
      dplyr::mutate("cohort_name" = paste0(
        .data$cohort_name, "_", .data$require_min_future_observation
      ))
  }
  sets <- sets |>
    # we will need a new release of omopgenerics so we can change ::: -> ::
    dplyr::mutate("cohort_name" = omopgenerics:::toSnakeCase(.data$cohort_name))
  return(sets)
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
    prepareColStart("require_min_age")
  ageMax <- lapply(age, function(x){x[2]}) |>
    prepareColEnd("require_max_age")
  c(ageMin, ageMax)
}
