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

  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  if (!is.null(ageRange)) {
    qAge <- datesAgeRange(ageRange)
    cohort <- cohort |>
      PatientProfiles::addDateOfBirth(name = "date_0") %>%
      dplyr::mutate(!!!qAge)
  }
  if (!is.null(minPriorObservation) |
      !is.null(minFutureObservation) |
      !is.null(sex)) {
    obs <- !is.null(minPriorObservation) | !is.null(minFutureObservation)
    cohort <- cohort |>
      PatientProfiles::addDemographics(
        age = FALSE,
        sex = !is.null(sex),
        priorObservation = obs,
        priorObservationType = "date",
        futureObservation = obs,
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
      "require_sex", "new_cohort_definition_id"
    )))
  )

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
      cohort <- cohort |>
        dplyr::filter(
          tolower(.data$sex) == tolower(.data$require_sex) |
            tolower(.data$require_sex) == "both"
        ) |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::recordCohortAttrition("Restrict sex")
    } else if (cond == "age") {
      cohort <- cohort |>
        dplyr::mutate(
          "cohort_start_date" = dplyr::if_else(
            .data[["date_", .data$require_min_age]] < .data$cohort_start_date,
            .data$cohort_start_date,
            .data[["date_", .data$require_min_age]]
          )
        )
    }
  }

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
      "as.Date(local(CDMConnector::dateadd('date_0', {val}, interval = 'year')))"
    ) |>
      rlang::parse_expr()
  }
  return(qA)
}
getNewSettings <- function(set, cohortId, age, sex, prior, future) {
  if (length(age) == 0) {
    ageId <- NULL
  } else {
    ageId <- seq_along(age)
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
    dplyr::mutate("cohort_name" = omopgenerics:::toSnakeCase(.data$cohort_name))
  return(sets)
}
