#' Generate a new cohort matched cohort
#'
#' @description
#' `matchCohorts()` generate a new cohort matched to individuals in an
#' existing cohort. Individuals can be matched based on year of birth and sex.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to include. If NULL all cohorts will be
#' considered. Cohorts not included will be removed from the cohort set.
#' @param matchSex Whether to match in sex.
#' @param matchYearOfBirth Whether to match in year of birth.
#' @param ratio Number of allowed matches per individual in the target cohort.
#' @param name Name of the new generated cohort set.
#'
#' @return A cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' library(dplyr)
#' cdm <- mockCohortConstructor(nPerson = 200)
#' cdm$new_matched_cohort <- cdm$cohort2 |>
#'   matchCohorts(
#'     name = "new_matched_cohort",
#'     cohortId = 2,
#'     matchSex = TRUE,
#'     matchYearOfBirth = TRUE,
#'     ratio = 1)
#' cdm$new_matched_cohort
#' }
matchCohorts <- function(cohort,
                         cohortId = NULL,
                         matchSex = TRUE,
                         matchYearOfBirth = TRUE,
                         ratio = 1,
                         name = tableName(cohort)) {
  cli::cli_inform("Starting matching")

  # validate initial input
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertNumeric(ratio, min = 0, length = 1)
  assertLogical(matchSex, length = 1)
  assertLogical(matchYearOfBirth, length = 1)

  # Check if there are repeated people within the cohort
  y <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::filter(dplyr::n() >= 2) |>
    dplyr::ungroup() |>
    dplyr::tally() |>
    dplyr::pull()
  if (y != 0) {
    cli::cli_warn(
      "Multiple records per person detected. The matchCohorts() function is designed to operate under the assumption that there is only one record per person within each cohort. If this assumption is not met, each record will be treated independently. As a result, the same individual may be matched multiple times, leading to inconsistent and potentially misleading results."
    )
  }

  # table prefix
  tablePrefix <- omopgenerics::tmpPrefix()
  target <- omopgenerics::uniqueTableName(tablePrefix)
  control <- omopgenerics::uniqueTableName(tablePrefix)

  if (cohort |> settings() |> nrow() == 0) {
    cdm[[name]] <- cohort |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::newCohortTable(.softValidation = TRUE)
    return(cdm[[name]])
  }

  # create target cohort
  cli::cli_inform(c("i" = "Creating copy of target cohort."))
  cdm[[target]] <- subsetCohorts(
    cohort = cohort,
    cohortId = cohortId,
    name = target
  )

  # get target cohort id
  cohortId <- cdm[[target]] |> settings() |> dplyr::pull("cohort_definition_id")
  cli::cli_inform(c("*" = "{length(cohortId)} cohort{?s} to be matched."))

  # Create match cohort
  cli::cli_inform(c("i" = "Creating controls cohorts."))
  cdm[[control]] <- getNewCohort(cdm[[target]], cohortId, control)

  # Exclude cases from controls
  cli::cli_inform(c("i" = "Excluding cases from controls"))
  cdm[[control]] <- excludeCases(cdm, target, control)

  # get matched tables
  matchCols <- getMatchCols(matchSex, matchYearOfBirth)

  # Exclude individuals without any match
  cli::cli_inform(c("*" = "Matching by {matchCols}"))
  cdm <- excludeNoMatchedIndividuals(cdm, target, control, matchCols, tablePrefix)

  # Match as ratio was infinite
  cdm <- infiniteMatching(cdm, target, control)

  # Delete controls that are not in observation
  cli::cli_inform(c("*" = "Removing controls that were not in observation at index date"))
  cdm[[control]] <- observationControl(cdm[[control]])
  cli::cli_inform(c("*" = "Excluding target records whose pair is not in observation"))
  cdm[[target]] <- observationTarget(cdm, target, control)

  # Check ratio
  cli::cli_inform(c("*" = "Adjusting ratio"))
  cdm[[control]] <- checkRatio(cdm[[control]], ratio)

  # update settings
  cdm[[control]] <- cdm[[control]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[control]]) |>
        dplyr::select("cohort_definition_id", "cohort_name") |>
        dplyr::mutate(
          "target_table_name" = omopgenerics::tableName(cohort),
          "target_cohort_id" = .data$cohort_definition_id,
          "match_sex" = .env$matchSex,
          "match_year_of_birth" = .env$matchYearOfBirth,
          "match_status" = "control"
        ) |>
        dplyr::left_join(
          settings(cdm[[target]]) |>
            dplyr::select("cohort_definition_id", "target_cohort_name" = "cohort_name"),
          by = "cohort_definition_id"
        ) |>
        dplyr::select(
          "cohort_definition_id",
          "cohort_name",
          "target_table_name",
          "target_cohort_id",
          "target_cohort_name",
          "match_sex",
          "match_year_of_birth",
          "match_status"
        )
      ,
      .softValidation = TRUE
    )
  cdm[[target]] <- cdm[[target]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[target]]) |>
        dplyr::select("cohort_definition_id", "cohort_name") |>
        dplyr::mutate(
          "target_table_name" = omopgenerics::tableName(cohort),
          "target_cohort_id" = .data$cohort_definition_id,
          "target_cohort_name" = .data$cohort_name,
          "match_sex" = .env$matchSex,
          "match_year_of_birth" = .env$matchYearOfBirth,
          "match_status" = "target"
        )
      ,
      .softValidation = TRUE
    )

  # Bind both cohorts
  cli::cli_inform(c("Binding both cohorts"))
  cdm <- omopgenerics::bind(cdm[[target]], cdm[[control]], name = name)

  # drop tmp tables
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  # Return
  cli::cli_inform(c("v" = "Done"))
  return(cdm[[name]])
}

getNewCohort <- function(cohort, cohortId, control) {
  cdm <- omopgenerics::cdmReference(cohort)
  # Create controls cohort
  temp_name <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = temp_name,
    table = dplyr::tibble("cohort_definition_id" = cohortId)
  )
  controls <- cdm[[temp_name]] %>%
    dplyr::cross_join(
      cdm[["person"]] %>%
        dplyr::select("subject_id" = "person_id") |>
        dplyr::inner_join(
          cdm[["observation_period"]] |>
            dplyr::select(
              "subject_id" = "person_id",
              "cohort_start_date" = "observation_period_start_date",
              "cohort_end_date" = "observation_period_end_date"
            ) |>
            dplyr::group_by(.data$subject_id) |>
            dplyr::filter(
              .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)
            ) |>
            dplyr::ungroup(),
          by = "subject_id"
        )
    ) %>%
    dplyr::compute(name = control, temporary = FALSE)
  cdm <- omopgenerics::dropTable(cdm, temp_name)

  controls <- controls |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cohort) |>
        dplyr::mutate("cohort_name" = paste0(.data$cohort_name, "_matched")),
      cohortAttritionRef = dplyr::tibble(
        "cohort_definition_id" = as.integer(cohortId),
        "number_records" = controls |> dplyr::tally() |> dplyr::pull(),
        "number_subjects" = controls |>
          dplyr::summarise(dplyr::n_distinct(.data$subject_id)) |>
          dplyr::pull(),
        "reason_id" = 1L,
        "reason" = "First observation per person",
        "excluded_records" = 0L,
        "excluded_subjects" = 0L
      ),
      cohortCodelistRef = NULL,
      .softValidation = TRUE
    )

  return(controls)
}

excludeCases <- function(cdm, target, control) {
  cdm[[control]] |>
    dplyr::anti_join(cdm[[target]], by = c("subject_id", "cohort_definition_id")) |>
    dplyr::compute(name = control, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition("Exclude cases from controls")
}

getMatchCols <- function(matchSex, matchYearOfBirth) {
  # Obtain matched columns
  matchCols <- c()
  if (matchSex) {
    matchCols <- append(matchCols, "gender_concept_id")
  }
  if (matchYearOfBirth) {
    matchCols <- append(matchCols, "year_of_birth")
  }
  return(matchCols)
}

addMatchCols <- function(x, matchCols) {
  x |>
    dplyr::left_join(
      omopgenerics::cdmReference(x)[["person"]] |>
        dplyr::select("subject_id" = "person_id", dplyr::all_of(matchCols)),
      by = c("subject_id")
    ) |>
    dplyr::compute(name = omopgenerics::tableName(x), temporary = FALSE)
}
excludeIndividualsWithNoMatch <- function(cohort, groups, matchCols) {
  cohort %>%
    dplyr::inner_join(groups, by = c("cohort_definition_id", matchCols)) %>%
    dplyr::select(!dplyr::all_of(matchCols)) |>
    dplyr::compute(name = tableName(cohort), temporary = FALSE) |>
    omopgenerics::recordCohortAttrition("Exclude individuals that do not have any match")
}
excludeNoMatchedIndividuals <- function(cdm,
                                        target,
                                        control,
                                        matchCols,
                                        tablePrefix) {
  # add columns to match
  cdm[[target]] <- cdm[[target]] |> addMatchCols(matchCols)
  cdm[[control]] <- cdm[[control]] |> addMatchCols(matchCols)

  # create groups
  groups <- cdm[[target]] |>
    dplyr::select("cohort_definition_id", dplyr::all_of(matchCols)) |>
    dplyr::distinct() |>
    dplyr::inner_join(
      cdm[[control]] |>
        dplyr::select("cohort_definition_id", dplyr::all_of(matchCols)) |>
        dplyr::distinct(),
      by = c("cohort_definition_id", matchCols)
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(
      "cohort_definition_id", matchCols
    )))) |>
    dplyr::mutate("group_id" = dplyr::row_number()) |>
    dplyr::arrange() |>
    dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix),
                   temporary = FALSE)

  # Exclude individuals that do not have any match
  cdm[[target]] <- cdm[[target]] %>%
    excludeIndividualsWithNoMatch(groups, matchCols)
  cdm[[control]] <- cdm[[control]] %>%
    excludeIndividualsWithNoMatch(groups, matchCols)

  return(cdm)
}

addRandPairId <- function(x) {
  x %>%
    dplyr::mutate("id" = stats::runif(n = dplyr::n())) %>%
    dplyr::arrange(.data$id) %>%
    dplyr::mutate("pair_id" = dplyr::row_number(), .by = "group_id") %>%
    dplyr::select(-"id") %>%
    dplyr::compute(name = omopgenerics::tableName(x), temporary = FALSE)
}
addClusterId <- function(x, u) {
  x |>
    dplyr::inner_join(u, by = c("pair_id", "group_id")) |>
    dplyr::select(-"pair_id", -"group_id") |>
    dplyr::compute(name = omopgenerics::tableName(x), temporary = FALSE)
}
clusterId <- function(x) {
  x |>
    dplyr::select("group_id", "pair_id") |>
    dplyr::distinct() |>
    dplyr::arrange(.data$group_id, .data$pair_id) |>
    dplyr::mutate("cluster_id" = dplyr::row_number())
}
infiniteMatching <- function(cdm, target, control) {
  # Create pair id to perform a random match
  cdm[[target]] <- cdm[[target]] %>% addRandPairId()
  cdm[[control]] <- cdm[[control]] %>% addRandPairId()

  cdm[[control]] <- cdm[[control]] %>%
    dplyr::inner_join(
      # Calculate the maximum number of cases per group
      cdm[[target]] %>%
        dplyr::group_by(.data$group_id) %>%
        dplyr::summarise(
          "max_id" = max(.data$pair_id, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("group_id")
    ) %>%
    # Calculate the maximum ratio per group
    dplyr::mutate("pair_id" = ((.data$pair_id - 1) %% .data$max_id) + 1) %>%
    dplyr::select(-"max_id") %>%
    dplyr::compute(name = control, temporary = FALSE)

  clusterId <- clusterId(cdm[[target]])
  cdm[[control]] <- cdm[[control]] |> addClusterId(clusterId)
  cdm[[target]] <- cdm[[target]] |> addClusterId(clusterId)

  # assign cohort_start_date and cohort end date to controls
  cdm[[control]] <- cdm[[control]] %>%
    dplyr::inner_join(
      # Cohort start date and end date of cases
      cdm[[target]] %>%
        dplyr::select("cluster_id", "index_date" = "cohort_start_date"),
      by = c("cluster_id")
    ) %>%
    dplyr::compute(name = control, temporary = FALSE)

  return(cdm)
}

observationControl <- function(x) {
  cdm <- omopgenerics::cdmReference(x)
  x |>
    dplyr::select(-"cohort_start_date", -"cohort_end_date") |>
    dplyr::rename("cohort_start_date" = "index_date") |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          "subject_id" = "person_id",
          "observation_period_start_date",
          "cohort_end_date" = "observation_period_end_date"
        ),
      by = "subject_id"
    ) |>
    dplyr::filter(
      .data$cohort_start_date <= .data$cohort_end_date &
        .data$cohort_start_date >= .data$observation_period_start_date
    ) |>
    dplyr::select(-"observation_period_start_date") |>
    dplyr::compute(name = tableName(x), temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(reason = "Exclude individuals not in observation")
}
observationTarget <- function(cdm, target, control) {
  cdm[[target]] |>
    dplyr::inner_join(cdm[[control]] |> dplyr::select("cluster_id") |> dplyr::distinct(), by = "cluster_id") |>
    dplyr::compute(name = target, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(reason = "No possible pairs in observation")
}

checkRatio <- function(x, ratio) {
  if (!is.infinite(ratio)) {
    x <- x %>%
      dplyr::mutate("id" = stats::runif(n = dplyr::n())) %>%
      dplyr::group_by(.data$cluster_id) %>%
      dplyr::arrange(.data$id) %>%
      dplyr::filter(dplyr::row_number() <= .env$ratio) %>%
      dplyr::ungroup() %>%
      dplyr::arrange() |>
      dplyr::select(-"id") |>
      dplyr::compute(name = tableName(x), temporary = FALSE) %>%
      omopgenerics::recordCohortAttrition("Exclude individuals to fulfil the ratio")
  }
  return(x)
}
