#' Generate a new cohort matched cohort
#'
#' @description
#' `matchCohorts()` generate a new cohort matched to individuals in an
#' existing cohort. Individuals can be matched based on year of birth and sex.
#' Matching is done at the record level, so if individuals have multiple
#' cohort entries they can be matched to different individuals for each of their
#' records.
#'
#' Two new cohorts will be created when matching. The first is those
#' cohort entries which were matched ("_sampled" is added to the original
#' cohort name for this cohort). The other is the matches found from the
#' database population ("_matched" is added to the original cohort name
#' for this cohort).
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdSubsetDoc
#' @inheritParams nameDoc
#' @inheritParams keepOriginalCohortsDoc
#' @param matchSex Whether to match in sex.
#' @param matchYearOfBirth Whether to match in year of birth.
#' @param ratio Number of allowed matches per individual in the target cohort.
#' @inheritParams softValidationDoc
#'
#' @return A cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' library(dplyr)
#' cdm <- mockCohortConstructor()
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
                         keepOriginalCohorts = FALSE,
                         name = tableName(cohort),
                         .softValidation = FALSE) {
  cli::cli_inform("Starting matching")

  # validate initial input
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertNumeric(ratio, min = 0, length = 1)
  omopgenerics::assertLogical(matchSex, length = 1)
  omopgenerics::assertLogical(matchYearOfBirth, length = 1)
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning empty cohort as `cohortId` is not valid.")
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  # table prefix
  tablePrefix <- omopgenerics::tmpPrefix()
  target <- omopgenerics::uniqueTableName(tablePrefix)
  control <- omopgenerics::uniqueTableName(tablePrefix)
  if (keepOriginalCohorts) {
    keep <- omopgenerics::uniqueTableName(tablePrefix)
    cdm[[keep]] <- subsetCohorts(cohort, cohortId, name = keep)
  }

  if (cohort |> settings() |> nrow() == 0) {
    cdm[[name]] <- cohort |>
      dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_matchCohorts_relocate_") |>
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
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
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
            dplyr::mutate(
              "cohort_name" = paste0(.data$cohort_name, "_sampled")) |>
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
      .softValidation = .softValidation
    )
  cdm[[target]] <- cdm[[target]] |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[target]]) |>
        dplyr::select("cohort_definition_id", "cohort_name") |>
        dplyr::mutate(
          "cohort_name" = paste0(.data$cohort_name, "_sampled")) |>
        dplyr::mutate(
          "target_table_name" = omopgenerics::tableName(cohort),
          "target_cohort_id" = .data$cohort_definition_id,
          "target_cohort_name" = .data$cohort_name,
          "match_sex" = .env$matchSex,
          "match_year_of_birth" = .env$matchYearOfBirth,
          "match_status" = "target"
        )
      ,
      .softValidation = .softValidation
    )

  # Bind both cohorts
  cli::cli_inform(c("Binding cohorts"))
  cohorts <- list(cdm[[target]], cdm[[control]])
  if (keepOriginalCohorts) cohorts <- c(list(cdm[[keep]]), cohorts)
  cdm <- do.call(omopgenerics::bind, c(cohorts, "name" = name))

  # drop tmp tables
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

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
  controls <- cdm[[temp_name]] |>
    dplyr::cross_join(
      cdm[["person"]] |>
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
    ) |>
    dplyr::compute(name = control, temporary = FALSE,
                   logPrefix = "CohortConstructor_matchCohorts_control_")
  cdm <- omopgenerics::dropSourceTable(cdm, temp_name)

  controls <- controls |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cohort) |>
        dplyr::mutate("cohort_name" = paste0(.data$cohort_name, "_matched")),
      cohortAttritionRef = dplyr::tibble(
        "cohort_definition_id" = as.integer(cohortId),
        "number_records" = controls |> dplyr::tally() |> dplyr::pull() |> as.integer(),
        "number_subjects" = controls |>
          dplyr::summarise(dplyr::n_distinct(.data$subject_id)) |>
          dplyr::pull() |> as.integer(),
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
    dplyr::compute(name = control, temporary = FALSE,
                   logPrefix = "CohortConstructor_excludeCases_") |>
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
    dplyr::compute(name = omopgenerics::tableName(x), temporary = FALSE,
                   logPrefix = "CohortConstructor_addMatchCols_")
}
excludeIndividualsWithNoMatch <- function(cohort, groups, matchCols) {
  cohort |>
    dplyr::inner_join(groups, by = c("cohort_definition_id", matchCols)) |>
    dplyr::select(!dplyr::all_of(matchCols)) |>
    dplyr::compute(name = tableName(cohort), temporary = FALSE,
                   logPrefix = "CohortConstructor_excludeIndividualsWithNoMatch_") |>
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
                   temporary = FALSE,
                   logPrefix = "CohortConstructor_excludeNoMatchedIndividuals_")

  # Exclude individuals that do not have any match
  cdm[[target]] <- cdm[[target]] |>
    excludeIndividualsWithNoMatch(groups, matchCols)
  cdm[[control]] <- cdm[[control]] |>
    excludeIndividualsWithNoMatch(groups, matchCols)

  return(cdm)
}

addRandPairId <- function(x) {
  x |>
    dplyr::mutate("id" = stats::runif(n = dplyr::n())) |>
    dplyr::arrange(.data$id) |>
    dplyr::mutate("pair_id" = dplyr::row_number(), .by = "group_id") |>
    dplyr::select(-"id") |>
    dplyr::compute(name = omopgenerics::tableName(x), temporary = FALSE,
                   logPrefix = "CohortConstructor_addRandPairId_")
}
addClusterId <- function(x, u) {
  x |>
    dplyr::inner_join(u, by = c("pair_id", "group_id")) |>
    dplyr::select(-"pair_id", -"group_id") |>
    dplyr::compute(name = omopgenerics::tableName(x), temporary = FALSE,
                   logPrefix = "CohortConstructor_addClusterId_")
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
  cdm[[target]] <- cdm[[target]] |> addRandPairId()
  cdm[[control]] <- cdm[[control]] |> addRandPairId()

  cdm[[control]] <- cdm[[control]] |>
    dplyr::inner_join(
      # Calculate the maximum number of cases per group
      cdm[[target]] |>
        dplyr::group_by(.data$group_id) |>
        dplyr::summarise(
          "max_id" = max(.data$pair_id, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("group_id")
    ) |>
    # Calculate the maximum ratio per group
    dplyr::mutate("pair_id" = ((.data$pair_id - 1) %% .data$max_id) + 1) |>
    dplyr::select(-"max_id") |>
    dplyr::compute(name = control, temporary = FALSE,
                   logPrefix = "CohortConstructor_infiniteMatching_1_")

  clusterId <- clusterId(cdm[[target]])
  cdm[[control]] <- cdm[[control]] |> addClusterId(clusterId)
  cdm[[target]] <- cdm[[target]] |> addClusterId(clusterId)

  # assign cohort_start_date and cohort end date to controls
  cdm[[control]] <- cdm[[control]] |>
    dplyr::inner_join(
      # Cohort start date and end date of cases
      cdm[[target]] |>
        dplyr::select("cluster_id", "index_date" = "cohort_start_date"),
      by = c("cluster_id")
    ) |>
    dplyr::compute(name = control, temporary = FALSE,
                   logPrefix = "CohortConstructor_infiniteMatching_2_")

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
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(name = tableName(x), temporary = FALSE,
                   logPrefix = "CohortConstructor_observationControl_") |>
    omopgenerics::recordCohortAttrition(reason = "Exclude individuals not in observation")
}
observationTarget <- function(cdm, target, control) {
  cdm[[target]] |>
    dplyr::inner_join(cdm[[control]] |> dplyr::select("cluster_id") |> dplyr::distinct(), by = "cluster_id") |>
    dplyr::compute(name = target, temporary = FALSE,
                   logPrefix = "CohortConstructor_observationTarget_") |>
    omopgenerics::recordCohortAttrition(reason = "No possible pairs in observation")
}

checkRatio <- function(x, ratio) {
  if (!is.infinite(ratio)) {
    x <- x |>
      dplyr::mutate("id" = stats::runif(n = dplyr::n())) |>
      dplyr::group_by(.data$cluster_id) |>
      dplyr::arrange(.data$id) |>
      dplyr::filter(dplyr::row_number() <= .env$ratio) |>
      dplyr::ungroup() |>
      dplyr::arrange() |>
      dplyr::select(-"id") |>
      dplyr::compute(name = tableName(x), temporary = FALSE,
                     logPrefix = "CohortConstructor_checkRatio_") |>
      omopgenerics::recordCohortAttrition("Exclude individuals to fulfil the ratio")
  }
  return(x)
}
