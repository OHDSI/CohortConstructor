#' Generate a new cohort matched cohort from a preexisting target cohort. The
#' new cohort will contain individuals not included in the target cohort with
#' same year of birth (matchYearOfBirth = TRUE) and same sex (matchSex = TRUE).
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Cohort definition id to match from the target cohort.
#' If NULL all the cohort definition id present in the target cohort will be
#' matched.
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
#' library(PatientProfiles)
#' library(CohortConstructor)
#' library(dplyr)
#' cdm <- mockPatientProfiles(
#'   person = tibble(
#'     person_id = 1:3,
#'     gender_concept_id = c(8532, 8532, 8507),
#'     year_of_birth = 1992,
#'     month_of_birth = 1,
#'     day_of_birth = 1,
#'     race_concept_id = 0,
#'     ethnicity_concept_id = 0
#'   ),
#'   observation_period = tibble(
#'     observation_period_id = 1:3,
#'       person_id = 1:3,
#'       observation_period_start_date = as.Date("1950-01-01"),
#'       observation_period_end_date = as.Date("2023-01-01"),
#'       period_type_concept_id = 0
#'   )
#' )
#' cdm$new_matched_cohort <- cdm$cohort2 %>%
#'   matchCohorts(
#'     name = "new_matched_cohort",
#'     cohortId = 2,
#'     matchSex = TRUE,
#'     matchYearOfBirth = TRUE,
#'     ratio = 1)
#' cdm$new_matched_cohort
#'
matchCohorts <- function(cohort,
                         cohortId = NULL,
                         matchSex = TRUE,
                         matchYearOfBirth = TRUE,
                         ratio = 1,
                         name = omopgenerics::tableName(cohort)) {
  cli::cli_inform("Starting matching")

  # validate initial input
  cdm <- omopgenerics::cdmReference(cohort)
  targetCohortName <- omopgenerics::tableName(cohort)
  validateInput(
    cdm = cdm, name = name, targetCohortName = targetCohortName,
    cohortId = cohortId, matchSex = matchSex,
    matchYearOfBirth = matchYearOfBirth, ratio = ratio
  )

  # table prefix
  tablePrefix <- omopgenerics::tmpPrefix()
  target <- omopgenerics::uniqueTableName(tablePrefix)
  control <- omopgenerics::uniqueTableName(tablePrefix)

  if (cohort |> settings() |> nrow() == 0) {
    cdm[[name]] <- cdm[[targetCohortName]] |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::newCohortTable()
    return(cdm[[name]])
  }

  # create target cohort
  cli::cli_inform(c("i" = "Creating copy of target cohort."))
  cdm[[target]] <- subsetCohorts(
    cohort = cohort, cohortId = cohortId, name = target
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
  cli::cli_inform(c("*" = "Removing pairs that were not in observation at index date"))
  cdm[[control]] <- observationControl(cdm[[control]])
  cli::cli_inform(c("*" = "Excluding target"))
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
          "target_table_name" = .env$targetCohortName,
          "target_cohort_id" = .data$cohort_definition_id,
          "match_sex" = .env$matchSex,
          "match_year_of_birth" = .env$matchYearOfBirth,
          "match_status" = "control"
        ) |>
        dplyr::left_join(
          settings(cdm[[target]]) |>
            dplyr::select(
              "cohort_definition_id", "target_cohort_name" = "cohort_name"
            ),
          by = "cohort_definition_id"
        ) |>
        dplyr::select(
          "cohort_definition_id", "cohort_name", "target_table_name",
          "target_cohort_id", "target_cohort_name", "match_sex",
          "match_year_of_birth", "match_status"
        )
      ,
      .softValidation = TRUE
    )
  cdm[[target]] <- cdm[[target]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[target]]) |>
        dplyr::select("cohort_definition_id", "cohort_name") |>
        dplyr::mutate(
          "target_table_name" = .env$targetCohortName,
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

#' @noRd
validateInput <- function(cdm,
                          name,
                          targetCohortName,
                          cohortId,
                          matchSex,
                          matchYearOfBirth,
                          ratio) {
  errorMessage <- checkmate::makeAssertCollection()
  # Check cdm class
  data_check   <- any("cdm_reference" == class(cdm))
  checkmate::assertTRUE(data_check, add = errorMessage)
  if(!isTRUE(data_check)){
    errorMessage$push(glue::glue("- cdm input must be a cdm object"))
  }
  # Check if targetCohortName is a character
  targetCohortName_format_check <- any(class(targetCohortName) %in% c("character"))
  checkmate::assertTRUE(targetCohortName_format_check, add = errorMessage)
  if(!isTRUE(targetCohortName_format_check)){
    errorMessage$push(glue::glue("- targetCohortName input must be a string"))
  }
  # Check if targetCohortName length
  targetCohortName_length_check <- length(targetCohortName) == 1
  checkmate::assertTRUE(  targetCohortName_length_check, add = errorMessage)
  if(!isTRUE(  targetCohortName_length_check)){
    errorMessage$push(glue::glue("- targetCohortName input must have length equal to 1"))
  }
  # Check if targetCohortName is within the cdm object
  targetCohortName_check <- targetCohortName %in% names(cdm)
  checkmate::assertTRUE(targetCohortName_check, add = errorMessage)
  if(!isTRUE(targetCohortName_check)){
    errorMessage$push(glue::glue("- cdm input has not table named {targetCohortName}"))
  }
  # Check if observation period is within the cdm object
  observation_period_check <- "observation_period" %in% names(cdm)
  checkmate::assertTRUE(observation_period_check , add = errorMessage)
  if(!isTRUE(observation_period_check)){
    errorMessage$push(glue::glue("- cdm input has not table named 'observation_period'"))
  }
  # Check if cohortId is a numeric value
  if(!is.null(cohortId)){
    cohortId_format_check <- any(class(cohortId) %in% c("numeric","double","integer"))
    checkmate::assertTRUE(cohortId_format_check, add = errorMessage)
    if(!isTRUE(cohortId_format_check)){
      errorMessage$push(glue::glue("- cohortId input must be numeric"))
    }
  }
  # Check if cohortId is in the cohort_definition_id
  if(!is.null(cohortId)){
    rows <- cdm[[targetCohortName]] %>% dplyr::filter(.data$cohort_definition_id %in% cohortId) %>% dplyr::tally() %>% dplyr::pull()
    cohortId_check <- rows != 0
    checkmate::assertTRUE(cohortId_check, add = errorMessage)
    if(!isTRUE(cohortId_check)){
      errorMessage$push(glue::glue("- {name} table does not containg '{cohortId}' as a cohort_definition_id"))
    }
  }
  # Check if ratio is > 0
  ratio_check <- ratio > 0
  checkmate::assertTRUE(ratio_check, add = errorMessage)
  if(!isTRUE(ratio_check)){
    errorMessage$push(glue::glue("- ratio parameter must be > 0 "))
  }

  checkmate::reportAssertions(collection = errorMessage)
  return(invisible(TRUE))
}

randomPrefix <- function(n = 5) {
  paste0(
    "temp_", paste0(sample(letters, 5, TRUE), collapse = ""), "_", collapse = ""
  )
}

getNumberOfCohorts <- function(cdm, targetCohortName){
  # Read number of cohorts
  n <- settings(cdm[[targetCohortName]]) %>%
    dplyr::summarise(v = max(.data$cohort_definition_id, na.rm = TRUE)) %>%
    dplyr::pull("v")

  if(is.na(n)){# Empty table, number of cohorts is 0
    n <- 0
  }
  return(n)
}

getcohortId <- function(cdm, cohortId, targetCohortName){
  if(is.null(cohortId)){
    cohortId <-cdm[[targetCohortName]] %>%
      omopgenerics::settings() %>%
      dplyr::arrange(.data$cohort_definition_id) %>%
      dplyr::pull("cohort_definition_id")
  }

  return(cohortId)
}

setInitialControlAttriton <- function(cdm, ids) {
  num_records <- cdm[["person"]] %>%
    dplyr::tally() %>%
    dplyr::pull(.data$n)
  num_subjects <- cdm[["person"]] %>%
    dplyr::distinct(.data$person_id) %>%
    dplyr::tally() %>%
    dplyr::pull(.data$n)
  return(
    dplyr::tibble(
      cohort_definition_id = ids,
      number_records = num_records,
      number_subjects = num_subjects,
      reason_id = 1,
      reason = "Subjects in the database",
      excluded_records = 0,
      excluded_subjects = 0
    )
  )
}

getNewCohort <- function(cohort, cohortId, control){
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
            dplyr::filter(.data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)) |>
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
      cohortCodelistRef = NULL
    )

  return(controls)
}

excludeCases <- function(cdm, target, control){
  cdm[[control]] |>
    dplyr::anti_join(
      cdm[[target]], by = c("subject_id", "cohort_definition_id")
    ) |>
    dplyr::compute(name = control, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition("Exclude cases from controls")
}

getMatchCols <- function(matchSex, matchYearOfBirth){
  # Obtain matched columns
  matchCols <- c()
  if(matchSex){
    matchCols <- append(matchCols, "gender_concept_id")
  }
  if(matchYearOfBirth){
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
    dplyr::inner_join(
      groups, by = c("cohort_definition_id", matchCols)
    ) %>%
    dplyr::select(!dplyr::all_of(matchCols)) |>
    dplyr::compute(name = omopgenerics::tableName(cohort), temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(
      "Exclude individuals that do not have any match"
    )
}
excludeNoMatchedIndividuals <- function(cdm, target, control, matchCols, tablePrefix) {
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
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
    )

  # Exclude individuals that do not have any match
  cdm[[target]] <- cdm[[target]] %>%
    excludeIndividualsWithNoMatch(groups, matchCols)
  cdm[[control]] <- cdm[[control]] %>%
    excludeIndividualsWithNoMatch(groups, matchCols)

  return(cdm)
}

addRandPairId <- function(x) {
  x %>%
    dplyr::mutate("id" = stats::runif()) %>%
    dplyr::group_by(.data$group_id) %>%
    dplyr::arrange(.data$id) %>%
    dplyr::mutate("pair_id" = dplyr::row_number()) %>%
    dplyr::select(-"id") %>%
    dplyr::ungroup() %>%
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
infiniteMatching <- function(cdm, target, control){
  # Create pair id to perform a random match
  cdm[[target]] <- cdm[[target]] %>% addRandPairId()
  cdm[[control]] <- cdm[[control]] %>% addRandPairId()

  cdm[[control]] <- cdm[[control]] %>%
    dplyr::inner_join(
      # Calculate the maximum number of cases per group
      cdm[[target]] %>%
        dplyr::group_by(.data$group_id) %>%
        dplyr::summarise(
          "max_id" = max(.data$pair_id, na.rm = TRUE), .groups = "drop"
        ),
      by = c("group_id")
    ) %>%
    # Calculate the maximum ratio per group
    dplyr::mutate("pair_id" = ((.data$pair_id-1) %% .data$max_id) + 1) %>%
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
    omopgenerics::recordCohortAttrition(
      reason = "Exclude individuals not in observation"
    )
}
observationTarget <- function(cdm, target, control) {
  cdm[[target]] |>
    dplyr::inner_join(
      cdm[[control]] |> dplyr::select("cluster_id") |> dplyr::distinct(),
      by = "cluster_id"
    ) |>
    dplyr::compute(name = target, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(
      reason = "No possible pairs in observation"
    )
}

checkRatio <- function(x, ratio) {
  if (!is.infinite(ratio)) {
    x <- x %>%
      dplyr::mutate("id" = stats::runif()) %>%
      dplyr::group_by(.data$cluster_id) %>%
      dplyr::arrange(.data$id) %>%
      dplyr::filter(dplyr::row_number() <= .env$ratio) %>%
      dplyr::ungroup() %>%
      dplyr::arrange() |>
      dplyr::select(-"id") |>
      dplyr::compute(name = tableName(x), temporary = FALSE) %>%
      omopgenerics::recordCohortAttrition(
        "Exclude individuals to fulfil the ratio"
      )
  }
  return(x)
}

