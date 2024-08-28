#' Collapse cohort entries using a certain gap to concatenate records.
#'
#' @description
#' `collapseCohorts()` concatenates cohort records, allowing for some number
#' of days between one finishing and the next starting.
#'
#' @param cohort A cohort table
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param gap Number of days to use when merging cohort entries.
#' @param name Name of the cohort table.
#'
#' @export
#'
#' @return A cohort table
#'
collapseCohorts <- function(cohort,
                            cohortId = NULL,
                            gap = 0,
                            name = tableName(cohort)) {
  # input validation
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  cohort <- validateCohortTable(cohort, dropExtraColumns = TRUE)
  ids <- settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  if (gap != Inf) {
    gap <- validateGap(gap)
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)

  if (all(ids %in% cohortId)) {
    newCohort <- cohort |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE) |>
      omopgenerics::newCohortTable(.softValidation = TRUE)
  } else {
    tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
    unchangedCohort <- cohort |>
      dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpUnchanged, temporary = FALSE)
    newCohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE)
  }

  if (gap == Inf) {
    newCohort <- newCohort |>
      addObservationPeriodId() |>
      joinAll(by = c(
        "cohort_definition_id",
        "subject_id",
        "observation_period_id"
      ))
  } else if (gap > 0) {
    newCohort <- newCohort |>
      addObservationPeriodId() |>
      joinOverlap(
        name = name,
        gap = gap,
        by = c(
          "cohort_definition_id",
          "subject_id",
          "observation_period_id"
        )
      )
  }

  if (!all(ids %in% cohortId)) {
    newCohort <- unchangedCohort |> dplyr::union_all(newCohort)
  }

  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(
      reason = "Collapse cohort with a gap of {gap} days.",
      cohortId = cohortId)

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(newCohort)
}


addObservationPeriodId <- function(x, indexDate = "cohort_start_date") {
  cdm <- omopgenerics::cdmReference(x)
  xName <- omopgenerics::tableName(x)
  if (!is.na(xName) &&
      omopgenerics::tableName(x) == "observation_period") {
    cli::cli_abort("addObservationPeriodId cannot be used on the observation period table")
  }
  personVariable <- c("person_id", "subject_id")
  personVariable <- personVariable[personVariable %in% colnames(x)]

  # drop variable if it already exists
  if ("observation_period_id" %in% colnames(x)) {
    cli::cli_warn(c("!" = "Existing observation_period_id column will be overwritten"))
    x <- x |>
      dplyr::select(!dplyr::all_of("observation_period_id"))
  }

  # if empty table, return with variable name added
  if (x |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") == 0) {
    return(x |>
             dplyr::mutate(observation_period_id = as.integer(NA)))
  }

  currentObsId <- x |>
    dplyr::select(dplyr::all_of(c(personVariable, indexDate))) |>
    dplyr::left_join(
      cdm$observation_period |>
        dplyr::select(dplyr::all_of(
          c(
            "person_id",
            "observation_period_id",
            "observation_period_start_date",
            "observation_period_end_date"
          )
        )) |>
        dplyr::rename(!!personVariable := "person_id"),
      by = personVariable
    ) |>
    dplyr::filter(.data[[indexDate]] <= .data[["observation_period_end_date"]] &&
                    .data[[indexDate]] >= .data[["observation_period_start_date"]]) |>
    dplyr::select(dplyr::all_of(c(
      personVariable, indexDate, "observation_period_id"
    )))

  x |>
    dplyr::left_join(currentObsId, by = c(personVariable, indexDate))


}
