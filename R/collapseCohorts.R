#' Collapse cohort entries using a certain gap to concatenate records.
#'
#' @description
#' `collapseCohorts()` concatenates cohort records, allowing for some number
#' of days between one finishing and the next starting.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams gapDoc
#' @inheritParams nameDoc
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
  cohort <- omopgenerics::validateCohortArgument(cohort, dropExtraColumns = TRUE)
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  ids <- settings(cohort)$cohort_definition_id

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE)
    return(cdm[[name]])
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- paste0(omopgenerics::uniqueTableName(tablePrefix), "_1")
  if (all(ids %in% cohortId)) {
    newCohort <- cohort |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_collapseCohorts_newCohort1_") |>
      omopgenerics::newCohortTable(.softValidation = TRUE)
  } else {
    tmpUnchanged <- paste0(omopgenerics::uniqueTableName(tablePrefix), "_2")
    unchangedCohort <- cohort |>
      dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpUnchanged, temporary = FALSE,
                     logPrefix = "CohortConstructor_collapseCohorts_unchangedCohort_")
    newCohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_collapseCohorts_newCohort2_")
  }
  if (gap == Inf) {
    newCohort <- newCohort |>
      PatientProfiles::addObservationPeriodId(name = tmpNewCohort) |>
      joinAll(by = c(
        "cohort_definition_id",
        "subject_id",
        "observation_period_id"
      )) |>
      dplyr::select(!"observation_period_id")
  } else if (gap > 0) {
    newCohort <- newCohort |>
      PatientProfiles::addObservationPeriodId(name = tmpNewCohort) |>
      joinOverlap(
        name = tmpNewCohort,
        gap = gap,
        by = c(
          "cohort_definition_id",
          "subject_id",
          "observation_period_id"
        )
      ) |>
      dplyr::select(!"observation_period_id")
  }
  if (!all(ids %in% cohortId)) {
    newCohort <- unchangedCohort |>
      dplyr::union_all(newCohort)|>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_collapseCohorts_union_")
  } else {
    newCohort <- newCohort |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_collapseCohorts_all_id_")
  }
  newCohort <- newCohort |>
    omopgenerics::newCohortTable(.softValidation = FALSE) |>
    omopgenerics::recordCohortAttrition(
      reason = "Collapse cohort with a gap of {gap} days.",
      cohortId = cohortId)

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}


