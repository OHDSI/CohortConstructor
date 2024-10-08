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
  cohort <- validateCohortTable(cohort, dropExtraColumns = TRUE)
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- validateCohortId(cohortId, settings(cohort))
  omopgenerics::assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  ids <- settings(cohort)$cohort_definition_id

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- paste0(omopgenerics::uniqueTableName(tablePrefix), "_1")
  if (all(ids %in% cohortId)) {
    newCohort <- cohort |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE) |>
      omopgenerics::newCohortTable(.softValidation = TRUE)
  } else {
    tmpUnchanged <- paste0(omopgenerics::uniqueTableName(tablePrefix), "_2")
    unchangedCohort <- cohort |>
      dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpUnchanged, temporary = FALSE)
    newCohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE)
  }
  if (gap == Inf) {
    newCohort <- newCohort |>
      PatientProfiles::addObservationPeriodId() |>
      joinAll(by = c(
        "cohort_definition_id",
        "subject_id",
        "observation_period_id"
      )) |>
      dplyr::select(!"observation_period_id")
  } else if (gap > 0) {
    newCohort <- newCohort |>
      PatientProfiles::addObservationPeriodId() |>
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
      dplyr::compute(name = name, temporary = FALSE)
  } else {
    newCohort <- newCohort |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  newCohort <- newCohort |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(
      reason = "Collapse cohort with a gap of {gap} days.",
      cohortId = cohortId)

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(newCohort)
}


