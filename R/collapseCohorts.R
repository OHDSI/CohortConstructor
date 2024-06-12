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
  gap <- validateGap(gap)

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

  if (gap > 0) {
    newCohort <- newCohort |> joinOverlap(gap = gap)
  }

  if (!all(ids %in% cohortId)) {
    newCohort <- unchangedCohort |> dplyr::union_all(newCohort)
  }

  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(
      reason = "Collapse cohort with a gap of {gap} days.",
      cohortId = cohortId
    )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(newCohort)
}
