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
#' @inheritParams softValidationDoc
#'
#' @export
#'
#' @return A cohort table
#'
collapseCohorts <- function(cohort,
                            cohortId = NULL,
                            gap = 0,
                            name = tableName(cohort),
                            .softValidation = FALSE) {
  # input validation
  cohort <- omopgenerics::validateCohortArgument(cohort, dropExtraColumns = TRUE)
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  ids <- settings(cohort)$cohort_definition_id
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE)
    return(cdm[[name]])
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]] |>
    dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort")))

  if (gap == Inf) {
    newCohort <- newCohort |>
      getObservationPeriodId(name = tmpNewCohort) |>
      joinAll(by = c(
        "cohort_definition_id",
        "subject_id",
        "observation_period_id"
      )) |>
      dplyr::select(!"observation_period_id")
  } else if (gap >= 0) {
    newCohort <- newCohort |>
      getObservationPeriodId(name = tmpNewCohort) |>
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

  if (isTRUE(needsIdFilter(cohort = cohort, cohortId = cohortId))) {
    newCohort <- cdm[[tmpUnchanged]] |>
      dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
      dplyr::union_all(newCohort)
  }

  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_collapseCohorts_name_") |>
    omopgenerics::newCohortTable(.softValidation = .softValidation) |>
    omopgenerics::recordCohortAttrition(
      reason = "Collapse cohort with a gap of {gap} days.",
      cohortId = cohortId)

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}
