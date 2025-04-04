#' Require cohort subjects have (or do not have) a death record
#'
#' @description
#' `requireDeathFlag()` filters a cohort table based on a requirement
#' that an individual is seen (or not seen) to have a death in some time
#' window around an index date.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams windowDoc
#' @inheritParams nameDoc
#' @param indexDate Name of the column in the cohort that contains the date to
#' use as time 0 for window days.
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of the cohort.
#' @param negate If set as TRUE, criteria will be applied as exclusion
#' rather than inclusion (i.e. require absence in another cohort).
#'
#' @return Cohort table with only those with a death event kept (or without
#' if negate = TRUE)
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(death = TRUE)
#' cdm$cohort1 <- cdm$cohort1 |> requireDeathFlag(window = list(c(0, Inf)))
#' attrition(cdm$cohort1)
#' }
requireDeathFlag <- function(cohort,
                             window,
                             cohortId = NULL,
                             indexDate = "cohort_start_date",
                             censorDate = NULL,
                             negate = FALSE,
                             name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  window <- omopgenerics::validateWindowArgument(window)
  omopgenerics::assertLogical(negate, length = 1)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireDeathFlag_entry_")
    return(cdm[[name]])
  }

  window_start <- window[[1]][1]
  window_end <- window[[1]][2]

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  intersectCol <- uniqueColumnName(newCohort)
  newCohort <- newCohort |>
    PatientProfiles::addDeathFlag(
      indexDate = indexDate,
      censorDate = censorDate,
      window = window,
      deathFlagName = intersectCol,
      name = tmpNewCohort
    )

  if (isFALSE(negate)) {
    newCohort <- newCohort |>
      dplyr::filter(.data[[intersectCol]] == 1) |>
      dplyr::select(!dplyr::all_of(intersectCol)) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireDeathFlag_negateFalse_")
    # attrition reason
    reason <- glue::glue("Death between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else {
    # ie require absence instead of presence
    newCohort <- newCohort |>
      dplyr::filter(.data[[intersectCol]] != 1) |>
      dplyr::select(!dplyr::all_of(intersectCol)) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireDeathFlag_negateTrue_")
    # attrition reason
    reason <- glue::glue("Alive between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  }

  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(cdm[[tmpUnchanged]]) |>
      dplyr::compute(
        name = tmpNewCohort, temporary = FALSE,
        logPrefix = "CohortConstructor_requireDeathFlag_union_"
      )
  }

  newCohort <- newCohort |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_requireDeathFlag_name_"
    ) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

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
