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

  cols <- unique(
    c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date",
      indexDate
    )
  )

  window_start <- window[[1]][1]
  window_end <- window[[1]][2]

  subsetName <- omopgenerics::uniqueTableName()
  subsetCohort <- cohort |>
    dplyr::select(dplyr::all_of(.env$cols)) |>
    PatientProfiles::addDeathFlag(
      indexDate = indexDate,
      censorDate = censorDate,
      window = window,
      deathFlagName = "death",
      name = subsetName
    )

  if (isFALSE(negate)) {
    subsetCohort <- subsetCohort |>
      dplyr::filter(.data$death == 1 |
                      (!.data$cohort_definition_id %in% cohortId)) |>
      dplyr::select(!"death") |>
      dplyr::compute(name = subsetName, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireDeathFlag_negateFalse_")
    # attrition reason
    reason <- glue::glue("Death between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else {
    # ie require absence instead of presence
    subsetCohort <- subsetCohort |>
      dplyr::filter(.data$death != 1 |
                      (!.data$cohort_definition_id %in% cohortId)) |>
      dplyr::select(!"death") |>
      dplyr::compute(name = subsetName, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireDeathFlag_negateTrue_")
    # attrition reason
    reason <- glue::glue("Alive between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  }

  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }

  x <- cohort |>
    dplyr::inner_join(subsetCohort, by = c(cols)) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_requireDeathFlag_join_") |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  omopgenerics::dropTable(cdm = cdm, name = subsetName)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = x,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(x)
}
