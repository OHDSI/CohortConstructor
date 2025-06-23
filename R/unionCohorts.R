#' Generate cohort from the union of different cohorts
#'
#' @description
#' `unionCohorts()` combines different cohort entries, with those records
#' that overlap combined and kept. Cohort entries are when an individual was in
#' _either_ of the cohorts.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdSubsetDoc
#' @inheritParams gapDoc
#' @inheritParams nameDoc
#' @inheritParams keepOriginalCohortsDoc
#' @param cohortName Name of the returned cohort. If NULL, the cohort name will
#' be created by collapsing the individual cohort names, separated by "_".
#' @inheritParams softValidationDoc
#'
#' @export
#'
#' @return A cohort table.
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort2 <- cdm$cohort2 |> unionCohorts()
#' settings(cdm$cohort2)
#'
#' }
unionCohorts <- function(cohort,
                         cohortId = NULL,
                         gap = 0,
                         cohortName = NULL,
                         keepOriginalCohorts = FALSE,
                         name = tableName(cohort),
                         .softValidation = TRUE) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  omopgenerics::assertCharacter(cohortName, length = 1, null = TRUE)
  omopgenerics::assertLogical(keepOriginalCohorts, length = 1)
  omopgenerics::assertLogical(.softValidation)

  if (is.infinite(gap)) {
    cli::cli_abort("`gap` can't be infinite")
  }
  if (length(cohortId) < 2) {
    cli::cli_abort("Settings of cohort table must contain at least two cohorts.")
  }

  if (length(cohortName) == 0) {
    names <- omopgenerics::settings(cohort) |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::pull("cohort_name")
    cohortName <- paste0(names, collapse = "_")
  }
  cohSet <- dplyr::tibble(
    cohort_definition_id = 1L,
    cohort_name = cohortName,
    gap = gap
  )

  if (keepOriginalCohorts) {
    if (any(cohortName %in% omopgenerics::settings(cohort)$cohort_name)) {
      cli::cli_abort("Change `cohortName` as there is already a cohort named `{cohortName}` in the cohort.")
    }
  }
  # union cohort
  # save to a separate table so can append to original cohorts at the end
  tmpTable  <- omopgenerics::uniqueTableName()
  unionedCohort <- copyCohorts(cohort = cohort,
                               name = tmpTable,
                               cohortId = cohortId) |>
    dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    getObservationPeriodId(name = tmpTable) |>
    joinOverlap(name = tmpTable,
                by = c("observation_period_id", "subject_id"),
                gap = gap) |>
    dplyr::select(!"observation_period_id") |>
    dplyr::mutate(cohort_definition_id = 1L) |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(name = tmpTable, temporary = FALSE,
                   logPrefix = "CohortConstructor_unionCohorts_tmpTable_")
  cohCodelist <- attr(cohort, "cohort_codelist")
  if (!is.null(cohCodelist)) {
    cohCodelist <- cohCodelist |> dplyr::mutate("cohort_definition_id" = 1L)
  }

  unionedCohort  <- unionedCohort |>
    dplyr::compute(name = tmpTable, temporary = FALSE,
                   logPrefix = "CohortConstructor_unionCohorts_newCohort_") |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohCodelist,
      .softValidation = .softValidation
    )

  if (isTRUE(keepOriginalCohorts)) {
    cdm <- bind(cohort, unionedCohort, name = name)
  } else {
    cdm[[name]] <- unionedCohort |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_unionCohorts_name_") |>
      omopgenerics::newCohortTable(.softValidation = TRUE)
  }

  CDMConnector::dropTable(cdm, name = dplyr::starts_with(tmpTable))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cdm[[name]])
}
