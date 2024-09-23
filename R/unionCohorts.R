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
#' @param cohortName Name of the returned cohort. If NULL, the cohort name will
#' be created by collapsing the individual cohort names, separated by "_".
#' @param keepOriginalCohorts If TRUE the original cohorts and the newly
#' created union cohort will be returned. If FALSE only the new cohort will be
#' returned.
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
                         name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- validateCohortId(cohortId, settings(cohort))
  omopgenerics::assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  omopgenerics::assertCharacter(cohortName, length = 1, null = TRUE)
  omopgenerics::assertLogical(keepOriginalCohorts, length = 1)

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

  # union cohort
  # save to a separate table so can append to original cohorts at the end
  tmpTable  <- omopgenerics::uniqueTableName()
  unionedCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    joinOverlap(name = name, by = "subject_id", gap = gap) |>
    dplyr::mutate(cohort_definition_id = 1L) |>
    dplyr::compute(name = tmpTable, temporary = FALSE)
  cohCodelist <- attr(cohort, "cohort_codelist")
  if (!is.null(cohCodelist)) {
    cohCodelist <- cohCodelist |> dplyr::mutate("cohort_definition_id" = 1L)
  }
  unionedCohort <- unionedCohort |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohCodelist,
      .softValidation = TRUE
    )

  if (isFALSE(keepOriginalCohorts)) {
    cdm[[name]] <- unionedCohort |>
      dplyr::compute(name = name, temporary = FALSE)
  } else {
    cdm <- bind(cohort, unionedCohort, name = name)
  }


  return(cdm[[name]])
}
