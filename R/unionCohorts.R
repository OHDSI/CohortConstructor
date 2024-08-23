#' Generate cohort from the union of different cohorts
#'
#' @description
#' `unionCohorts()` combines different cohort entries, with those records
#' that overlap combined and kept. Cohort entries are when an individual was in
#' _either_ of the cohorts.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to include. If NULL all cohorts will be
#' considered. Cohorts not included will be removed from the cohort set.
#' @param gap Number of days between two subsequent cohort entries of a subject
#' that will be merged in a single cohort entry
#' @param cohortName Name of the returned cohort. If NULL, the cohort name will
#' be created by collapsing the individual cohort names, separated by "_".
#' @param name Name of the new cohort table.
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
                         name = tableName(cohort)) {
  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  assertCharacter(cohortName, length = 1, null = TRUE)

  if (length(cohortId) < 2) {
    cli::cli_warn("At least 2 cohort id must be provided to do the union.")
    # set
    set <- cohort |>
      omopgenerics::settings() |>
      dplyr::filter(.data$cohort_definition_id == .env$cohortId)
    if (is.null(cohortName)) {cohortName <- set$cohort_name}
    # codelist
    cohCodelist <- attr(cohort, "cohort_codelist")
    if(!is.null(cohCodelist)) {
      cohCodelist <- cohCodelist |>
        dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
        dplyr::mutate("cohort_definition_id" = 1)
    }
    # update properly
    cohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
      dplyr::mutate(cohort_definition_id = 1) |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::newCohortTable(
        cohortSetRef = set |>
          dplyr::mutate(cohort_definition_id = 1, cohort_name = cohortName),
        cohortAttritionRef = cohort |>
          omopgenerics::attrition() |>
          dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
          dplyr::mutate(cohort_definition_id = 1),
        cohortCodelistRef = cohCodelist,
        .softValidation = TRUE
      )
    return(cohort)
  }

  # cohort set
  names <- omopgenerics::settings(cohort)|>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::pull("cohort_name")
  if (length(cohortName) == 0) {
    cohortName <- paste0(names, collapse = "_")
  }
  cohSet <- dplyr::tibble(
    cohort_definition_id = 1, cohort_name = cohortName, gap = gap
  )

  # union cohort
  newCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    joinOverlap(name = name,
                by = "subject_id", gap = gap) |>
    dplyr::mutate(cohort_definition_id = 1) |>
    dplyr::compute(name = name, temporary = FALSE)

  # concept list
  cohCodelist <- attr(cohort, "cohort_codelist")
  if(!is.null(cohCodelist)) {
    cohCodelist <- cohCodelist |> dplyr::mutate("cohort_definition_id" = 1)
  }

  # new cohort
  newCohort <- newCohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohCodelist,
      .softValidation = TRUE
    )
  return(newCohort)
}
