#' Generate cohort from the union of different cohorts
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
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
    # update properly
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohortId) %>%
      dplyr::compute(name = name, temporary = FALSE) %>%
      omopgenerics::newCohortTable(
        cohortSetRef = cohort %>%
          omopgenerics::settings() %>%
          dplyr::filter(.data$cohort_definition_id == .env$cohortId),
        cohortAttritionRef = cohort %>%
          omopgenerics::attrition() %>%
          dplyr::filter(.data$cohort_definition_id == .env$cohortId),
        .softValidation = TRUE
      )
    return(cohort)
  }

  # union cohort
  newCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    joinOverlap(by = "subject_id", gap = gap) |>
    dplyr::mutate(cohort_definition_id = 1) |>
    dplyr::compute(name = name, temporary = FALSE)

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

  # cohort attrition
  cohAtt <- newCohort |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(number_records = dplyr::n(),
                     number_subjects = dplyr::n_distinct(.data$subject_id)) |>
    dplyr::mutate(
      "reason_id" = 1,
      "reason" = "Initial qualifying events",
      "excluded_records" = 0,
      "excluded_subjects" = 0
    )

  # concept list
  cohCodelist <- attr(cohort, "cohort_codelist")
  if(!is.null(cohCodelist)) {
    cohCodelist <- cohCodelist |> dplyr::mutate("cohort_definition_id" = 1)
  }

  # new cohort
  newCohort <- newCohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohSet,
      cohortAttritionRef = cohAtt,
      cohortCodelistRef = cohCodelist,
      .softValidation = TRUE
    )
  return(newCohort)
}
