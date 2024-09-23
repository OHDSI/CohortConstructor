#' Restrict cohort to specific entry
#'
#' @description
#' `requireIsFirstEntry()` filters cohort records, keeping only the first
#' cohort entry per person.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param entryRange Range for entries to include.
#'
#' @return A cohort table in a cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 <- requireIsEntry(cdm$cohort1, c(1, Inf))
#' }
#'
requireIsEntry <- function(cohort,
                           entryRange,
                           cohortId = NULL,
                           name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- validateCohortId(cohortId, settings(cohort))

  omopgenerics::assertNumeric(entryRange, integerish = TRUE, min = 0)
  if (length(entryRange) < 1 || length(entryRange) > 2) {
    cli::cli_abort("entryRange must be lenght 1 or 2")
  }

  if (length(entryRange) == 1) {
    minEntry <- entryRange
    maxEntry <- entryRange
  } else {
    minEntry <- entryRange[1]
    maxEntry <- entryRange[2]
  }

  if (minEntry == 1 && maxEntry == Inf) {
    return(
      cohort |>
        dplyr::compute(name = name, temporary = FALSE) |>
        omopgenerics::newCohortTable(.softValidation = TRUE) |>
        omopgenerics::recordCohortAttrition(
          "Restricted to entries between {minEntry} and {maxEntry}",
          cohortId = cohortId
        )
    )
  }

  cohort <- cohort |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
    dplyr::mutate(entry = dplyr::row_number())

  if (maxEntry == Inf) {
    cohort <- cohort  |>
      dplyr::filter(.data$entry >= {{minEntry}})
  } else {
    cohort <- cohort |>
      dplyr::filter(.data$entry >= {{minEntry}}, .data$entry <= {{maxEntry}})
  }

  cohort <- cohort |>
    dplyr::ungroup() |>
    dplyr::select(!"entry") |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition("Restricted to entries between {minEntry} and {maxEntry}",
                                        cohortId = cohortId)



  return(cohort)
}



#' Restrict cohort to first entry
#'
#' @description
#' `requireIsFirstEntry()` filters cohort records, keeping only the first
#' cohort entry per person.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param name Name of the new cohort with the restriction.
#'
#' @return A cohort table in a cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 <- requireIsFirstEntry(cdm$cohort1)
#' }
#'
requireIsFirstEntry <- function(cohort,
                                cohortId = NULL,
                                name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- validateCohortId(cohortId, settings(cohort))

  cohort <- cohort |>
    dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
    dplyr::filter(
      .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE) |
        (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition("Restricted to first entry", cohortId = cohortId)

  return(cohort)
}

#' Restrict cohort to last entry per person
#'
#' @description
#' `requireIsLastEntry()` filters cohort records, keeping only the last
#' cohort entry per person.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param name Name of the new cohort with the restriction.
#'
#' @return A cohort table in a cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 <- requireIsLastEntry(cdm$cohort1)
#' }
#'
requireIsLastEntry <- function(cohort,
                               cohortId = NULL,
                               name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- validateCohortId(cohortId, settings(cohort))

  cohort <- cohort |>
    dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
    dplyr::filter(
      .data$cohort_start_date == max(.data$cohort_start_date, na.rm = TRUE) |
        (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition("Restricted to last entry", cohortId = cohortId)

  return(cohort)
}
