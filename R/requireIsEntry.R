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
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireIsEntry_entry_")
    return(cdm[[name]])
  }

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
    cohort <- cohort |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireIsEntry_restrict_") |>
      omopgenerics::newCohortTable(.softValidation = TRUE) |>
      omopgenerics::recordCohortAttrition(
        "Restricted to entries between {minEntry} and {maxEntry}",
        cohortId = cohortId
      )
    useIndexes <- getOption("CohortConstructor.use_indexes")
    if (!isFALSE(useIndexes)) {
      addIndex(
        cohort = cohort,
        cols = c("subject_id", "cohort_start_date")
      )
    }
    return(cohort)
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
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_requireIsEntry_select_") |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition("Restricted to entries between {minEntry} and {maxEntry}",
                                        cohortId = cohortId)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

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
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireIsFirstEntry_entry_")
    return(cdm[[name]])
  }

  cohort <- cohort |>
    dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
    dplyr::filter(
      .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE) |
        (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_requireIsFirstEntry_min_") |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition("Restricted to first entry", cohortId = cohortId)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cohort)
}

#' Restrict cohort to last entry per person
#'
#' @description
#' `requireIsLastEntry()` filters cohort records, keeping only the last
#' cohort entry per person.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
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
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireIsLastEntry_entry_")
    return(cdm[[name]])
  }

  cohort <- cohort |>
    dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
    dplyr::filter(
      .data$cohort_start_date == max(.data$cohort_start_date, na.rm = TRUE) |
        (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_requireIsLastEntry_max_") |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition("Restricted to last entry", cohortId = cohortId)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cohort)
}
