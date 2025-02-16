#' Sample a cohort table for a given number of individuals.
#'
#' @description
#' `sampleCohorts()` samples an existing cohort table for a given number of
#' people. All records of these individuals are preserved.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param n Number of people to be sampled for each included cohort.
#'
#' @return Cohort table with the specified cohorts sampled.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort2 |> sampleCohorts(cohortId = 1, n = 10)
#' }
sampleCohorts <- function(cohort,
                          n,
                          cohortId = NULL,
                          name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  n <- validateN(n)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_sampleCohorts_entry_")
    useIndexes <- getOption("CohortConstructor.use_indexes")
    if (!isFALSE(useIndexes)) {
      addIndex(
        cohort = cdm[[name]],
        cols = c("subject_id", "cohort_start_date")
      )
    }
    return(cdm[[name]])
  }

  if (all(cohort |>
          dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
          dplyr::distinct(.data$cohort_definition_id, .data$subject_id) |>
          dplyr::group_by(.data$cohort_definition_id) |>
          dplyr::tally() |>
          dplyr::pull() <= n)) {
    cli::cli_inform("Returning entry cohort as the size of the cohorts to be sampled is equal or smaller than `n`.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_sampleCohorts_return_")
    useIndexes <- getOption("CohortConstructor.use_indexes")
    if (!isFALSE(useIndexes)) {
      addIndex(
        cohort = cdm[[name]],
        cols = c("subject_id", "cohort_start_date")
      )
    }
    return(cdm[[name]])
  }

  cdm[[name]] <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::select("subject_id", "cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::slice_sample(n = n) |>
    dplyr::left_join(cohort, by = c("subject_id", "cohort_definition_id")) |>
    dplyr::union_all(cohort |>
      dplyr::filter(!(
        .data$cohort_definition_id %in% .env$cohortId
      ))) |>
    dplyr::ungroup() |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort")))  |>
    dplyr::compute(name = name,
                   temporary = FALSE,
                   logPrefix = "CohortConstructor_sampleCohorts_sample_")

  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::recordCohortAttrition(
      reason = paste0("Sample ", n, " individuals"),
      cohortId = cohortId
    )

  return(cdm[[name]])
}
