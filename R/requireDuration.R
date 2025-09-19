
#' Require cohort entries last for a certain number of days
#'
#' @description
#' `requireDuration()` filters cohort records, keeping only those which last
#' for the specified amount of days
#'
#' @inheritParams cohortDoc
#' @param daysInCohort Number of days cohort entries must last. Can be a vector
#' of length two if a range, or a vector of length one if a specific number
#' of days
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#'
#' @return The cohort table with any cohort entries that last less or more than
#' the required duration dropped
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#' cdm$cohort1 |>
#'   requireDuration(daysInCohort = c(1, Inf))
#' }
requireDuration <- function(cohort,
                            daysInCohort,
                            cohortId = NULL,
                            name = tableName(cohort)) {

  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  daysInCohort <- validateDaysInCohort(daysInCohort)

  newCol <- omopgenerics::uniqueTableName()

  IdFilter <- needsIdFilter(cohort, cohortId)

  cohort <- cohort |>
    dplyr::mutate(!!newCol := clock::date_count_between(
      start = .data$cohort_start_date,
      end = .data$cohort_end_date,
      precision = "day"
    ))

  if(length(daysInCohort) == 1){
    if(isTRUE(IdFilter)){
      cohort <- cohort |>
        dplyr::filter((.data[[newCol]] == !!daysInCohort &
                      .data$cohort_definition_id %in% .env$cohortId) |
                      !.data$cohort_definition_id %in% .env$cohortId)
    } else {
      cohort <- cohort |>
        dplyr::filter(.data[[newCol]] == .env$daysInCohort)
    }

    cohort <- cohort |>
      dplyr::select(!dplyr::all_of(newCol)) |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_requireDuration1_"
    ) |>
      omopgenerics::recordCohortAttrition(
        reason = "Keep records with duration of {daysInCohort[1]} days",
        cohortId = cohortId)
  }

  if(length(daysInCohort) == 2){
    # only needs lower filter
    if(daysInCohort[1] > 0 &&
       daysInCohort[2] == Inf){
      if(isTRUE(IdFilter)){
        cohort |>
          dplyr::filter((.data[[newCol]] >= !!daysInCohort[1] &
                          .data$cohort_definition_id %in% .env$cohortId) |
                          !.data$cohort_definition_id %in% .env$cohortId)
      } else {
        cohort <- cohort |>
          dplyr::filter(.data[[newCol]] >= !!daysInCohort[1])
      }
    }
    # only needs upper filter
    if(daysInCohort[1] == 0 &&
       daysInCohort[2] != Inf){
      if(isTRUE(IdFilter)){
        cohort <- cohort |>
          dplyr::filter((.data[[newCol]] <= !!daysInCohort[2] &
                          .data$cohort_definition_id %in% .env$cohortId) |
                          !.data$cohort_definition_id %in% .env$cohortId)
      } else {
        cohort <- cohort |>
          dplyr::filter(.data[[newCol]] <= !!daysInCohort[2])
      }
    }
    # needs lower and upper
    if(daysInCohort[1] > 0 &&
       daysInCohort[2] != Inf){
      if(isTRUE(IdFilter)){
        cohort <- cohort |>
          dplyr::filter((.data[[newCol]] >= !!daysInCohort[1] &
                          .data[[newCol]] <= !!daysInCohort[2] &
                          .data$cohort_definition_id %in% .env$cohortId) |
                          !.data$cohort_definition_id %in% .env$cohortId)
      } else {
        cohort <- cohort |>
          dplyr::filter(.data[[newCol]] >= !!daysInCohort[1] &
                          .data[[newCol]] <= !!daysInCohort[2])
      }
    }

  cohort <- cohort |>
    dplyr::select(!dplyr::all_of(newCol)) |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_requireDuration2_"
    ) |>
    omopgenerics::recordCohortAttrition(
      reason = "Keep records with duration {daysInCohort[1]} to {daysInCohort[2]} days",
      cohortId = cohortId)
  }

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  cohort
}

