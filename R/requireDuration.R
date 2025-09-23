
#' Require cohort entries last for a certain number of days
#'
#' @description
#' `requireDuration()` filters cohort records, keeping only those which last
#' for the specified amount of days
#'
#' @inheritParams cohortDoc
#' @param daysInCohort Number of days cohort entries must last. Can be a vector
#' of length two if a range, or a vector of length one if a specific number
#' of days. Note, cohort entry and exit on the same day counts as one day in
#' the cohort. So if, for example, you wish to require individuals are in the
#' cohort for at least one night then set daysInCohort to c(2, Inf). Meanwhile,
#'  if set to c(30, 90) then only cohort entries that are 30 days or more
#'  longer and shorter or equal to 90 days will be kept.
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
#'   requireDuration(daysInCohort = c(2, Inf))
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

  # we consider same day entry and exit to count as one day
  startDaysInCohort <- daysInCohort
  daysInCohort[1] <- daysInCohort[1] - 1L
  if(daysInCohort[2] != Inf){
    daysInCohort[2] <- daysInCohort[2] - 1L
  }

  cohort <- cohort |>
    dplyr::mutate(!!newCol := clock::date_count_between(
      start = .data$cohort_start_date,
      end = .data$cohort_end_date,
      precision = "day"
    ))

    # only needs lower filter
    if(daysInCohort[1] > 0 &&
       daysInCohort[2] == Inf){
      if(isTRUE(IdFilter)){
        cohort <- cohort |>
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
      reason = "Keep records with duration {startDaysInCohort[1]} to {startDaysInCohort[2]} days",
      cohortId = cohortId)


  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  cohort
}

#' Trim cohort dates to be within a certain interval of days
#'
#' @description
#' `trimDuration()` resets the cohort start and end date, keeping only those
#' which include the specified amount of days
#'
#' @inheritParams cohortDoc
#' @param daysInCohort Number of days cohort relative to current cohort start
#' dates. Cohort entries will be trimmed to these dates. Note, cohort entry and
#' exit on the same day counts as one day in the cohort.Set lower bound
#' to 1 if keeping cohort start to the same as current cohort start.
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
#'   requireDuration(daysInCohort = c(2, Inf))
#' }
trimDuration <- function(cohort,
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

  # we consider same day entry and exit to count as one day
  startDaysInCohort <- daysInCohort
  daysInCohort[1] <- daysInCohort[1] - 1L
  if(daysInCohort[2] != Inf){
  daysInCohort[2] <- daysInCohort[2] - 1L
  }

    # only needs lower mutate
    if(daysInCohort[1] > 0 &&
       daysInCohort[2] == Inf){
      if(isTRUE(IdFilter)){
       cohort <- cohort |>
          dplyr::mutate(cohort_start_date = dplyr::if_else(
            .data$cohort_definition_id %in% !!cohortId,
            as.Date(clock::add_days(
              "cohort_start_date",
              !!daysInCohort[1])),
              cohort_start_date
          ))
      } else {
        cohort <- cohort |>
          dplyr::mutate(cohort_start_date = as.Date(clock::add_days(
            "cohort_start_date",
            !!daysInCohort[1]
          )))
      }
    }
    # only needs upper mutate
    if(daysInCohort[1] == 0 &&
       daysInCohort[2] != Inf){
      if(isTRUE(IdFilter)){
        cohort <- cohort |>
          dplyr::mutate(!!newCol :=  as.Date(clock::add_days(
            "cohort_start_date",
            !!daysInCohort[2]
          ))) |>
          dplyr::mutate(cohort_end_date = dplyr::if_else(
            (.data[[newCol]] < .data$cohort_end_date &
            .data$cohort_definition_id %in% !!cohortId),
            .data[[newCol]], .data$cohort_end_date
          ))
      } else {
        cohort <- cohort |>
          dplyr::mutate(!!newCol :=  as.Date(clock::add_days(
            "cohort_start_date",
            !!daysInCohort[2]
          ))) |>
          dplyr::mutate(cohort_end_date = dplyr::if_else(
            .data[[newCol]] < .data$cohort_end_date,
            .data[[newCol]], .data$cohort_end_date
          ))
      }
    }
    # needs lower and upper
    if(daysInCohort[1] > 0 &&
       daysInCohort[2] != Inf){
      if(isTRUE(IdFilter)){
        cohort <- cohort |>
          dplyr::mutate(cohort_start_date = dplyr::if_else(
            .data$cohort_definition_id %in% !!cohortId,
            as.Date(clock::add_days(
              "cohort_start_date",
              !!daysInCohort[1])),
            cohort_start_date),
            !!newCol := dplyr::if_else(
            .data$cohort_definition_id %in% !!cohortId,
            as.Date(clock::add_days(
              "cohort_start_date",
              !!daysInCohort[2])),
            cohort_end_date
          ))|>
          dplyr::mutate(cohort_end_date = dplyr::if_else(
            .data[[newCol]] < .data$cohort_end_date,
            .data[[newCol]], .data$cohort_end_date
          ))
      } else {
        cohort <- cohort |>
          dplyr::mutate(
            cohort_start_date :=  as.Date(clock::add_days(
              "cohort_start_date",
              !!daysInCohort[1]
            )),
            !!newCol :=  as.Date(clock::add_days(
            "cohort_start_date",
            !!daysInCohort[2]
          ))) |>
          dplyr::mutate(cohort_end_date = dplyr::if_else(
            .data[[newCol]] < .data$cohort_end_date,
            .data[[newCol]], .data$cohort_end_date
          ))
      }
    }

  cohort <- cohort |>
    dplyr::select(!dplyr::any_of(newCol)) |>
    dplyr::filter(.data$cohort_start_date <=
                    .data$cohort_end_date) |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_trimDuration_"
    ) |>
    omopgenerics::recordCohortAttrition(
      reason = "Trim records to {startDaysInCohort[1]} to {startDaysInCohort[2]} days following entry",
      cohortId = cohortId)

useIndexes <- getOption("CohortConstructor.use_indexes")
if (!isFALSE(useIndexes)) {
  addIndex(
    cohort = cohort,
    cols = c("subject_id", "cohort_start_date")
  )
}


  cohort

}
