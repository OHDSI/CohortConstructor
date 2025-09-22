
#' Require cohort entries last for a certain number of days
#'
#' @description
#' `requireDuration()` filters cohort records, keeping only those which last
#' for the specified amount of days
#'
#' @inheritParams cohortDoc
#' @param daysInCohort Range of days that cohort entries must include. For
#' example, if set to c(30, 90) then only cohort entries that are 30 days
#' or more longer and shorter or equal to 90 days will be kept.
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
      reason = "Keep records with duration {daysInCohort[1]} to {daysInCohort[2]} days",
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
#' dates. Cohort entries will be trimmed to these dates
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

  cohort <- cohort|>
    dplyr::filter(.data$cohort_start_date <=
                    .data$cohort_end_date) |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_trimDuration_"
    ) |>
    omopgenerics::recordCohortAttrition(
      reason = "Trim records to {daysInCohort[1]} to {daysInCohort[2]} days following entry",
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
