#' Add days to cohort end
#'
#' @description
#' `padCohortEnd()` Adds (or subtracts) a certain number of days to the cohort
#' end date. Note:
#' * If the days added means that cohort end would be after observation
#' period end date, then observation period end date will be used for cohort
#' exit.
#' * If the days added means that cohort exit would be after the next cohort
#' start then these overlapping cohort entries will be collapsed.
#' * If days subtracted means that cohort end would be before cohort start then
#' the cohort entry will be dropped.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param days Number of day to add to the cohort end date.
#'
#' @return Cohort table
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' # add 10 days to each cohort exit
#' cdm$cohort1 |>
#'   padCohortEnd(days = 10)
#' }
padCohortEnd <- function(cohort,
                         days,
                         cohortId = NULL,
                         name = tableName(cohort)) {
  # validate input
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  days <- omopgenerics::assertNumeric(days, length = 1)
  days <- as.integer(days)
  cohortId <- validateCohortId(cohortId, settings(cohort))

  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  # temp variable names
  futureObsCol <- omopgenerics::uniqueId()
  newEndCol <- omopgenerics::uniqueId()
  diffCol <- omopgenerics::uniqueId()

  if (days > 0) {
    # if days is more than zero then updating end could take the date
    # out of observation
    cohort <- cohort |>
      PatientProfiles::addFutureObservationQuery(
        futureObservationType = "date",
        futureObservationName = futureObsCol
      )
  }

  if(length(cohortId) < length(ids)) {
    # if only a subset of ids are provided then only update these
    cohort <- cohort |>
      dplyr::mutate(
        !!newEndCol :=
          dplyr::if_else(
            .data$cohort_definition_id %in% .env$cohortId,
            as.Date(clock::add_days(x = .data$cohort_end_date, n = days)),
            .data$cohort_end_date
          )
      )
  } else {
    # if all ids are provided then simpler query - update all
    cohort <- cohort |>
      dplyr::mutate(
        !!newEndCol := as.Date(clock::add_days(x = .data$cohort_end_date, n = days))
      )
  }
  if (days > 0) {
    cohort <- cohort |>
      dplyr::mutate(
        !!diffCol := clock::date_count_between(.data[[newEndCol]], .data[[futureObsCol]], "day")
        ) |>
      dplyr::mutate(cohort_end_date = dplyr::if_else(!!rlang::ensym(diffCol) >= 0,
                                                     !!rlang::ensym(newEndCol),
                                                     !!rlang::ensym(futureObsCol)))
  } else {
    cohort <- cohort |>
      dplyr::mutate(cohort_end_date = !!rlang::ensym(newEndCol))
  }

  # drop anyone with end before start
  cohort <- cohort |>
    dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
    dplyr::select(!dplyr::any_of(c(futureObsCol, diffCol, newEndCol)))

  # collapse any overlapping cohort entries
  cohort <- cohort |>
    joinOverlap(name = name,
                by = c("cohort_definition_id", "subject_id"),
                gap = 0)

  cdm[[name]] <- cohort |>
    dplyr::compute(temporary = FALSE, name = name) |>
    omopgenerics::newCohortTable() |>
    omopgenerics::recordCohortAttrition(
      reason = "Pad cohort start date by {days} day{?s}")

  return(cdm[[name]])

}
