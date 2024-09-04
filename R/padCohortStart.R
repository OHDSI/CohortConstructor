#' Add days to cohort start
#'
#' @description
#' `padCohortStart()` Adds (or subtracts) a certain number of days to the cohort
#' start date. Note:
#' * If the days added means that cohort start would be after cohort end then
#' the cohort entry will be dropped.
#' * If subtracting day means that cohort start would be before observation
#' period start then the cohort entry will be dropped.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @param days Number of day to add to the cohort start date.
#'
#' @return Cohort table
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' # add 10 days to each cohort entry
#' cdm$cohort1 |>
#'   padCohortStart(days = 10)
#' }
padCohortStart <- function(cohort,
                           days,
                           cohortId = NULL,
                           name = tableName(cohort)) {

  cdm <- omopgenerics::cdmReference(cohort)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  days <- omopgenerics::assertNumeric(days, length = 1)
  days <- as.integer(days)
  name <- omopgenerics::validateNameArgument(name)

  if(length(cohortId) < length(ids)) {
  # if only a subset of ids are provided then only update these
  cohort <- cohort %>%
      dplyr::mutate(
        cohort_start_date =
          dplyr::if_else(
            .data$cohort_definition_id %in% .env$cohortId,
            as.Date(
              !!CDMConnector::dateadd(
                "cohort_start_date",
                number = days,
                interval = "day"
              )
            ),
            .data$cohort_start_date
          )
      )
  } else {
    # if all ids are provided then simpler query - update all
    cohort <- cohort %>%
      dplyr::mutate(
        cohort_start_date = as.Date(
              !!CDMConnector::dateadd(
                "cohort_start_date",
                number = days,
                interval = "day")))
  }
  cohort <- cohort %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)

  if (days < 0) {
    # if days is less than zero then updating start could take the date
    # out of observation
    priorObsCol <- omopgenerics::uniqueId()
    cohort <- cohort |>
      PatientProfiles::addPriorObservationQuery(
        priorObservationType = "date",
        priorObservationName = priorObsCol) %>%
      dplyr::filter(.data$cohort_start_date >= .data[[priorObsCol]]) |>
      dplyr::select(!priorObsCol)
  }

    cdm[[name]] <- cohort |>
      dplyr::compute(temporary = FALSE, name = name) |>
      omopgenerics::recordCohortAttrition(
        reason = "Pad cohort start date by {days} day{?s}")

    return(cdm[[name]])

}
