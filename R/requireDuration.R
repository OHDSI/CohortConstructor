
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
#' @param reason `r reasonDoc("duration")`
#' @inheritParams nameDoc
#'
#' @return The cohort table with any cohort entries that last less or more than
#' the required duration dropped
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   requireDuration(daysInCohort = c(2, Inf))
#' }
requireDuration <- function(cohort,
                            daysInCohort,
                            cohortId = NULL,
                            reason = "Keep records with duration {daysInCohort[1]} to {daysInCohort[2]} days",
                            name = tableName(cohort)) {

  name <- validateNameArgumentInternal(missing(name), name, tableName(cohort))
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  daysInCohort <- validateDaysInCohort(daysInCohort)

  # prepare reason
  reason <- validateReason(
    reason = reason,
    len = 1,
    daysInCohort = daysInCohort
  )

  # apply requirements
  requireInternal(
    cohort = cohort,
    cohortId = cohortId,
    atFirst = FALSE,
    params = list(
      add_type = "duration",
      colname = omopgenerics::uniqueId(exclude = colnames(cohort)),
      remove_column = TRUE,
      filter_type == "range",
      range_min = daysInCohort[1] - 1L,
      range_max = daysInCohort[2] - 1L
    ),
    reason = reason,
    name = name
  )
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
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 |>
#'   requireDuration(daysInCohort = c(2, Inf))
#' }
trimDuration <- function(cohort,
                         daysInCohort,
                         cohortId = NULL,
                         name = tableName(cohort)) {

  name <- validateNameArgumentInternal(missing(name), name, tableName(cohort))
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  daysInCohort <- validateDaysInCohort(daysInCohort)

  newCol <- omopgenerics::uniqueTableName()

  IdFilter <- needsIdFilter(cohort, cohortId)

  # we consider same day entry and exit to count as one day
  if (daysInCohort[1] > 1) {
    daysStart <- daysInCohort[1] - 1L
    qs <- "as.Date(clock::add_days(x = .data$cohort_start_date, n = .env$daysStart))"
    if (isTRUE(IdFilter)) {
      qs <- paste0("dplyr::if_else(.data$cohort_definition_id %in% .env$cohortId, ", qs, ", .data$cohort_start_date)")
    }
    qs <- rlang::set_names(x = qs, nm = "cohort_start_date")
  } else {
    qs <- character()
  }

  if (!is.infinite(daysInCohort[2])) {
    daysEnd <- daysInCohort[2] - 1L
    qe <- "pmin(.data$cohort_end_date, as.Date(clock::add_days(x = .data$cohort_start_date, n = .env$daysEnd)), na.rm = TRUE)"
    if (isTRUE(IdFilter)) {
      qe <- paste0("dplyr::if_else(.data$cohort_definition_id %in% .env$cohortId, ", qe, ", .data$cohort_end_date)")
    }
    qe <- rlang::set_names(x = qe, nm = "cohort_end_date")
  } else {
    qe <- character()
  }

  q <- c(qe, qs) |>
    rlang::parse_exprs()

  cohort <- cohort |>
    dplyr::mutate(!!!q) |>
    dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
    dplyr::compute(name = name, logPrefix = "CohortConstructor_trimDuration_") |>
    omopgenerics::recordCohortAttrition(
      reason = "Trim records to {daysInCohort[1]} to {daysInCohort[2]} days following entry",
      cohortId = cohortId
    )

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(cohort = cohort, cols = c("subject_id", "cohort_start_date"))
  }

  cohort
}
