
#' Set cohort start or cohort end
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @inheritParams collapseDoc
#' @inheritParams daysDoc
#' @inheritParams padObservationDoc
#' @param cohortDate 'cohort_start_date' or 'cohort_end_date'.
#' @param indexDate Variable in cohort that contains the index date to add.
#'
#' @return Cohort table
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   padCohortDate(
#'     cohortDate = "cohort_end_date",
#'     indexDate = "cohort_start_date",
#'     days = 10)
#' }
padCohortDate <- function(cohort,
                          days,
                          cohortDate = "cohort_start_date",
                          indexDate = "cohort_start_date",
                          collapse = TRUE,
                          padObservation = TRUE,
                          cohortId = NULL,
                          name = tableName(cohort)) {
  cohort |>
    .padCohortDate(
      cohortDate = cohortDate,
      indexDate = indexDate,
      days = days,
      collapse = collapse,
      padObservation = padObservation,
      cohortId = cohortId,
      name = name
    )
}

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
#' @inheritParams collapseDoc
#' @inheritParams daysDoc
#' @inheritParams padObservationDoc
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
                         collapse = TRUE,
                         padObservation = TRUE,
                         cohortId = NULL,
                         name = tableName(cohort)) {
  cohort |>
    .padCohortDate(
      cohortDate = "cohort_end_date",
      indexDate = "cohort_end_date",
      days = days,
      collapse = collapse,
      padObservation = padObservation,
      cohortId = cohortId,
      name = name
    )
}

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
#' @inheritParams collapseDoc
#' @inheritParams daysDoc
#' @inheritParams padObservationDoc
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
                           collapse = TRUE,
                           padObservation = TRUE,
                           cohortId = NULL,
                           name = tableName(cohort)) {
  cohort |>
    .padCohortDate(
      cohortDate = "cohort_start_date",
      indexDate = "cohort_start_date",
      days = days,
      collapse = collapse,
      padObservation = padObservation,
      cohortId = cohortId,
      name = name
    )
}

.padCohortDate <- function(cohort,
                           cohortDate,
                           indexDate,
                           days,
                           collapse,
                           padObservation,
                           cohortId,
                           name,
                           call = parent.frame()) {
  # validate input
  cohort <- omopgenerics::validateCohortArgument(cohort = cohort, call = call)
  cohortDate |>
    omopgenerics::assertChoice(
      c("cohort_start_date", "cohort_end_date"), length = 1, call = call
    )
  omopgenerics::assertCharacter(indexDate, length = 1, call = call)
  validateColumn(indexDate, cohort, call = call)
  omopgenerics::assertLogical(collapse, length = 1)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  name <- omopgenerics::validateNameArgument(name, validation = "warning")

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cohort <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                       logPrefix = "CohortConstructor_.padCohortDate_empty_")
    return(cohort)
  }

  msg <- "`days` be an integerish or point to an integerish column of cohort"
  reason <- paste0("pad `", cohortDate, "` ")
  if (is.numeric(days)) {
    omopgenerics::assertNumeric(days, integerish = TRUE, length = 1, msg = msg, call = call)
    reason <- paste0(reason, round(days), " ", ifelse(days == 1, "day", "days"))
    q <- "as.Date(local(CDMConnector::dateadd(indexDate, {round(days)}L)))"

  } else if (is.character(days)) {
    omopgenerics::assertCharacter(days, length = 1, call = call, msg = msg)
    validateColumn(days, cohort, call = call)
    reason <- paste0(reason, "'", days, "' days")
    cohort <- cohort |>
      dplyr::mutate(!!days := as.integer(.data[[days]]))
    q <- "as.Date(local(CDMConnector::dateadd(indexDate, '{days}')))"
  } else {
    cli::cli_abort(message = msg, call = call)
  }

  intermediate <- omopgenerics::uniqueTableName()
  subCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  # pad days
  q <- q |>
    glue::glue() |>
    as.character() |>
    rlang::parse_exprs() |>
    rlang::set_names(cohortDate)
  subCohort <- subCohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) %>%
    dplyr::mutate(!!!q) |>
    # drop start > end
    dplyr::filter(
      .data$cohort_start_date <= .data$cohort_end_date &
        !is.na(.data[[cohortDate]])
    ) |>
    dplyr::compute(name = intermediate, temporary = FALSE,
                   logPrefix = "CohortConstructor_.padCohortDate_intermediate_")

  # solve observation
  subCohort <- subCohort |>
    solveObservation(padObservation, intermediate, cohortDate)

  # solve overlap
  subCohort <- subCohort |>
    solveOverlap(collapse, intermediate)

  # recreate the cohort
  cohort <- cohort |>
    dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::union_all(subCohort) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_.padCohortDate_recreate_") |>
    omopgenerics::newCohortTable(.softValidation = FALSE) |>
    omopgenerics::recordCohortAttrition(cohortId = cohortId, reason = reason)

  # drop temp table
  cdm <- omopgenerics::cdmReference(cohort)
  omopgenerics::dropTable(cdm = cdm, name = intermediate)

  return(cohort)
}

validateColumn <- function(col, x, call) {
  if (!col %in% colnames(x)) {
    cli::cli_abort(c("{.var {col}} column does not exist."), call = call)
  }
  invisible()
}
solveOverlap <- function(x, collapse, intermediate) {
  x <- x |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    )
  if (collapse) {
    x <- x |>
      joinOverlap(name = intermediate)
  } else {
    uniqueName <- omopgenerics::uniqueTableName()
    xId <- x |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::arrange(.data$cohort_start_date) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::compute(name = uniqueName, temporary = FALSE,
                     logPrefix = "CohortConstructor_solveOverlap_xId_")
    x <- xId |>
      dplyr::left_join(
        xId |>
          dplyr::select(
            "cohort_definition_id", "subject_id",
            "prior_end_date" = "cohort_end_date"
          ) |>
          dplyr::mutate(id = .data$id + 1L),
        by = c("cohort_definition_id", "subject_id", "id")
      ) |>
      dplyr::filter(
        is.na(.data$prior_end_date) |
          .data$prior_end_date < .data$cohort_start_date
      ) |>
      dplyr::select(!"prior_end_date") |>
      dplyr::compute(name = intermediate, temporary = FALSE,
                     logPrefix = "CohortConstructor_solveOverlap_final_")
    cdm <- omopgenerics::cdmReference(x)
    omopgenerics::dropTable(cdm = cdm, name = uniqueName)
  }
  return(x)
}
solveObservation <- function(x, padObservation, intermediate, cohortDate) {
  idcol <- omopgenerics::uniqueId(exclude = colnames(x))
  if (cohortDate == "cohort_start_date") {
    x <- x |>
      PatientProfiles::addPriorObservationQuery(
        indexDate = "cohort_end_date",
        priorObservationName = idcol,
        priorObservationType = "date"
      )
    if (padObservation) {
      x <- x |>
        dplyr::mutate("cohort_start_date" = dplyr::if_else(
          .data$cohort_start_date < .data[[idcol]],
          .data[[idcol]],
          .data$cohort_start_date
        ))
    } else {
      x <- x |>
        dplyr::filter(.data$cohort_start_date >= .data[[idcol]])
    }
  } else {
    x <- x |>
      PatientProfiles::addFutureObservationQuery(
        indexDate = "cohort_start_date",
        futureObservationName = idcol,
        futureObservationType = "date"
      )
    if (padObservation) {
      x <- x |>
        dplyr::mutate("cohort_end_date" = dplyr::if_else(
          .data$cohort_end_date > .data[[idcol]],
          .data[[idcol]],
          .data$cohort_end_date
        ))
    } else {
      x <- x |>
        dplyr::filter(.data$cohort_end_date <= .data[[idcol]])
    }
  }
  x |>
    dplyr::select(!dplyr::all_of(idcol)) |>
    dplyr::compute(name = intermediate, temporary = FALSE,
                   logPrefix = "CohortConstructor_solveObservation_")
}
