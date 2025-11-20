
#' Set cohort start or cohort end
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams nameDoc
#' @inheritParams collapseDoc
#' @inheritParams daysDoc
#' @inheritParams requireFullContributionDoc
#' @param cohortDate 'cohort_start_date' or 'cohort_end_date'.
#' @param indexDate Variable in cohort that contains the index date to add.
#' @inheritParams softValidationDoc
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
                          requireFullContribution = FALSE,
                          cohortId = NULL,
                          name = tableName(cohort),
                          .softValidation = FALSE) {
  cohort |>
    .padCohortDate(
      cohortDate = cohortDate,
      indexDate = indexDate,
      days = days,
      collapse = collapse,
      requireFullContribution = requireFullContribution,
      cohortId = cohortId,
      name = name,
      .softValidation = .softValidation
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
#' @inheritParams requireFullContributionDoc
#' @inheritParams softValidationDoc
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
                         requireFullContribution = FALSE,
                         cohortId = NULL,
                         name = tableName(cohort),
                         .softValidation = FALSE) {
  cohort |>
    .padCohortDate(
      cohortDate = "cohort_end_date",
      indexDate = "cohort_end_date",
      days = days,
      collapse = collapse,
      requireFullContribution = requireFullContribution,
      cohortId = cohortId,
      name = name,
      .softValidation = .softValidation
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
#' @inheritParams requireFullContributionDoc
#' @inheritParams softValidationDoc
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
                           requireFullContribution = FALSE,
                           cohortId = NULL,
                           name = tableName(cohort),
                           .softValidation = FALSE) {
  cohort |>
    .padCohortDate(
      cohortDate = "cohort_start_date",
      indexDate = "cohort_start_date",
      days = days,
      collapse = collapse,
      requireFullContribution = requireFullContribution,
      cohortId = cohortId,
      name = name,
      .softValidation = .softValidation
    )
}

.padCohortDate <- function(cohort,
                           cohortDate,
                           indexDate,
                           days,
                           collapse,
                           requireFullContribution,
                           cohortId,
                           name,
                           call = parent.frame(),
                           .softValidation) {
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
  cdm <- omopgenerics::cdmReference(cohort)
  omopgenerics::assertLogical(.softValidation)

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
    q <- "as.Date(clock::add_days(x = .data[['{indexDate}']], n = {round(days)}L))"

  } else if (is.character(days)) {
    omopgenerics::assertCharacter(days, length = 1, call = call, msg = msg)
    validateColumn(days, cohort, call = call)
    reason <- paste0(reason, "'", days, "' days")
    cohort <- cohort |>
      dplyr::mutate(!!days := as.integer(.data[[days]]))
    q <- "as.Date(clock::add_days(x = .data[['{indexDate}']], n = {days}))"
  } else {
    cli::cli_abort(message = msg, call = call)
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  # pad days
  q <- q |>
    glue::glue() |>
    as.character() |>
    rlang::parse_exprs() |>
    rlang::set_names(cohortDate)
  newCohort <- newCohort |>
    dplyr::mutate(!!!q) |>
    # drop start > end
    dplyr::filter(
      .data$cohort_start_date <= .data$cohort_end_date &
        !is.na(.data[[cohortDate]])
    ) |>
    dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                   logPrefix = "CohortConstructor_.padCohortDate_intermediate_")

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  # solve observation
  newCohort <- newCohort |>
    solveObservation(requireFullContribution, tmpNewCohort, cohortDate)

  # solve overlap
  newCohort <- newCohort |>
    solveOverlap(collapse, tmpNewCohort)

  # recreate the cohort
  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(
        cdm[[tmpUnchanged]] |>
          dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort")))
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_.padCohortDate_union_")
  }

  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_.padCohortDate_recreate_") |>
    omopgenerics::newCohortTable(.softValidation = .softValidation) |>
    omopgenerics::recordCohortAttrition(cohortId = cohortId, reason = reason)

  # drop temp table
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
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
            "prior_end_date" = "cohort_end_date", "id"
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
    omopgenerics::dropSourceTable(cdm = cdm, name = uniqueName)
  }
  return(x)
}
solveObservation <- function(x, requireFullContribution, intermediate, cohortDate) {
  idcol <- omopgenerics::uniqueId(exclude = colnames(x))
  tablePrefix <- omopgenerics::tmpPrefix()
  cdm <- omopgenerics::cdmReference(x)

  if (cohortDate == "cohort_start_date") {
    newX <- x |>
      PatientProfiles::addPriorObservation(
        indexDate = "cohort_end_date",
        priorObservationName = idcol,
        priorObservationType = "date",
        name = omopgenerics::uniqueTableName(prefix = tablePrefix)
      )
    if (isFALSE(requireFullContribution)) {
      newX <- newX |>
        dplyr::mutate("cohort_start_date" = dplyr::if_else(
          .data$cohort_start_date < .data[[idcol]],
          .data[[idcol]],
          .data$cohort_start_date
        ))
    } else {
      newX <- newX |>
        dplyr::filter(.data$cohort_start_date >= .data[[idcol]])
    }
  } else {
    newX <- x |>
      PatientProfiles::addFutureObservation(
        indexDate = "cohort_start_date",
        futureObservationName = idcol,
        futureObservationType = "date",
        name = omopgenerics::uniqueTableName(prefix = tablePrefix)
      )
    if (isFALSE(requireFullContribution)) {
      newX <- newX |>
        dplyr::mutate("cohort_end_date" = dplyr::if_else(
          .data$cohort_end_date > .data[[idcol]],
          .data[[idcol]],
          .data$cohort_end_date
        ))
    } else {
      newX <- newX |>
        dplyr::filter(.data$cohort_end_date <= .data[[idcol]])
    }
  }
  newX <- newX |>
    dplyr::select(!dplyr::all_of(idcol)) |>
    dplyr::compute(name = intermediate, temporary = FALSE,
                   logPrefix = "CohortConstructor_solveObservation_")

  omopgenerics::dropSourceTable(
    cdm = cdm, name = dplyr::starts_with(tablePrefix)
  )

  newX
}
