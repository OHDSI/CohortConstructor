#' Require cohort subjects are present in another clinical table
#'
#' @description
#' `requireTableIntersect()` filters a cohort table based on a requirement
#' that an individual is seen (or not seen) to have a record (or no records) in
#' a clinical table in some time window around an index date.
#'
#' @inheritParams requireIntersectDoc
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams windowDoc
#' @inheritParams nameDoc
#' @inheritParams atFirstDoc
#'
#' @return Cohort table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort1 |>
#'   requireTableIntersect(tableName = "drug_exposure",
#'                             indexDate = "cohort_start_date",
#'                             window = c(-Inf, 0))
#' }
requireTableIntersect <- function(cohort,
                                  tableName,
                                  window,
                                  intersections = c(1, Inf),
                                  cohortId = NULL,
                                  indexDate = "cohort_start_date",
                                  targetStartDate = startDateColumn(tableName),
                                  targetEndDate = endDateColumn(tableName),
                                  inObservation = TRUE,
                                  censorDate = NULL,
                                  atFirst = FALSE,
                                  name = tableName(cohort)) {
  # checks
  name <- validateNameArgumentInternal(missing(name), name, tableName(cohort))
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  window <- omopgenerics::validateWindowArgument(window)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  intersections <- validateIntersections(intersections)
  omopgenerics::assertCharacter(tableName)
  omopgenerics::assertLogical(atFirst, length = 1)

  if (length(cohortId) == 0) {
    cli::cli_warn("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireTableIntersect_entry_")
    return(cdm[[name]])
  }

  lower_limit <- as.integer(intersections[[1]])
  upper_limit <- intersections[[2]]
  upper_limit[is.infinite(upper_limit)] <- 999999L
  upper_limit <- as.integer(upper_limit)

  window_start <- window[[1]][1]
  window_end <- window[[1]][2]

  if (length(tableName) > 1) {
    cli::cli_abort("Currently just one table supported.")
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  intersectCol <- uniqueColumnName(newCohort)
  newCohort <- newCohort |>
    PatientProfiles::addTableIntersectCount(
      tableName = tableName,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      inObservation = inObservation,
      nameStyle = intersectCol,
      name = tmpNewCohort
    )

  missCount <- newCohort |>
    dplyr::filter(!!!glue::glue("is.na(.data${intersectCol})") |> rlang::parse_exprs()) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if(missCount > 0){
    if(window_end < 0) {
      cli::cli_inform("A total of {missCount} records do not have at least {abs(window_end)} days of prior observation and so will be dropped.")
      cli::cli_inform("Adding requirement of {abs(window_end)} days of prior observation")
      newCohort <- newCohort |>
        requirePriorObservation(minPriorObservation = abs(window_end),
                                cohortId = cohortId,
                                indexDate = indexDate,
                                atFirst = atFirst,
                                name = tableName(newCohort))
    } else if (window_start > 0){
      cli::cli_inform("A total of {missCount} records do not have at least {window_start} days of future observation and so will be dropped.")
      cli::cli_inform("Adding requirement of {window_start} days of future observation")
      newCohort <- newCohort |>
        requireFutureObservation(minFutureObservation = window_start,
                                 cohortId = cohortId,
                                 indexDate = indexDate,
                                 atFirst = atFirst,
                                 name = tableName(newCohort))
    }
  }

  newCohort <- applyRequirement(
    newCohort, atFirst, tmpNewCohort, intersectCol, lower_limit, upper_limit, cdm
  )

  # attrition reason
  if (all(intersections == 0)) {
    reason <- glue::glue(
      "Not in table {tableName} between {window_start} & ",
      "{window_end} days relative to {indexDate}"
    )
  } else if (intersections[[1]] != intersections[[2]]) {
    reason <- glue::glue(
      "In table {tableName} between {window_start} & ",
      "{window_end} days relative to {indexDate} between ",
      "{intersections[[1]]} and {intersections[[2]]}"
    )
  } else {
    reason <- glue::glue(
      "In table {tableName} between {window_start} & ",
      "{window_end} days relative to {indexDate} ",
      "{intersections[[1]]} times"
    )
  }
  reason <- completeAttritionReason(reason, censorDate, atFirst)

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(cdm[[tmpUnchanged]]) |>
      dplyr::compute(
        name = tmpNewCohort, temporary = FALSE,
        logPrefix = "CohortConstructor_requireTableIntersect_union_"
      )
  }

  newCohort <- newCohort |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_requireTableIntersect_name_"
    ) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

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

applyRequirement <- function(newCohort, atFirst, tmpNewCohort, intersectCol, lower_limit, upper_limit, cdm) {
  if (atFirst) {
    tmpNewCohortFirst <- paste0(tmpNewCohort, "_1")
    newCohortFirst <- newCohort |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::filter(.data$cohort_start_date == base::min(.data$cohort_start_date)) |>
      dplyr::ungroup() |>
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE, logPrefix = "CohortConstructor_applyRequirement_subset_arrange_") |>
      dplyr::filter(.data[[intersectCol]] >= .env$lower_limit & .data[[intersectCol]] <= .env$upper_limit) |>
      dplyr::select(dplyr::all_of(c("cohort_definition_id", "subject_id"))) |>
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE, logPrefix = "CohortConstructor_applyRequirement_subset_first_")
    newCohort <- newCohort |>
      dplyr::inner_join(newCohortFirst, by = c("cohort_definition_id", "subject_id")) |>
      dplyr::select(!dplyr::all_of(intersectCol)) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE, logPrefix = "CohortConstructor_applyRequirement_requirement_first_")
    omopgenerics::dropSourceTable(cdm = cdm, name = tmpNewCohortFirst)
  } else {
    newCohort <- newCohort |>
      dplyr::filter(
        .data[[intersectCol]] >= .env$lower_limit & .data[[intersectCol]] <= .env$upper_limit
      ) |>
      dplyr::select(!dplyr::all_of(intersectCol)) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyRequirement_subset_")
  }
  return(newCohort)
}
