#' Require cohort subjects are present (or absence) in another cohort
#'
#' @description
#' `requireCohortIntersect()` filters a cohort table based on a requirement
#' that an individual is seen (or not seen) in another cohort in some time
#' window around an index date.
#'
#' @inheritParams requireIntersectDoc
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams windowDoc
#' @inheritParams nameDoc
#' @inheritParams atFirstDoc
#' @inheritParams softValidationDoc
#'
#' @return Cohort table with only those entries satisfying the criteria
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   requireCohortIntersect(targetCohortTable = "cohort2",
#'                          targetCohortId = 1,
#'                          indexDate = "cohort_start_date",
#'                          window = c(-Inf, 0))
#' }
requireCohortIntersect <- function(cohort,
                                   targetCohortTable,
                                   window,
                                   intersections = c(1, Inf),
                                   cohortId = NULL,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   targetStartDate = "cohort_start_date",
                                   targetEndDate = "cohort_end_date",
                                   censorDate = NULL,
                                   atFirst = FALSE,
                                   name = tableName(cohort),
                                   .softValidation = TRUE) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  omopgenerics::validateCohortArgument(cdm[[targetCohortTable]])
  window <- omopgenerics::validateWindowArgument(window)
  cohortId <- omopgenerics::validateCohortIdArgument(
    {{cohortId}}, cohort, validation = "warning"
  )
  targetCohortId <- omopgenerics::validateCohortIdArgument(
    {{targetCohortId}}, cdm[[targetCohortTable]], validation = "error"
  )
  intersections <- validateIntersections(intersections)
  omopgenerics::assertLogical(.softValidation, length = 1)
  omopgenerics::assertLogical(atFirst, length = 1)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireCohortIntersect_entry_1_")
    return(cdm[[name]])
  }

  if (cdm[[targetCohortTable]] |>
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
      dplyr::tally() |>
      dplyr::pull("n") == 0) {
    cli::cli_inform("Returning entry cohort as the target cohort to intersect is empty.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireCohortIntersect_entry_2_")
    return(cdm[[name]])
  }

  # targetCohortId must be singular
  if (length(targetCohortId) > 1) {
    cli::cli_abort(c("requireCohortIntersect can only be use with one target cohort at a time.",
                     "i" = "Cohort IDs {targetCohortId} found in targetCohortTable {targetCohortTable}",
                     "i" = "Use targetCohortId argument to specify just one cohort for intersection"))
  }

  lower_limit <- as.integer(intersections[[1]])
  upper_limit <- intersections[[2]]
  upper_limit[is.infinite(upper_limit)] <- 999999L
  upper_limit <- as.integer(upper_limit)

  window_start <- window[[1]][1]
  window_end <- window[[1]][2]

  if (length(targetCohortTable) > 1) {
    cli::cli_abort("Only one target cohort table is currently supported")
  }

  if (is.null(targetCohortId)) {
    targetCohortId <- omopgenerics::settings(cdm[[targetCohortTable]]) |>
      dplyr::pull("cohort_definition_id")
  }

  if (length(targetCohortId) > 1) {
    cli::cli_abort("Only one target cohort ID is currently supported")
  }

  target_name <- cdm[[targetCohortTable]] |>
    omopgenerics::settings() |>
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) |>
    dplyr::pull("cohort_name")

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  # requirement
  intersectCol <- uniqueColumnName(newCohort)
  newCohort <- newCohort |>
    PatientProfiles::addCohortIntersectCount(
      targetCohortTable = targetCohortTable,
      targetCohortId = targetCohortId,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = intersectCol,
      name = tmpNewCohort
    )

  newCohort <- applyRequirement(
    newCohort, atFirst, tmpNewCohort, intersectCol, lower_limit, upper_limit, cdm
  )

  # attrition reason
  if (all(intersections == 0)) {
    reason <- glue::glue(
      "Not in cohort {target_name} between {window_start} & ",
      "{window_end} days relative to {indexDate}"
    )
  } else if (intersections[[1]] != intersections[[2]]) {
    reason <- glue::glue(
      "In cohort {target_name} between {window_start} & ",
      "{window_end} days relative to {indexDate} between ",
      "{intersections[[1]]} and {intersections[[2]]} times"
    )
  } else {
    reason <- glue::glue(
      "In cohort {target_name} between {window_start} & ",
      "{window_end} days relative to {indexDate} ",
      "{intersections[[1]]} times"
    )
  }
  reason <- completeAttritionReason(reason, censorDate, atFirst)

  # codelist
  targetCodelist <- attr(cdm[[targetCohortTable]], "cohort_codelist") |>
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
    dplyr::collect()
  newCodelist <- getIntersectionCodelist(
    cohort, cohortId, targetCodelist
  )

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(
        cdm[[tmpUnchanged]] |>
          dplyr::select(dplyr::all_of(colnames(newCohort)))
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireCohortIntersect_union_")
  }

  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_requireCohortIntersect_name_") |>
    omopgenerics::newCohortTable(
      .softValidation = .softValidation, cohortCodelistRef = newCodelist
    ) |>
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
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyRequirement_subset_arrange_") |>
      dplyr::filter(
        .data$rec_id_1234 == 1 & .data[[intersectCol]] >= .env$lower_limit & .data[[intersectCol]] <= .env$upper_limit
        ) |>
      dplyr::select(dplyr::all_of(c("cohort_definition_id", "subject_id"))) |>
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyRequirement_subset_first_")
    newCohort <- newCohort |>
      dplyr::inner_join(newCohortFirst, by = c("cohort_definition_id", "subject_id")) |>
      dplyr::select(!dplyr::all_of(intersectCol)) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyRequirement_requirement_first_")
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

completeAttritionReason <- function(reason, censorDate, atFirst) {
  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }
  if (atFirst) {
    reason <- glue::glue("{reason}. Requirement applied to the first entry")
  }
  return(reason)
}
