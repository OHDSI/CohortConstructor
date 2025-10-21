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
#' @param cohortCombinationCriteria Can be 'all', 'any, or a numeric vector
#' (length 1 or 2) that specifies how many of the target cohorts must meet the
#' intersection requirement.
#'
#' Examples:
#' - 'all': must meet criteria for each of the target cohorts.
#' - 'any': must meet criteria for only one of the target cohorts.
#' - Single value: e.g., `4`, exactly 4 cohorts must meet the criteria. If
#' there were 4 target cohorts, this would be the same as 'all'.
#' - Range: e.g., `c(2, Inf)`, must meet criteria at last 2 of the
#' target cohorts. Note, `c(1, Inf)` is equivalent to 'any'.
#'
#' @return Cohort table with only those entries satisfying the criteria
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#' cdm$cohort1 |>
#'   requireCohortIntersect(targetCohortTable = "cohort2",
#'                          targetCohortId = 1,
#'                          indexDate = "cohort_start_date",
#'                          window = c(-Inf, 0))
#' }
#' }
requireCohortIntersect <- function(cohort,
                                   targetCohortTable,
                                   window,
                                   intersections = c(1, Inf),
                                   cohortId = NULL,
                                   targetCohortId = NULL,
                                   cohortCombinationCriteria = "all",
                                   indexDate = "cohort_start_date",
                                   targetStartDate = "cohort_start_date",
                                   targetEndDate = "cohort_end_date",
                                   censorDate = NULL,
                                   atFirst = FALSE,
                                   name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "date")
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
  cohortCombinationCriteria <- validateIntersections(cohortCombinationCriteria,
                                                     name = "cohortCombinationCriteria",
                                                     targetCohort = cdm[[targetCohortTable]],
                                                     targetCohortId = targetCohortId)
  omopgenerics::assertLogical(atFirst, length = 1)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireCohortIntersect_entry_1_")
    return(cdm[[name]])
  }

  # Fix ranges
  lower_limit <- as.integer(intersections[[1]])
  upper_limit <- intersections[[2]]
  upper_limit[is.infinite(upper_limit)] <- 999999L

  combinations_lower_limit <- as.integer(cohortCombinationCriteria[[1]])
  combinations_upper_limit <- cohortCombinationCriteria[[2]]
  combinations_upper_limit[is.infinite(combinations_upper_limit)] <- 999999L

  window_start <- window[[1]][1]
  window_end <- window[[1]][2]

  # Check if the target cohort is empty for the specified targetCohortId
  targetCohortCount <- cdm[[targetCohortTable]] |>
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
    dplyr::tally() |>
    dplyr::pull("n")
  if (targetCohortCount == 0) {
    if (lower_limit == 0) {
      cli::cli_inform("Target cohort is empty. No subjects will be excluded from the result.")
    } else {
      cli::cli_inform("Target cohort is empty. All subjects will be excluded from the result.")
    }
  }

  if (length(targetCohortTable) > 1) {
    cli::cli_abort("Only one target cohort table is supported")
  }

  if (is.null(targetCohortId)) {
    targetCohortId <- omopgenerics::settings(cdm[[targetCohortTable]]) |>
      dplyr::pull("cohort_definition_id")
  }

  target_name <- cdm[[targetCohortTable]] |>
    omopgenerics::settings() |>
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
    dplyr::pull("cohort_name")

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  # requirement
  newCohort <- newCohort |>
    PatientProfiles::addCohortIntersectCount(
      targetCohortTable = targetCohortTable,
      targetCohortId = targetCohortId,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect_{cohort_name}",
      name = tmpNewCohort
    )

  intersectCols <- settings(cdm[[targetCohortTable]]) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
    dplyr::pull("cohort_name")
  intersectCols <- paste0("intersect_", intersectCols)

  newCohort <- applyCohortRequirement(
    cdm, newCohort, tmpNewCohort, atFirst, lower_limit, upper_limit, intersectCols, combinations_lower_limit, combinations_upper_limit
  )

  # attrition reason
  reason <- createAttritionReason(
    intersections, cohortCombinationCriteria, target_name, window_start, window_end,
    indexDate, censorDate, atFirst
  )

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
      .softValidation = TRUE, cohortCodelistRef = newCodelist
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

applyCohortRequirement <- function(cdm, newCohort, tmpNewCohort, atFirst, lower_limit, upper_limit, intersectCols, combinations_lower_limit, combinations_upper_limit) {
  mutateExpr <- getMutateFilterExpression(lower_limit, upper_limit, intersectCols)
  if (atFirst) {
    tmpNewCohortFirst <- paste0(tmpNewCohort, "_1")
    newCohortFirst <- newCohort |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::filter(.data$cohort_start_date == base::min(.data$cohort_start_date)) |>
      dplyr::ungroup() |>
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyCohortRequirement_subset_arrange_") |>
      dplyr::mutate(!!!mutateExpr) |>
      dplyr::filter(.data$cc_requirement_sum >= .env$combinations_lower_limit & .data$cc_requirement_sum <= .env$combinations_upper_limit) |>
      dplyr::select(dplyr::all_of(c("cohort_definition_id", "subject_id"))) |>
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyCohortRequirement_subset_first_")
    newCohort <- newCohort |>
      dplyr::inner_join(newCohortFirst, by = c("cohort_definition_id", "subject_id")) |>
      dplyr::select(!dplyr::all_of(c(intersectCols))) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyCohortRequirement_requirement_first_")
    omopgenerics::dropSourceTable(cdm = cdm, name = tmpNewCohortFirst)
  } else {
    newCohort <- newCohort |>
      dplyr::mutate(!!!mutateExpr) |>
      dplyr::filter(.data$cc_requirement_sum >= .env$combinations_lower_limit & .data$cc_requirement_sum <= .env$combinations_upper_limit) |>
      dplyr::select(!dplyr::all_of(c(intersectCols, "cc_requirement_sum"))) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyCohortRequirement_subset_")
  }
  return(newCohort)
}

getMutateFilterExpression <- function(lower_limit, upper_limit, intersectCols) {
  mutateExpr <- NULL
  for (col in intersectCols) {
    mutateExpr <- c(mutateExpr, glue::glue("dplyr::if_else(.data${col} >= {lower_limit} & .data${col} <= {upper_limit}, 1L, 0L)"))
  }
  mutateExpr <- c(mutateExpr, glue::glue("{paste0('.data$', intersectCols, collapse = ' + ')}"))
  mutateExpr |> rlang::parse_exprs() |> rlang::set_names(c(intersectCols, "cc_requirement_sum"))
}

formatRange <- function(x) {
  x <- unique(x)
  if (length(x) == 1) {
    if (is.infinite(x)) return(paste0("any number of intersections"))
    if (x == 1) return(paste0("1 intersection"))
    return(paste0(x, " intersections"))
  }
  if (is.infinite(x[2])) return(paste0(x[1], " or more intersections"))
  if (x[1] == x[2]) return(paste0(x[1], " intersections"))
  paste0(x[1], " to ", x[2], " intersections")
}

formatCohortCombo <- function(x, n) {
  x <- unique(x)
  if (length(x) == 1) {
    if (is.infinite(x)) return("any of the cohorts")
    if (x == n) return(paste0("all ", n, " cohorts"))
    return(paste0(x, " of the cohorts"))
  }
  if (is.infinite(x[2])) return(paste0(x[1], " or more of the cohorts"))
  paste0(x[1], " to ", x[2], " of the cohorts")
}

createAttritionReason <- function(intersections,
                                  cohortCombinationCriteria,
                                  targetName,
                                  windowStart,
                                  windowEnd,
                                  indexDate,
                                  censorDate,
                                  atFirst) {
  if (length(targetName) > 1) {
    reason <- paste0(
      "Require ", formatRange(intersections),
      " for ", formatCohortCombo(cohortCombinationCriteria, length(targetName)),
      ": ", glue::glue_collapse(targetName, sep = ", ", last = " and "),
      ". Intersection window: ", windowStart, " to ",
      windowEnd, " days relative to ", indexDate
    )
  } else {
    reason <- paste0(
      "Require ", formatRange(intersections),
      " with cohort ", targetName,
      ". Intersection window: ", windowStart, " to ",
      windowEnd, " days relative to ", indexDate
    )
  }


  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }
  if (atFirst) {
    reason <- glue::glue("{reason}. Requirement applied to the first entry")
  }
  return(reason)
}
