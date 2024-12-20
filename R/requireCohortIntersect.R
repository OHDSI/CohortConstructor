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
#'                              targetCohortId = 1,
#'                              indexDate = "cohort_start_date",
#'                              window = c(-Inf, 0))
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
                                   name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  window <- omopgenerics::validateWindowArgument(window)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  intersections <- validateIntersections(intersections)

  lower_limit <- as.integer(intersections[[1]])
  upper_limit <- intersections[[2]]
  upper_limit[is.infinite(upper_limit)] <- 999999L
  upper_limit <- as.integer(upper_limit)

  cols <- unique(
    c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date",
      indexDate
    )
  )

  window_start <- window[[1]][1]
  window_end <- window[[1]][2]

  if (length(targetCohortTable) > 1) {
    cli::cli_abort("Only one target cohort table is currently supported")
  }

  if (length(targetCohortId) > 1) {
    cli::cli_abort("Only one target cohort is currently supported")
  }

  if (is.null(targetCohortId)) {
    targetCohortId <- omopgenerics::settings(cdm[[targetCohortTable]]) |>
      dplyr::pull("cohort_definition_id")
  }

  target_name <- cdm[[targetCohortTable]] |>
    omopgenerics::settings() |>
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) |>
    dplyr::pull("cohort_name")

  subsetName <- omopgenerics::uniqueTableName()
  subsetCohort <- cohort |>
    dplyr::select(dplyr::all_of(.env$cols)) |>
    PatientProfiles::addCohortIntersectCount(
      targetCohortTable = targetCohortTable,
      targetCohortId = targetCohortId,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect_cohort",
      name = name
    )

  subsetCohort <- subsetCohort |>
    dplyr::mutate(lower_limit = .env$lower_limit,
                  upper_limit = .env$upper_limit) |>
    dplyr::filter((
      .data$intersect_cohort >= .data$lower_limit &
        .data$intersect_cohort <= .data$upper_limit
    ) |
      (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::compute(name = subsetName, temporary = FALSE)

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
  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }

  x <- cohort |>
    dplyr::inner_join(subsetCohort, by = c(cols)) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  omopgenerics::dropTable(cdm = cdm, name = subsetName)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = x,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(x)
}
