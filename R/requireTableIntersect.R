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
#'
#' @return Cohort table with only those in the other table kept (or those that
#' are not in the table if negate = TRUE)
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(drugExposure = TRUE)
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
  omopgenerics::assertCharacter(tableName)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireTableIntersect_entry_")
    return(cdm[[name]])
  }

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

  if (length(tableName) > 1) {
    cli::cli_abort("Currently just one table supported.")
  }

  subsetName <- omopgenerics::uniqueTableName()
  subsetCohort <- cohort |>
    dplyr::select(dplyr::all_of(.env$cols)) |>
    PatientProfiles::addTableIntersectCount(
      tableName = tableName,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect_table",
      name = subsetName
    )

  subsetCohort <- subsetCohort |>
    dplyr::mutate(lower_limit = .env$lower_limit,
                  upper_limit = .env$upper_limit) |>
    dplyr::filter((
      .data$intersect_table >= .data$lower_limit &
        .data$intersect_table <= .data$upper_limit
    ) |
      (!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::compute(name = subsetName, temporary = FALSE,
                   logPrefix = "CohortConstructor_requireTableIntersect_subset_")

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
  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }

  x <- cohort |>
    dplyr::inner_join(subsetCohort, by = c(cols)) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_requireTableIntersect_join_") |>
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
