#' Require cohort subjects are present in another clinical table
#'
#' @description
#' `requireTableIntersectFlag()` filters a cohort table based on a requirement
#' that an individual is seen (or not seen) to have a record (or no records) in
#' a clinical table in some time window around an index date.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param tableName Name of the table to check for intersect.
#' @param window Window to consider events over.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param targetStartDate Date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate Date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of x.
#' @param negate If set as TRUE, criteria will be applied as exclusion
#' rather than inclusion (i.e. require absence in another cohort).
#' @param name Name of the new cohort with the future observation restriction.
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
#'   requireTableIntersectFlag(tableName = "drug_exposure",
#'                             indexDate = "cohort_start_date",
#'                             window = c(-Inf, 0))
#' }
requireTableIntersectFlag <- function(cohort,
                                      tableName,
                                      window,
                                      cohortId = NULL,
                                      indexDate = "cohort_start_date",
                                      targetStartDate = startDateColumn(tableName),
                                      targetEndDate = endDateColumn(tableName),
                                      censorDate = NULL,
                                      negate = FALSE,
                                      name = tableName(cohort)) {
  # checks
  name <- validateName(name)
  assertLogical(negate, length = 1)
  assertCharacter(tableName)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)

  cols <- unique(c("cohort_definition_id", "subject_id",
                   "cohort_start_date", "cohort_end_date",
                   indexDate))

  if (is.list(window)) {
    window_start <- window[[1]][1]
    window_end <- window[[1]][2]
  } else {
    window_start <- window[1]
    window_end <- window[2]
  }

  if (length(tableName) > 1) {
    cli::cli_abort("Currently just one table supported.")
  }

  subsetCohort <- cohort %>%
    dplyr::select(dplyr::all_of(.env$cols)) %>%
    PatientProfiles::addTableIntersectFlag(
      tableName = tableName,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect"
    )

  if (isFALSE(negate)) {
    subsetCohort <- subsetCohort %>%
      dplyr::filter(
        .data$intersect == 1 |
          (!.data$cohort_definition_id %in% .env$cohortId)
      ) %>%
      dplyr::select(!"intersect")
    # attrition reason
    reason <- glue::glue("In table {tableName} between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  } else {
    # ie require absence instead of presence
    subsetCohort <- subsetCohort %>%
      dplyr::filter(
        .data$intersect != 1 |
          (!.data$cohort_definition_id %in% .env$cohortId)
      ) %>%
      dplyr::select(!"intersect")
    # attrition reason
    reason <- glue::glue("Not in table {tableName} between {window_start} & ",
                         "{window_end} days relative to {indexDate}")
  }

  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }

  x <- cohort %>%
    dplyr::inner_join(subsetCohort,
                      by = c(cols)) %>%
    dplyr::compute(name = name, temporary = FALSE) %>%
    omopgenerics::newCohortTable(.softValidation = TRUE) %>%
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(x)
}

#' Require cohort subjects have a certain amount of records in another clinical
#' table
#'
#' @description
#' `requireTableIntersectCount()` filters a cohort table based on a requirement
#' that an individual has a certain amount of records in
#' a clinical table in some time window around an index date.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param tableName Name of the table to check for intersect.
#' @param window Window to consider events over.
#' @param counts Number of intersections to require.
#' @param requirementType Can take the values: "exactly", "at_least" or
#' "at_most". It indicates whether the number of intersections required should
#' match the counts ("excatly"), or if the counts refer to the minimum
#' ("at_least") or maximum ("at_most") number of intersections required.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param cohortId IDs of the cohorts to modify. If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#' @param targetStartDate Date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate Date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of x.
#' @param name Name of the new cohort with the future observation restriction.
#'
#' @return Cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(drugExposure = TRUE)
#' cdm$cohort1 |>
#'   requireTableIntersectCount(tableName = "drug_exposure",
#'                             indexDate = "cohort_start_date",
#'                             window = c(-Inf, 0),
#'                             counts = 2)
#' }
requireTableIntersectCount <- function(cohort,
                                       tableName,
                                       window,
                                       counts,
                                       requirementType = "exactly",
                                       cohortId = NULL,
                                       indexDate = "cohort_start_date",
                                       targetStartDate = startDateColumn(tableName),
                                       targetEndDate = endDateColumn(tableName),
                                       censorDate = NULL,
                                       name = tableName(cohort)) {
  # checks
  name <- validateName(name)
  assertCharacter(tableName)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  validateCohortColumn(indexDate, cohort, class = "Date")
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertChoice(requirementType, c("exactly", "at_least", "at_most"), length = 1)
  assertNumeric(counts, integerish = TRUE)

  cols <- unique(c("cohort_definition_id", "subject_id",
                   "cohort_start_date", "cohort_end_date",
                   indexDate))

  if (is.list(window)) {
    window_start <- window[[1]][1]
    window_end <- window[[1]][2]
  } else {
    window_start <- window[1]
    window_end <- window[2]
  }

  if (length(tableName) > 1) {
    cli::cli_abort("Currently just one table supported.")
  }

  subsetCohort <- cohort %>%
    dplyr::select(dplyr::all_of(.env$cols)) %>%
    PatientProfiles::addTableIntersectCount(
      tableName = tableName,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      nameStyle = "intersect"
    )

  # filter
  filterExp <- getIntersectCountFilter(counts, requirementType)
  subsetCohort <- subsetCohort %>%
    dplyr::filter(!!filterExp) %>%
    dplyr::select(!"intersect")

  # attrition reason
  reason <- "{stringr::str_to_sentence(gsub('_', ' ', requirementType))} {counts} time{?s} in table {tableName} between {window_start} & {window_end} days relative to {indexDate}"
  if (!is.null(censorDate)) {
    reason <- paste0(reason, ", censoring at {censorDate}")
  }

  x <- cohort %>%
    dplyr::inner_join(subsetCohort, by = c(cols)) %>%
    dplyr::compute(name = name, temporary = FALSE) %>%
    omopgenerics::newCohortTable(.softValidation = TRUE) %>%
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(x)
}
