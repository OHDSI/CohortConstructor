#' Split cohorts based on time-windows
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdSubsetDoc
#' @inheritParams nameDoc
#' @inheritParams keepOriginalCohortsDoc
#' @param window A list specifying the time windows (in days) used to split the
#' cohort. Each element must be a numeric vector of length 2:
#' `c(start_day, end_day)`, where the values are days since `cohort_start_date`.
#' Use `Inf` as the end value to indicate a window that extends until the
#' subject's `cohort_end_date`. If the list is named, window names will be used
#' to identify the output cohorts
#'
#' @export
#'
#' @return A cohort table
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' # if "cohort1" contained pregnancy episodes, we can generate trimester-specific
#' # cohorts with this function
#' cdm$pregnancy_trimesters <- splitCohortTime(
#'   cohort = cdm$cohort1,
#'   windows = list(
#'     "trimester_1" = c(0, 90),
#'     "trimester_2" = c(91,180),
#'     "trimester_3" = c(181, Inf)
#'   ),
#'   cohortId = NULL,
#'   keepOriginalCohorts = FALSE,
#'   name = "pregnancy_trimesters"
#' )
#' }
splitCohortTime <- function (cohort,
                             window,
                             cohortId = NULL,
                             keepOriginalCohorts = TRUE,
                             name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  window <- omopgenerics::validateWindowArgument(window)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning empty cohort as `cohortId` is not valid.")
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  if (length(window) == 0 ||
      sum(cohortCount(cohort)$number_records) == 0) {
    return(
      subsetCohorts(
        cohort = cohort,
        cohortId = cohortId,
        name = name
      )
    )
  }

  # prefix
  prefix <- omopgenerics::tmpPrefix()
  # indexes
  useIndexes <- getOption("CohortConstructor.use_indexes")

  # New cohort settings
  newSetName <- omopgenerics::uniqueTableName(prefix = prefix)
  newSet <- settings(cohort) |>
    dplyr::select(!dplyr::any_of(c(
      "window_1", "window_2", "window",
      "target_cohort_id", "target_cohort_name", "target_cohort_table_name"
    ))) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::mutate("target_cohort_table_name" = tableName(cohort)) |>
    dplyr::rename(
      "target_cohort_id" = "cohort_definition_id",
      "target_cohort_name" = "cohort_name"
    ) |>
    dplyr::cross_join(
      dplyr::tibble(
        cohort_name = names(windows),
        window_1 = purrr::map_chr(windows, ~ as.character(.x[1])),
        window_2 = purrr::map_chr(windows, ~ as.character(.x[2]))
      )
    ) |>
    dplyr::mutate(
      cohort_definition_id = dplyr::row_number(),
      cohort_name = paste0(.data$target_cohort_name, "_", .data$cohort_name),
      window = paste0("[", .data$window_1, ", ", .data$window_2, "]")
    )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = newSetName, table = newSettings
  )
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[newSetName]],
      cols = c("cohort_name")
    )
  }

  # New cohort attrition
  newAttrition <- attrition(cohort) |>
    dplyr::rename("target_cohort_id" = "cohort_definition_id") |>
    dplyr::inner_join(
      newSettings |>
        dplyr::select(dplyr::all_of(c("cohort_definition_id", "target_cohort_id"))),
      by = "target_cohort_id"
    ) |>
    dplyr::select(!"target_cohort_id")
  # New cohort codelist
  newCodelist <- attr(cohort, "cohort_codelist") |>
    dplyr::rename("target_cohort_id" = "cohort_definition_id") |>
    dplyr::collact() |>
    dplyr::inner_join(
      newSettings |>
        dplyr::select(dplyr::all_of(c("cohort_definition_id", "target_cohort_id"))),
      by = "target_cohort_id"
    ) |>
    dplyr::select(!"target_cohort_id")

  # cohort
  newCohortName <- omopgenerics::uniqueTableName(prefix = prefix)
  newCohort <- cohort |>
    dplyr::select(!dplyr::any_of(colnames(newSet))) |>
    PatientProfiles::addCohortName() |>
    dplyr::compute(name = newCohortName, temporary = FALSE)
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("cohort_name")
    )
  }
  newCohort <- newCohort |>
    dplyr::inner_join(cdm[[newSetName]], by = "cohort_name") |>
    dplyr::compute(name = newCohortName, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet |>
        dplyr::select(dplyr::all_of(c(
          "cohort_definition_id", "cohort_name", "window", "target_cohort_name",
          colnames(settings(cohort))
        ))),
      cohortAttritionRef = newAttrition,
      cohortCodelistRef = newCodelist
    ) |>
    dplyr::mutate(
      date_1 = clock::add_days(.data$cohort_start_date, .data$window_1),
      date_2 = dplyr::if_else(
        is.infinite(window_2),
        .data$cohort_end_date,
        clock::add_days(.data$cohort_start_date, .data$window_2)
      )
    ) |>
    # drop if start after cohort end
    dplyr::filter(.data$date_1 <= .data$cohort_end_date) |>
    # correct end date if > cohort end
    dplyr::mutate(
      date_2 = dplyr::if_else(
        .data$cohort_end_date > .data$date_2,
        .data$cohort_end_date,
        .data$date_2
      )
    ) |>
    dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(name = newCohortName, temporary = FALSE)

  # Add reasons
  attritionReason <- newSet |>
    dplyr::pull(window) |>
    unique()
  for (win in attritionReason) {
    ids <- newSet |> dplyr::filter(.data$window == win) |> dplyr::pull(.data$cohort_definition_id)
    newCohort <- newCohort |>
      omopgenerics::recordCohortAttrition(
        reason = "Records within {win} days from cohort entry",
        cohortId = ids
      )
  }

  if (keepOriginalCohorts) {
    cdm <- omopgenerics::bind(newCohort, cohort, name = name)
  } else {
    cdm[[name]] <- newCohort |>
      dplyr::compute(name = name, temporary = FALSE) |>
      omopgenerics::newCohortTable()
  }

  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))

  return(cdm[[name]])
}
