#' Generate cohort from the union of different cohorts
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param gap Number of days between two subsequent cohort entries of a subject
#' that will be merged in a single cohort entry
#' @param cohortName Name of the returned cohort. If NULL a the name will be
#' created by collapsing the individual cohort names, separated by "_".
#' @param name Name of the new cohort table.
#'
#' @export
#'
#' @return The cdm object with the new generated cohort set
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#' cdm$cohort2 <- cdm$cohort2 |> unionCohort()
#' CDMConnector::settings(cdm$cohort2)
#'
#' }
unionCohort <- function(cohort,
                        cohortId = NULL,
                        gap = 0,
                        cohortName = NULL,
                        name = omopgenerics::tableName(cohort)) {
  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  assertCharacter(cohortName, length = 1, null = TRUE)

  # union cohort
  newCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    joinOverlap(by = "subject_id", gap = gap) |>
    dplyr::mutate(cohort_definition_id = 1) |>
    dplyr::compute(name = name, temporary = FALSE)
  # cohort set
  names <- omopgenerics::settings(cohort)|>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::pull("cohort_name")
  if (length(cohortName) == 0) {
    cohortName <- paste0(names, collapse = "_")
  }
  cohSet <- dplyr::tibble(
    cohort_definition_id = 1, cohort_name = cohortName
  )
  # cohort attrition
  namesReason <- paste0(paste0(names[1:(length(names) - 1)], collapse = ", "), " and ", names[length(names)])
  cohAtt <- omopgenerics::attrition(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::inner_join(omopgenerics::settings(cohort) |>
                        dplyr::select("cohort_definition_id", "cohort_name"),
                      by = "cohort_definition_id") |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id) |>
    dplyr::mutate(
      cohort_definition_id = 1,
      reason = paste0("[", .data$cohort_name, "] ", .data$reason),
      reason_id = dplyr::row_number(),
      cohort_name = cohortName
    )
  cohAtt <- cohAtt |>
    dplyr::bind_rows(
      newCohort |>
        dplyr::summarise(
          "number_records" = dplyr::n(),
          "number_subjects" = dplyr::n_distinct(.data$subject_id)
        ) |>
        dplyr::collect() |>
        dplyr::bind_cols(
          omopgenerics::cohortCount(cohort) |>
            dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
            dplyr::summarise(
              "previous_number_records" = sum(number_records),
              "previous_number_subjects" = sum(number_subjects)
            )
        ) |>
        dplyr::mutate(
          cohort_definition_id = 1,
          excluded_records = .data$previous_number_records - .data$number_records,
          excluded_subjects = .data$previous_number_subjects - .data$number_subjects,
          reason_id = max(cohAtt$reason_id) + 1,
          reason = paste0("Union of ", namesReason)
        )
    )
  # concept list
  codes <- list()
  for (k in cohortId) {codes <- c(codes, suppressWarnings(cohort |> omopgenerics::cohortCodelist(k)))}
  if (length(codes) > 0) {
    cohCodelist <- omopgenerics::newCodelist(codes)
  } else {
    cohCodelist <- NULL
  }
  # new cohort
  newCohort <- newCohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohSet,
      cohortAttritionRef = cohAtt,
      cohortCodelistRef = cohCodelist
    )
  return(newCohort)
}
