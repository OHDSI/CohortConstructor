#' Generate a new cohort_table object restrincting observation to certain years.
#'
#' @param cohort A cohort_table in a cdm_reference.
#' @param cohortId Cohort definition id to use. If NULL all the cohort
#' definition ids in settings will be used.
#' @param years Numeric vector of years to use to restrict observation to.
#' @param name Name of the new cohort table.
#'
#' @return A cohort table.
#'
#' @export
#'
#' @examples
#' library(omock)
#'
#' cdm <- mockCdmReference() |>
#'   mockPerson() |>
#'   mockObservationPeriod() |>
#'   mockCohort()
#' cdm$year_restricted <- cdm$cohort |>
#'   yearCohorts(years = 2010:2019, name = "year_restricted")
#' cdm$year_restricted
#' cdm$year_restricted |> settings()
#' cdm$year_restricted |> attrition()
#'
yearCohorts <- function(cohort,
                        cohortId = NULL,
                        years,
                        name = tableName(cohort)) {
  # initial checks
  cohort <- validateCohortTable(cohort)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_definition_id)
  assertNumeric(years, integerish = T)
  name <- validateName(name)
  originalName <- omopgenerics::tableName(cohort)

  if (length(years) == 0) {
    return(subsetCohorts(cohort = cohort, cohortId = cohortId, name = name))
  }

  tablePrefix <- omopgenerics::tmpPrefix()
  tmpCohort <- omopgenerics::uniqueTableName(tablePrefix)

  # start dates
  date <- glue::glue("as.Date('{years}-01-01')")
  startDates <- "dplyr::if_else(.data$cohort_start_date <= {date}, {date}, .data$cohort_start_date )" |>
    glue::glue() |>
    rlang::parse_exprs() |>
    rlang::set_names(glue::glue("cohort_start_date_{years}"))

  # end dates
  date <- glue::glue("as.Date('{years}-12-31')")
  endDates <- "dplyr::if_else(.data$cohort_end_date >= {date}, {date}, .data$cohort_end_date )" |>
    glue::glue() |>
    rlang::parse_exprs() |>
    rlang::set_names(glue::glue("cohort_end_date_{years}"))

  sets <- cohort |>
    settings() |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  sets <- lapply(years, function(y) {
    sets |>
      dplyr::mutate(
        "year" = as.integer(.env$y),
        "cohort_name" = paste0(.data$cohort_name, "_", as.character(.env$y))
      )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate("new_cohort_definition_id" = dplyr::row_number())
  tmpName <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = tmpName,
    table = sets |>
      dplyr::select("cohort_definition_id", "year", "new_cohort_definition_id"),
    temporary = TRUE
  )

  cohort <- cohort |>
    subsetCohorts(cohortId = cohortId, name = tmpCohort)

  newCohort <- cohort |>
    dplyr::mutate(!!!startDates, !!!endDates) |>
    dplyr::select(!c("cohort_start_date", "cohort_end_date")) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with(c("cohort_start_date", "cohort_end_date")),
      names_to = c(".value", "year"),
      names_pattern = "(cohort_start_date|cohort_end_date)_(\\d+)"
    ) |>
    dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
    dplyr::mutate("year" = as.integer(.data$year)) |>
    dplyr::inner_join(cdm[[tmpName]], by = c("cohort_definition_id", "year")) |>
    dplyr::select(-"cohort_definition_id", - "year") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::compute(name = tmpCohort, temporary = FALSE)

  # build new metadata
  newSet <- sets |>
    dplyr::mutate("target_cohort" = originalName) |>
    dplyr::rename("target_cohort_definition_id" = "cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")
  counts <- newCohort |>
    dplyr::summarise(
      "number_records" = dplyr::n(),
      "number_subjects" = dplyr::n_distinct(.data$subject_id),
      .by = "cohort_definition_id"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))
  originalAttrition <- attrition(cohort)
  newAttrition <- list()
  for (k in newSet$cohort_definition_id) {
    targetId <- newSet$target_cohort_definition_id[
      newSet$cohort_definition_id == k
    ]
    yr <- newSet$year[newSet$cohort_definition_id == k]
    reason <- "Restrict to observations between: {yr}-01-01 and {yr}-12-31" |>
      glue::glue()
    newAttrition[[k]] <- originalAttrition |>
      dplyr::filter(.data$cohort_definition_id == .env$targetId) |>
      dplyr::mutate("cohort_definition_id" = as.integer(k))
    newAttrition[[k]] <- newAttrition[[k]] |>
      dplyr::union_all(
        newAttrition[[k]] |>
          dplyr::filter(.data$reason_id == max(.data$reason_id)) |>
          dplyr::select(-"excluded_subjects", -"excluded_records") |>
          dplyr::rename(
            "excluded_subjects" = "number_subjects",
            "excluded_records" = "number_records"
          ) |>
          dplyr::left_join(counts, by = "cohort_definition_id") |>
          dplyr::mutate(dplyr::across(
            .cols = c("number_records", "number_subjects"),
            .fns = ~ dplyr::if_else(is.na(.x), 0L, .x)
          )) |>
          dplyr::mutate(
            "excluded_subjects" = .data$excluded_subjects - .data$number_subjects,
            "excluded_records" = .data$excluded_records - .data$number_records,
            "reason_id" = .data$reason_id + 1,
            "reason" = .env$reason
          )
      )
  }

  newCohort <- newCohort|>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = newAttrition |> dplyr::bind_rows(),
      .softValidation = TRUE
    )

  cdm <- cohort |> omopgenerics::bind(newCohort, name = name)

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(cdm[[name]])
}
