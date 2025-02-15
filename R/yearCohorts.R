#' Generate a new cohort table restricting cohort entries to certain years
#'
#' @description
#' `yearCohorts()` splits a cohort into multiple cohorts, one for each year.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdSubsetDoc
#' @inheritParams nameDoc
#' @param years Numeric vector of years to use to restrict observation to..
#'
#' @return A cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort1 <- cdm$cohort1 |> yearCohorts(years = 2000:2002)
#' settings(cdm$cohort1)
#' }
yearCohorts <- function(cohort,
                        years,
                        cohortId = NULL,
                        name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertNumeric(years, integerish = TRUE)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning empty cohort as `cohortId` is not valid.")
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  if (length(years) == 0) {
    cohort <- cohort |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_yearCohorts_entry_") |>
      omopgenerics::newCohortTable(.softValidation = TRUE)

    useIndexes <- getOption("CohortConstructor.use_indexes")
    if (!isFALSE(useIndexes)) {
      addIndex(
        cohort = cohort,
        cols = c("subject_id", "cohort_start_date")
      )
    }

    return(cohort)
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpName <- omopgenerics::uniqueTableName(tablePrefix)
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)

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

  # years set
  sets <- cohort |>
    settings() |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  sets <- lapply(years, function(y) {
    sets |>
      dplyr::mutate(
        "year" = as.integer(.env$y),
        "target_cohort_name" = .data$cohort_name,
        "cohort_name" = paste0(.data$cohort_name, "_", as.character(.env$y))
      )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate("new_cohort_definition_id" = dplyr::row_number())
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = tmpName,
    table = sets |>
      dplyr::select("cohort_definition_id", "year", "new_cohort_definition_id"),
    temporary = TRUE
  )

  # years cohort
  cohort <- cohort |>
    subsetCohorts(cohortId = cohortId, name = name)

  cohort <- cohort |>
    dplyr::mutate(!!!startDates, !!!endDates) |>
    dplyr::select(!c("cohort_start_date", "cohort_end_date")) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with(c(
        "cohort_start_date", "cohort_end_date"
      )),
      names_to = c(".value", "year"),
      names_pattern = "(cohort_start_date|cohort_end_date)_(\\d+)"
    ) |>
    dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
    dplyr::mutate("year" = as.integer(.data$year)) |>
    dplyr::inner_join(cdm[[tmpName]], by = c("cohort_definition_id", "year")) |>
    dplyr::select(-"cohort_definition_id", -"year") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::compute(name = name, temporary = FALSE)

  # build new metadata
  ## set
  newSet <- sets |>
    dplyr::rename("target_cohort_definition_id" = "cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")
  ## attrition
  counts <- cohort |>
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
    targetId <- newSet$target_cohort_definition_id[newSet$cohort_definition_id == k]
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
            "reason_id" = .data$reason_id + 1L,
            "reason" = .env$reason |> as.character()
          )
      )
  }
  ## codelist
  codelist <- attr(cohort, "cohort_codelist")
  newCodelist <- cdm[[tmpName]] |>
    dplyr::rename("target_cohort_definition_id" = "cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::select(c("cohort_definition_id", "target_cohort_definition_id")) |>
    dplyr::inner_join(
      codelist |>
        dplyr::rename("target_cohort_definition_id" = "cohort_definition_id"),
      by = "target_cohort_definition_id",
      relationship = "many-to-many"
    ) |>
    dplyr::select(!"target_cohort_definition_id")

  # new cohort
  cohort <- cohort |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_yearCohorts_newCohort_") |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = newAttrition |> dplyr::bind_rows(),
      cohortCodelistRef = newCodelist,
      .softValidation = FALSE
    )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cohort)
}
