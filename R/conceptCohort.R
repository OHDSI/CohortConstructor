#' Generate a cohort based on a concept set. The current supported domains are:
#'
#' @param cdm A cdm_reference object.
#' @param conceptSet A conceptSet, can either be a list of concepts, a codelist
#' or a conceptSetExpression (TO DO).
#' @param name Name of the cohort in the cdm object.
#'
#' @export
#'
#' @return A cohort_table object.
#'
conceptCohort <- function(cdm,
                          conceptSet,
                          name) {
  # initial input validation
  cdm <- validateCdm(cdm)
  name <- validateName(name)
  if (length(conceptSet) == 0) {
    cli::cli_inform(c("i" = "Empty codelist provided, returning empty cohort"))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }
  conceptSet <- validateConceptSet(conceptSet)

  # create concept set tibble
  cohortSet <- dplyr::tibble("cohort_name" = names(conceptSet)) |>
    dplyr::mutate("cohort_definition_id" = dplyr::row_number())
  cohortCodelist <- lapply(conceptSet, dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "cohort_name") |>
    dplyr::inner_join(cohortSet, by = "cohort_name") |>
    dplyr::select("cohort_definition_id", "concept_id"  = "value", "codelist_name" = "cohort_name") |>
    dplyr::mutate("type" = "index event") |>
    addDomains(cdm)

  ud <- cohortCodelist |>
    dplyr::group_by(.data$domain_id) |>
    dplyr::tally() |>
    dplyr::collect() |>
    dplyr::filter(!.data$domain_id %in% domainsData$domain_id)
  for (k in seq_len(nrow(ud))) {
    cli::cli_inform(c(
      "x" = "Domain {.strong {ud$domain_id[k]}} ({ud$n[k]} concept{?s}) excluded because it is not supported."
    ))
  }

  cohortCodelist <- cohortCodelist |>
    dplyr::filter(.data$domain_id %in% !!domainsData$domain_id) |>
    dplyr::compute()

  domains <- cohortCodelist |>
    dplyr::select("domain_id") |>
    dplyr::distinct() |>
    dplyr::pull()

  cohorts <- list()
  for (k in seq_along(domains)) {
    domain <- domains[k]
    table <- domainsData$table[domainsData$domain_id == domain]
    concept <- domainsData$concept[domainsData$domain_id == domain]
    start <- domainsData$start[domainsData$domain_id == domain]
    end <- domainsData$end[domainsData$domain_id == domain]
    n <- cohortCodelist |>
      dplyr::filter(.data$domain_id %in% .env$domain) |>
      dplyr::tally() |>
      dplyr::pull()
    if (table %in% names(cdm)) {
      cli::cli_inform(c(
        "i" = "Subsetting table {.strong {table}} using {n} concept{?s} with domain: {.strong {domain}}."
      ))
      cohorts[[k]] <- cdm[[table]] |>
        dplyr::select(
          "subject_id" = "person_id",
          "concept_id" = dplyr::all_of(concept),
          "cohort_start_date" = dplyr::all_of(start),
          "cohort_end_date" = dplyr::all_of(end)
        ) |>
        dplyr::inner_join(
          cohortCodelist |>
            dplyr::filter(.data$domain_id %in% .env$domain) |>
            dplyr::select("concept_id", "cohort_definition_id"),
          by = "concept_id"
        )
    } else {
      cli::cli_inform(c(
        "x" = "Domain {.strong {domain}} ({n} concept{?s}) excluded because table {table} is not present in the cdm."
      ))
    }
  }

  if (length(cohorts) == 0) {
    cli::cli_inform(c("i" = "No table could be subsetted, returning empty cohort."))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    cdm[[name]] <- cdm[[name]] |>
      omopgenerics::newCohortTable(
        cohortSetRef = cohortSet,
        cohortAttritionRef = NULL,
        cohortCodelistRef = NULL
      )
    return(cdm[[name]])
  }

  cli::cli_inform(c("i" = "Subsetting tables."))
  cohort <- cohorts[[1]]
  if (length(cohorts) > 1) {
    for (k in 2:length(cohorts)) {
      cohort <- cohort |> dplyr::union_all(cohorts[[k]])
    }
  }
  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE)

  cli::cli_inform(c("i" = "Collapsing records."))
  # assign to cdm so we keep class, to be removed when https://github.com/darwin-eu-dev/omopgenerics/issues/256
  cdm[[name]] <- cohort |>
    collapseGap(gap = 0)
  cohort <- cdm[[name]] |>
    dplyr::compute(name = name, temporary = FALSE)

  cli::cli_inform(c("i" = "Creating cohort attributes."))
  cdm[[name]] <- cohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohortCodelist |>
        dplyr::select(-"domain_id") |>
        dplyr::collect()
    )

  cli::cli_inform(c("v" = "Cohort {.strong {name}} created."))

  return(cdm[[name]])
}

addDomains <- function(cohortCodelist, cdm) {
  # insert table as temporary
  tmpName <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = tmpName, table = cohortCodelist
  )
  cdm[[tmpName]] <- cdm[[tmpName]] |> dplyr::compute()

  cohortCodelist <- cdm[["concept"]] |>
    dplyr::select("concept_id", "domain_id") |>
    dplyr::right_join(cdm[[tmpName]], by = "concept_id") |>
    dplyr::mutate("domain_id" = tolower(.data$domain_id)) |>
    dplyr::compute()

  omopgenerics::dropTable(cdm = cdm, name = tmpName)

  return(cohortCodelist)
}
collapseGap <- function(cohort, gap) {
  start <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "date" = "cohort_start_date"
    ) |>
    dplyr::mutate("date_id" = -1)
  end <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "date" = "cohort_end_date"
    ) |>
    dplyr::mutate("date_id" = 1)
  start |>
    dplyr::union_all(end) |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$date, .data$date_id) |>
    dplyr::mutate("cum_id" = cumsum(.data$date_id)) |>
    dplyr::filter(
      .data$cum_id == 0 | (.data$cum_id == -1 & .data$date_id == -1)
    ) |>
    dplyr::mutate(
      "name" = dplyr::if_else(
        .data$date_id == -1, "cohort_start_date", "cohort_end_date"
      ),
      "era_id" = dplyr::if_else(.data$date_id == -1, 1, 0)
    ) |>
    dplyr::mutate("era_id" = cumsum(as.numeric(.data$era_id))) |>
    dplyr::ungroup() |>
    dplyr::arrange() |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "era_id", "name", "date"
    ) |>
    tidyr::pivot_wider(names_from = "name", values_from = "date") |>
    dplyr::select(-"era_id")
}
