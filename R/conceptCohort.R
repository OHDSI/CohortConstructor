#' Create cohorts based on a concept set
#'
#' @param cdm A cdm reference.
#' @param conceptSet A conceptSet, which can either be a codelist
#' or a conceptSetExpression.
#' @param name Name of the cohort in the cdm object.
#'
#' @export
#'
#' @return A cohort table
#'
#' @examples
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(conditionOccurrence = TRUE)
#'
#' cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort")
#'
#' cohort |> attrition()
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

  cohortCodelistRef <- cohortCodelist |>
    dplyr::select(-"domain_id") |>
    dplyr::collect()

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
      tempCohort <- cdm[[table]] |>
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
        ) |>
        dplyr::filter(!is.na(.data$cohort_start_date)) |>
        dplyr::mutate(cohort_end_date = dplyr::if_else(
          is.na(.data$cohort_end_date),
          .data$cohort_start_date,
          .data$cohort_end_date
        ))
      if (tempCohort |> dplyr::tally() |> dplyr::pull("n") > 0) {
        cohorts[[k]] <- tempCohort
      }
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
        cohortCodelistRef = cohortCodelistRef
      )
    return(cdm[[name]])
  }

  if (length(cohorts) == 1) {
    cohort <- cohorts[[1]]
  } else {
    cli::cli_inform(c("i" = "Combining tables."))
    cohort <- cohorts[[1]] |> dplyr::compute()
    for (k in 2:length(cohorts)) {
      cohort <- cohort |>
        dplyr::union_all(cohorts[[k]]) |>
        dplyr::compute()
    }
  }
  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE)

  cli::cli_inform(c("i" = "Collapsing records."))
  cohort <- cohort |>
    PatientProfiles::addDemographics(
      age = FALSE,
      sex = FALSE,
      priorObservationType = "date",
      futureObservationType = "date"
    ) |>
    dplyr::filter(
      .data$prior_observation <= .data$cohort_start_date,
      .data$future_observation >= .data$cohort_end_date,
      .data$cohort_start_date <= .data$cohort_end_date
    ) |>
    dplyr::select(-"prior_observation", -"future_observation") |>
    joinOverlap(gap = 0) |>
    dplyr::compute(name = name, temporary = FALSE)

  cli::cli_inform(c("i" = "Creating cohort attributes."))

  cohort <- cohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohortCodelistRef,
      .softValidation = TRUE
    )

  cli::cli_inform(c("v" = "Cohort {.strong {name}} created."))

  return(cohort)
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
