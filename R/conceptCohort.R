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

  cohortSet <- conceptSetToCohortSet(conceptSet)
  cohortCodelist <- conceptSetToCohortCodelist(conceptSet)

  tableCohortCodelist <- omopgenerics::uniqueTableName()
  cdm <- uploadCohortCodelistToCdm(cdm = cdm,
                                   cohortCodelist = cohortCodelist,
                                   tableCohortCodelist = tableCohortCodelist
  )

  # report codes from unsupported domains
  reportConceptsFromUnsopportedDomains(cdm = cdm,
                                       tableCohortCodelist = tableCohortCodelist,
                                       supportedDomains = domainsData$domain_id)

  # get cohort entries from omop records
  cdm[[name]] <- unerafiedConceptCohort(cdm = cdm,
                                   conceptSet = conceptSet,
                                   cohortSet = cohortSet,
                                   cohortCodelist = cohortCodelist,
                                   tableCohortCodelist = tableCohortCodelist,
                                   name = name,
                                   extraCols = NULL)

  omopgenerics::dropTable(cdm = cdm,
                          name = tableCohortCodelist)
  cdm[[tableCohortCodelist]] <- NULL

  if(cdm[[name]] |>
     utils::head(1) |>
     dplyr::tally() |>
     dplyr::pull("n") == 0){
  cli::cli_inform(c("i" = "No cohort entries found, returning empty cohort table."))
  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohortCodelist,
      .softValidation = TRUE
    )

  return(cdm[[name]])
  }

  cli::cli_inform(c("i" = "Collapsing records."))
  cdm[[name]] <- fulfillCohortReqs(cdm = cdm, name = name) |>
    dplyr::compute(name = name,
                   temporary = FALSE,
                   overwrite = TRUE)

  cli::cli_inform(c("i" = "Creating cohort attributes."))
  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohortCodelist,
      .softValidation = TRUE
    )

  cli::cli_inform(c("v" = "Cohort {.strong {name}} created."))

  return(cdm[[name]])
}


# get concept cohort with each entry a concept record
# note, entries may overlap, be out of observation, etc
# we can keep extra columns from the omop tables (to use for measurement cohort etc)
unerafiedConceptCohort <- function(cdm,
                       conceptSet,
                       cohortSet,
                       cohortCodelist,
                       tableCohortCodelist,
                       name,
                       extraCols){


  domains <- sort(cdm[[tableCohortCodelist]] |>
                    dplyr::select("domain_id") |>
                    dplyr::distinct() |>
                    dplyr::pull())

  tableRef <- domainsData |>
    dplyr::filter(.data$domain_id %in% .env$domains)

  cohorts <- list()
  for (k in seq_along(tableRef$domain_id)) {
    domain <- tableRef$domain_id[k]
    table <- tableRef$table[k]
    concept <- tableRef$concept[k]
    start <- tableRef$start[k]
    end <- tableRef$end[k]
    n <- cdm[[tableCohortCodelist]] |>
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
          "concept_id" = dplyr::all_of(.env$concept),
          "cohort_start_date" = dplyr::all_of(.env$start),
          "cohort_end_date" = dplyr::all_of(.env$end),
          dplyr::all_of(extraCols)
        ) |>
        dplyr::inner_join(
          cdm[[tableCohortCodelist]] |>
            dplyr::filter(.data$domain_id %in% .env$domain) |>
            dplyr::select("concept_id", "cohort_definition_id"),
          by = "concept_id"
        ) |>
        dplyr::filter(!is.na(.data$cohort_start_date)) |>
        dplyr::mutate(cohort_end_date = dplyr::if_else(
          is.na(.data$cohort_end_date),
          .data$cohort_start_date,
          .data$cohort_end_date
        )) |>
        dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
      if (tempCohort |>
          utils::head(1) |>
          dplyr::tally() |>
          dplyr::pull("n") > 0) {
        cohorts[[k]] <- tempCohort
      }
    } else {
      cli::cli_warn(c(
        "x" = "Domain {.strong {domain}} ({n} concept{?s}) excluded because table {table} is not present in the cdm."
      ))
    }
  }

  cohorts <- cohorts %>%
    purrr::discard(is.null)

  if (length(cohorts) == 0) {
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])

  } else if (length(cohorts) == 1) {
    cohort <- cohorts[[1]]
  } else { # more than one cohort, so will put together
    cli::cli_inform(c("i" = "Combining tables."))
    cohort <- cohorts[[1]] |>
      dplyr::compute(name = paste0(name, "_1"),
                     temporary = FALSE,
                     overwrite = TRUE)
    for (k in 2:length(cohorts)) {
      cohort <- cohort |>
        dplyr::union_all(cohorts[[k]])  |>
        dplyr::compute(name = paste0(name, "_k_", k),
                       temporary = FALSE,
                       overwrite = TRUE)
    }
  }

  cohort <- cohort |>
    dplyr::compute(name = name,
                   temporary = FALSE)

  omopgenerics::dropTable(cdm = cdm,
                          name = dplyr::contains(paste0(name, "_k_")))

  cohort
}

fulfillCohortReqs <- function(cdm, name){
  # if start is out of observation, drop cohort entry
  # if end is after observation end, set cohort end as observation end
 cdm[[name]] |>
    PatientProfiles::addDemographics(
      age = FALSE,
      sex = FALSE,
      priorObservationType = "date",
      futureObservationType = "date"
    ) |>
    dplyr::filter(
      .data$prior_observation <= .data$cohort_start_date
    ) |>
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      .data$future_observation >= .data$cohort_end_date,
      .data$cohort_end_date, .data$future_observation)
    ) |>
    dplyr::select(-"prior_observation",
                  -"future_observation") |>
    joinOverlap(gap = 0)
}


conceptSetToCohortSet <- function(conceptSet){
  dplyr::tibble("cohort_name" = names(conceptSet)) |>
    dplyr::mutate("cohort_definition_id" = dplyr::row_number())
}

conceptSetToCohortCodelist <- function(conceptSet){

  cohortSet <- dplyr::tibble("cohort_name" = names(conceptSet)) |>
    dplyr::mutate("cohort_definition_id" = dplyr::row_number())

  lapply(conceptSet, dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "cohort_name") |>
    dplyr::inner_join(cohortSet, by = "cohort_name") |>
    dplyr::select("cohort_definition_id",
                  "concept_id"  = "value",
                  "codelist_name" = "cohort_name") |>
    dplyr::mutate("type" = "index event")
}

# upload codes to cdm and add domain
uploadCohortCodelistToCdm <- function(cdm, cohortCodelist, tableCohortCodelist){
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = tableCohortCodelist,
    table = cohortCodelist |>
      dplyr::select("cohort_definition_id",
                    "concept_id")
  )

  cdm[[tableCohortCodelist]] <- cdm[[tableCohortCodelist]] |>
    dplyr::left_join(cdm[["concept"]] |>
                       dplyr::select("concept_id", "domain_id"),
                     by = "concept_id") |>
    dplyr::mutate("domain_id" = tolower(.data$domain_id)) |>
    dplyr::compute(name = tableCohortCodelist,
                   temporary = FALSE,
                   overwrite = TRUE)

  cdm
}

reportConceptsFromUnsopportedDomains <- function(cdm,
                                                 tableCohortCodelist,
                                                 supportedDomains){

  ud <- cdm[[tableCohortCodelist]] |>
    dplyr::group_by(.data$domain_id) |>
    dplyr::tally() |>
    dplyr::collect() |>
    dplyr::filter(!.data$domain_id %in% .env$supportedDomains)
  for (k in seq_len(nrow(ud))) {
    cli::cli_inform(c(
      "x" = "Domain {.strong {ud$domain_id[k]}} ({ud$n[k]} concept{?s}) excluded because it is not supported."
    ))
  }
}
