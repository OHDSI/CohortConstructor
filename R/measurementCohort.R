#' Create cohorts from the measurement table based on a concept set and
#' required measurement values
#'
#' @param cdm A cdm reference.
#' @param conceptSet A conceptSet, which can either be a codelist
#' or a conceptSetExpression.
#' @param name Name of the cohort in the cdm object.
#' @param valueAsConcept A vector of cohort IDs used to filter measurements.
#' Only measurements with these values in the `value_as_concept_id` column of
#' the measurement table will be included. If NULL all entries independently of
#' their value as concept will be considered.
#' @param valueAsNumber A named list indicating the range of values and the unit
#' they correspond to, as follows:
#' list("unit_concept_id" = c(rangeValue1, rangeValue2)). If NULL, all entries
#' independently of their value as number will be included.
#'
#' @export
#'
#' @description
#' This function creates cohorts from concepts in the measurement table. Value
#' of the target measurements in `conceptSet` can be specified through
#' `valueAsConcept` and `valueAsNumber`. If both NULL, conceptSets will not be
#' restricted to any value, if one of them is not NULL, value restrictions will
#' be based on the non NULL entry, and if both are specified, concepts
#' fulfilling one OR the other will be included.
#'
#' @return A cohort table
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(con = NULL)
#' cdm$concept <- cdm$concept |>
#'   dplyr::union_all(
#'     dplyr::tibble(
#'       concept_id = c(4326744, 4298393, 45770407, 8876, 4124457),
#'       concept_name = c("Blood pressure", "Systemic blood pressure",
#'                        "Baseline blood pressure", "millimeter mercury column",
#'                        "Normal range"),
#'       domain_id = "Measurement",
#'       vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "UCUM", "SNOMED"),
#'       standard_concept = "S",
#'       concept_class_id = c("Observable Entity", "Observable Entity",
#'                            "Observable Entity", "Unit", "Qualifier Value"),
#'       concept_code = NA,
#'       valid_start_date = NA,
#'       valid_end_date = NA,
#'       invalid_reason = NA
#'     )
#'   )
#' cdm$measurement <- dplyr::tibble(
#'   measurement_id = 1:4,
#'   person_id = c(1, 1, 2, 3),
#'   measurement_concept_id = c(4326744, 4298393, 4298393, 45770407),
#'   measurement_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08",
#'                                 "2015-02-19")),
#'   measurement_type_concept_id = NA,
#'   value_as_number = c(100, 125, NA, NA),
#'   value_as_concept_id = c(0, 0, 0, 4124457),
#'   unit_concept_id = c(8876, 8876, 0, 0)
#' )
#' cdm <- CDMConnector::copyCdmTo(
#'   con = DBI::dbConnect(duckdb::duckdb()),
#'   cdm = cdm, schema = "main")
#'
#' cdm$cohort <- measurementCohort(
#'   cdm = cdm,
#'   name = "cohort",
#'   conceptSet = list("normal_blood_pressure" = c(4326744, 4298393, 45770407)),
#'   valueAsConcept = c(4124457),
#'   valueAsNumber = list("8876" = c(70, 120))
#' )
#'
#' cdm$cohort
#' }
measurementCohort <- function(cdm,
                              conceptSet,
                              name,
                              valueAsConcept = NULL,
                              valueAsNumber = NULL) {
  # initial input validation
  cdm <- validateCdm(cdm)
  name <- validateName(name)
  if (length(conceptSet) == 0) {
    cli::cli_inform(c("i" = "Empty codelist provided, returning empty cohort"))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }
  conceptSet <- validateConceptSet(conceptSet)
  assertNumeric(valueAsConcept, integerish = TRUE, null = TRUE)
  validateValueAsNumber(valueAsNumber)

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
    dplyr::filter(!tolower(.data$domain_id) %in% "measurement" | is.na(.data$domain_id)) |>
    dplyr::tally() |>
    dplyr::pull("n")

  if (ud > 0) {
    cli::cli_inform(c(
      "x" = "{.strong {ud}} concept{?s} excluded because don't correspond to the `Measurement` domain."
    ))
  }

  cohortCodelist <- cohortCodelist |>
    dplyr::filter(tolower(.data$domain_id) %in% "measurement") |>
    dplyr::compute()

  cohort <- cdm$measurement |>
    dplyr::select(
      "subject_id" = "person_id",
      "concept_id" = "measurement_concept_id",
      "cohort_start_date" = "measurement_date",
      "cohort_end_date" = "measurement_date",
      "value_as_number",
      "value_as_concept_id",
      "unit_concept_id"
    ) |>
    dplyr::inner_join(
      cohortCodelist |>
        dplyr::select("concept_id", "cohort_definition_id"),
      by = "concept_id"
    ) |>
    dplyr::filter(!is.na(.data$cohort_start_date))

  if (!is.null(valueAsConcept)) {
    value <- cohort |> dplyr::pull("value_as_concept_id") |> unique()
    matches <- valueAsConcept %in% value
    matching_ids <- valueAsConcept[!matches]

    if (length(matching_ids) > 0) {
      cli::cli_inform(c(
        "i" = "These valueAsConcept don't exist for the input measurement concepts:",
        paste(matching_ids, collapse = ", ")
      ))
    }
  }

  if (!is.null(valueAsNumber)) {
    unit <- cohort |> dplyr::pull("unit_concept_id") |> unique()
    matches <-
      as.numeric(names(valueAsNumber)) %in% as.numeric(unit)
    matching_ids <- valueAsNumber[!matches]

    if (length(matching_ids) > 0) {
      cli::cli_inform(c(
        "i" = "These unit concepts don't exist for the input measurement concepts:",
        paste(matching_ids, collapse = ", ")
      ))
    }
  }


  if (cohort |> dplyr::tally() |> dplyr::pull("n") == 0) {
    cli::cli_inform(c("i" = "No table could be subsetted, returning empty cohort."))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name) # TODO: overwritte to TRUE when omopgenerics in CRAN
    cdm[[name]] <- cdm[[name]] |>
      omopgenerics::newCohortTable(
        cohortSetRef = cohortSet,
        cohortAttritionRef = NULL,
        cohortCodelistRef = cohortCodelist |> dplyr::collect()
      )
    return(cdm[[name]])
  }

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) # To move after line 90 when omopgenerics in CRAN

  cli::cli_inform(c("i" = "Getting records in observation."))
  cohort <- cohort |>
    PatientProfiles::addDemographics(
      age = FALSE,
      sex = FALSE,
      priorObservationType = "date",
      futureObservationType = "date"
    ) |>
    dplyr::filter(
      .data$prior_observation <= .data$cohort_start_date,
      .data$future_observation >= .data$cohort_end_date
    ) |>
    dplyr::select(-"prior_observation", -"future_observation") |>
    dplyr::compute(name = name, temporary = FALSE)


  if (!is.null(valueAsConcept) | !is.null(valueAsNumber)) {
    cli::cli_inform(c("i" = "Applying measurement requirements."))
    filterExpr <- getFilterExpression(valueAsConcept, valueAsNumber)
    cohort <- cohort |>
      dplyr::filter(!!!filterExpr) |>
      dplyr::compute(name = name, temporary = FALSE)

  if (cohort |> dplyr::tally() |> dplyr::pull("n") == 0){
    cli::cli_warn("There are no subjects with the specified value_as_concept_id or value_as_number.")
  }

  }

  cohort <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
    ) |>
    dplyr::distinct() |>
    dplyr::compute(name = name, temporary = FALSE)

  cli::cli_inform(c("i" = "Creating cohort attributes."))

  cohort <- cohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohortCodelist |> dplyr::collect(),
      .softValidation = TRUE
    )

  cli::cli_inform(c("v" = "Cohort {.strong {name}} created."))

  return(cohort)
}

getFilterExpression <- function(valueAsConcept, valueAsNumber) {
  expFilter <- character()
  if (!is.null(valueAsNumber)) {
    for (ii in seq_along(valueAsNumber)) {
      expFilter[ii] <- glue::glue(
        "(.data$unit_concept_id %in% {names(valueAsNumber)[ii]} &
      .data$value_as_number >= {valueAsNumber[[ii]][1]} &
      .data$value_as_number <= {valueAsNumber[[ii]][2]})"
      )
    }
  } else {
    ii <- 0
  }

  if (!is.null(valueAsConcept)) {
    expFilter[ii+1] <- ".data$value_as_concept_id %in% .env$valueAsConcept"
  }

  return(paste0(expFilter, collapse = " | ") |>  rlang::parse_exprs())
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
