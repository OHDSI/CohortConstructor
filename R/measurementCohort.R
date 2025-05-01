#' Create measurement-based cohorts
#'
#' @description
#' `measurementCohort()` creates cohorts based on patient records contained
#' in the measurement table. This function extends the `conceptCohort()` as it
#' allows for measurement values associated with the records to be specified.
#'
#' * If `valueAsConcept` and `valueAsNumber` are NULL then no requirements on
#' of the values associated with measurement records and using
#' `measurementCohort()` will lead to the same result as using `conceptCohort()`
#' (so long as all concepts are from the measurement domain).
#' * If one of `valueAsConcept` and `valueAsNumber` is not NULL then records
#' will be required to have values that satisfy the requirement specified.
#' * If both `valueAsConcept` and `valueAsNumber` are not NULL, records will
#' be required to have values that fulfill _either_ of the requirements
#'
#' @inheritParams cdmDoc
#' @inheritParams conceptSetDoc
#' @inheritParams nameDoc
#' @param valueAsConcept A vector of cohort IDs used to filter measurements.
#' Only measurements with these values in the `value_as_concept_id` column of
#' the measurement table will be included. If NULL all entries independent of
#' their value as concept will be considered.
#' @param valueAsNumber A list indicating the range of values and the unit
#' they correspond to, as follows:
#' list("unit_concept_id" = c(rangeValue1, rangeValue2)). If no name is supplied
#' in the list, no requirement on unit concept id will be applied. If NULL, all
#' entries independent of their value as number will be included.
#' @param table Name of OMOP tables to search for records of the concepts
#' provided. Options are "measurement" and/or "observation".
#' @param inObservation If TRUE, only records in observation will be used. If
#' FALSE, records before the start of observation period will be considered,
#' with startdate the start of observation.
#'
#' @export
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
#'   "2015-02-19")),
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
#'   valueAsNumber = list("8876" = c(70, 120)),
#'   inObservation = TRUE
#' )
#'
#' cdm$cohort
#'
#'# You can also create multiple measurement cohorts, and include records
#'# outside the observation period.
#'
#' cdm$cohort2 <- measurementCohort(
#'   cdm = cdm,
#'   name = "cohort2",
#'   conceptSet = list("normal_blood_pressure" = c(4326744, 4298393, 45770407),
#'                   "high_blood_pressure" = c(4326744, 4298393, 45770407)),
#'   valueAsConcept = c(4124457),
#'   valueAsNumber = list("8876" = c(70, 120),
#'                        "8876" = c(121, 200)),
#'   inObservation = FALSE
#' )
#'
#' cdm$cohort2
#'
#' }
measurementCohort <- function(cdm,
                              conceptSet,
                              name,
                              valueAsConcept = NULL,
                              valueAsNumber = NULL,
                              table = c("measurement", "observation"),
                              inObservation = TRUE) {
  # initial input validation
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(cdm)
  conceptSet <- omopgenerics::validateConceptSetArgument(conceptSet, cdm)
  omopgenerics::assertNumeric(valueAsConcept, integerish = TRUE, null = TRUE)
  validateValueAsNumber(valueAsNumber)
  omopgenerics::assertLogical(inObservation, length = 1)
  if (length(table) == 0) cli::cli_abort("`table` argument can't be empty. Options are 'measurement' and 'observation'.")
  table <- validateTable(table)

  useIndexes <- getOption("CohortConstructor.use_indexes")

  # empty concept set
  cohortSet <- conceptSetToCohortSet(conceptSet, cdm)
  if (length(conceptSet) == 0) {
    cli::cli_inform(c("i" = "Empty codelist provided, returning empty cohort"))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    cdm[[name]] <- cdm[[name]] |>
      # To get columns "cdm_version" and "vocabulary_version" in the set
      omopgenerics::newCohortTable(cohortSetRef = cohortSet)
    return(cdm[[name]])
  }

  # codelist attribute
  cohortCodelist <- conceptSetToCohortCodelist(conceptSet)
  tableCohortCodelist <- omopgenerics::uniqueTableName()
  cdm <- uploadCohortCodelistToCdm(
    cdm = cdm,
    cohortCodelist = cohortCodelist,
    tableCohortCodelist = tableCohortCodelist,
    table = table
  )
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[tableCohortCodelist]],
      cols = "concept_id"
    )
  }

  # get cohort entries from omop records
  cdm[[name]] <- unerafiedConceptCohort(
    cdm = cdm,
    conceptSet = conceptSet,
    cohortSet = cohortSet,
    cohortCodelist = cohortCodelist,
    tableCohortCodelist = tableCohortCodelist,
    name = name,
    extraCols = c("value_as_number", "value_as_concept_id", "unit_concept_id"),
    exit = "event_start_date",
    useSourceFields = FALSE,
    subsetIndividuals = NULL
  )

  if (cdm[[name]] |> dplyr::tally() |> dplyr::pull("n") == 0) {
    cli::cli_inform(c("i" = "No table could be subsetted, returning empty cohort."))
    cohortAttrition <- attrition(cdm[[name]])
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    cdm[[name]] <- cdm[[name]] |>
      omopgenerics::newCohortTable(
        cohortSetRef = cohortSet,
        cohortAttritionRef = NULL,
        cohortCodelistRef = cohortCodelist
      )
    omopgenerics::dropSourceTable(cdm = cdm, name = tableCohortCodelist)
    return(cdm[[name]])
  }

  if (!is.null(valueAsConcept) || !is.null(valueAsNumber)) {
    cli::cli_inform(c("i" = "Applying measurement requirements."))
    filterExpr <- getFilterExpression(valueAsConcept, valueAsNumber)
    cdm[[name]] <- cdm[[name]] |>
      dplyr::filter(!!!filterExpr) |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_measurementCohort_reqs_")
    if (cdm[[name]] |> dplyr::tally() |> dplyr::pull("n") == 0) {
      cli::cli_warn(
        "There are no subjects with the specified value_as_concept_id or value_as_number."
      )
    }
  }

  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet,
      cohortCodelistRef = cohortCodelist,
      .softValidation = TRUE
    )

  if (cdm[[name]] |> dplyr::tally() |> dplyr::pull("n") == 0) {
    cli::cli_inform(c("i" = "No table could be subsetted, returning empty cohort."))
    cohortAttrition <- attrition(cdm[[name]])
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    cdm[[name]] <- cdm[[name]] |>
      omopgenerics::newCohortTable(
        cohortSetRef = cohortSet,
        cohortAttritionRef = cohortAttrition,
        cohortCodelistRef = cohortCodelist
      )
    omopgenerics::dropSourceTable(cdm = cdm, name = tableCohortCodelist)
    return(cdm[[name]])
  }

  cli::cli_inform(c("i" = "Getting records in observation."))
  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }
  cdm[[name]] <- fulfillCohortReqs(cdm, name, inObservation = inObservation, type = "start", useIndexes = useIndexes)

  cdm[[name]] <- cdm[[name]] |>
    dplyr::select("cohort_definition_id",
                  "subject_id",
                  "cohort_start_date",
                  "cohort_end_date") |>
    dplyr::distinct() |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_measurementCohort_distinct_") |>
    omopgenerics::recordCohortAttrition(reason = "Distinct measurement records")

  cli::cli_inform(c("i" = "Creating cohort attributes."))

  cdm[[name]] <- cdm[[name]] |> omopgenerics::newCohortTable()

  cli::cli_inform(c("v" = "Cohort {.strong {name}} created."))

  omopgenerics::dropSourceTable(cdm = cdm, name = tableCohortCodelist)

  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cdm[[name]])
}

getFilterExpression <- function(valueAsConcept, valueAsNumber) {
  expFilter <- character()
  if (!is.null(valueAsNumber)) {
    for (ii in seq_along(valueAsNumber)) {
      if(!is.null(names(valueAsNumber)[ii])){
        expFilter[ii] <- glue::glue(
          "(.data$unit_concept_id %in% {names(valueAsNumber)[ii]} &
      .data$value_as_number >= {valueAsNumber[[ii]][1]} &
      .data$value_as_number <= {valueAsNumber[[ii]][2]})"
        )
      } else {
        expFilter[ii] <- glue::glue(
          "(.data$value_as_number >= {valueAsNumber[[ii]][1]} &
          .data$value_as_number <= {valueAsNumber[[ii]][2]})"
        )
      }
    }
  } else {
    ii <- 0
  }

  if (!is.null(valueAsConcept)) {
    expFilter[ii + 1] <- ".data$value_as_concept_id %in% .env$valueAsConcept"
  }

  return(paste0(expFilter, collapse = " | ") |>  rlang::parse_exprs())
}

addDomains <- function(cohortCodelist, cdm, name) {
  # insert table as temporary
  tmpName <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = tmpName,
                                   table = cohortCodelist)

  cohortCodelist <- cdm[["concept"]] |>
    dplyr::select("concept_id", "domain_id") |>
    dplyr::right_join(cdm[[tmpName]], by = "concept_id") |>
    dplyr::mutate("domain_id" = tolower(.data$domain_id)) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_addDomains_")

  omopgenerics::dropSourceTable(cdm = cdm, name = tmpName)

  return(cohortCodelist)
}
