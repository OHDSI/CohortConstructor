#' Create measurement-based cohorts
#'
#' @description
#' `measurementCohort()` creates cohorts based on patient records from the
#' measurement or observation tables. It extends the function `conceptCohort()`
#' by allowing users to specify measurement values associated with those records.
#'
#' This function supports both concept-based and value-based filtering:
#'
#' * Either `valueAsConcept` or `valueAsNumber` must be provided.
#' * If one of them is specified (not NULL), only records that satisfy the other
#'   filter will be included.
#' * If both are provided, records that meet _either_ filter will be included.
#'
#' @inheritParams cdmDoc
#' @inheritParams conceptSetDoc
#' @inheritParams nameDoc
#' @inheritParams baseCohortDoc
#'
#' @param valueAsConcept A named list defining cohorts based on measurement
#' values as concept IDs. Each element name defines the name of cohort to
#' create, and its value is a vector of concept IDs used to filter measurements
#' by `value_as_concept_id.`
#' If NULL, all records will be included regardless of `value_as_concept_id.`
#'
#' For instance, to create two bmi cohorts from a bmi `conceptSet` we can do the
#' following:
#' `valueAsConcept = list(high_bmi = c(4328749, 35819253), low_bmi = c(4267416, 45881666))`
#'
#' See more examples in the function examples.
#'
#' @param valueAsNumber A named list defining cohorts based on numeric
#' measurement ranges.
#' Each list element should contain one or more numeric vectors of length two,
#' specifying the allowed range(s) for the measurement value.
#' If the numeric vector is named, the name should correspond to the
#' `unit_concept_id` that will be used for that range.
#'
#' For example, the following creates a cohort named "low_weight" based on
#' measurements recorded in kilograms (unit_concept_id = 9529) and stones
#' (unit_concept_id = 21498905):
#' `valueAsNumber = list("low_weight" = list("9529" = c(30, 40), "21498905" = c(4.7, 6.3))) `
#'
#' See the examples below for how to define multiple cohorts based on different measurement filters.
#'
#' @param table Character vector specifying which OMOP tables to use.
#' Accepts "measurement", "observation", or both.
#'
#' @return A cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' library(omock)
#' library(dplyr)
#'
#' cdm <- mockVocabularyTables(concept = tibble(
#'   concept_id = c(4326744, 4298393, 45770407, 8876, 4124457),
#'   concept_name = c("Blood pressure", "Systemic blood pressure",
#'                    "Baseline blood pressure", "millimeter mercury column",
#'                    "Normal range"),
#'   domain_id = "Measurement",
#'   vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "UCUM", "SNOMED"),
#'   standard_concept = "S",
#'   concept_class_id = c("Observable Entity", "Observable Entity",
#'                        "Observable Entity", "Unit", "Qualifier Value"),
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date(NA_character_),
#'   valid_end_date = as.Date(NA_character_),
#'   invalid_reason = NA_character_
#' )) |>
#'   mockCdmFromTables(tables = list(
#'     measurement = tibble(
#'       measurement_id = 1:4L,
#'       person_id = c(1L, 1L, 2L, 3L),
#'       measurement_concept_id = c(4326744L, 4298393L, 4298393L, 45770407L),
#'       measurement_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19")),
#'       measurement_type_concept_id = 0L,
#'       value_as_number = c(100, 125, NA, NA),
#'       value_as_concept_id = c(0L, 0L, 0L, 4124457L),
#'       unit_concept_id = c(8876L, 8876L, 0L, 0L)
#'    )
#'  ))
#'
#' # create one cohort of blood pressure measurements indicating normal levels
#' cdm$cohort <- measurementCohort(
#'   cdm = cdm,
#'   name = "cohort",
#'   conceptSet = list("blood_pressure" = c(4326744, 4298393, 45770407)),
#'   valueAsConcept = list("normal_blood_preassure" = c(4124457)),
#'   valueAsNumber = list("normal_blood_preassure" = list("8876" = c(70, 120))),
#'   useRecordsBeforeObservation = FALSE
#' )
#'
#' cdm$cohort
#'
#' # create two cohorts of blood preassure measurements, one with results
#' # indicating normal blood pressure and the other inidcating high
#'
#' cdm$cohort2 <- measurementCohort(
#'   cdm = cdm,
#'   name = "cohort2",
#'   conceptSet = list("blood_pressure" = c(4326744, 4298393, 45770407)),
#'   valueAsConcept = list(
#'     "normal_blood_pressure" = 4124457,
#'     "high_blood_pressure" = 4328749
#'   ),
#'   valueAsNumber = list(
#'     "normal_blood_pressure" = list("8876" = c(70, 120)),
#'     "high_blood_pressure" = list("8876" = c(121, 200))
#'   ),
#'   useRecordsBeforeObservation = TRUE
#' )
#'
#' cdm$cohort2 |> settings()
#'
#' }
measurementCohort <- function(cdm,
                              conceptSet,
                              name,
                              valueAsConcept = NULL,
                              valueAsNumber = NULL,
                              table = NULL,
                              useRecordsBeforeObservation = FALSE,
                              useSourceFields = FALSE,
                              subsetCohort = NULL,
                              subsetCohortId = NULL) {
  # initial input validation
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(cdm)
  conceptSet <- omopgenerics::validateConceptSetArgument(conceptSet, cdm)
  if (length(conceptSet) > 1) cli::cli_abort("Only one codelist in `conceptSet` is allowed.")
  if (length(valueAsConcept) == 0 & length(valueAsNumber) == 0) {
    cli::cli_abort("Measurement values must be specified. For a cohort based on measurements without filtering use `conceptCohort`.")
  }
  validateValueAsConcept(valueAsConcept)
  validateValueAsNumber(valueAsNumber)
  omopgenerics::assertLogical(useSourceFields, length = 1)
  omopgenerics::assertLogical(useRecordsBeforeObservation, length = 1)
  omopgenerics::assertCharacter(subsetCohort, length = 1, null = TRUE)
  if (!is.null(subsetCohort)) {
    subsetCohort <- omopgenerics::validateCohortArgument(cdm[[subsetCohort]])
    subsetCohortId <- omopgenerics::validateCohortIdArgument({{subsetCohortId}}, subsetCohort, validation = "warning")
  }
  omopgenerics::assertChoice(table, choices = c("measurement", "observation"), null = TRUE)
  table <- validateTable(table)

  useIndexes <- getOption("CohortConstructor.use_indexes")
  cohortSet <- measurementConceptSet(valueAsNumber, valueAsConcept, cdm)
  # empty concept set
  if (length(conceptSet) == 0) {
    cli::cli_inform(c("i" = "Empty codelist provided, returning empty cohort"))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    cdm[[name]] <- cdm[[name]] |>
      omopgenerics::newCohortTable(cohortSetRef = cohortSet)
    return(cdm[[name]])
  }

  # attributes
  tablePref <- omopgenerics::tmpPrefix()
  on.exit(omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePref)))
  tableCohortCodelist <- omopgenerics::uniqueTableName(prefix = tablePref)
  tableCohortSet <- omopgenerics::uniqueTableName(prefix = tablePref)
  tempCodelist <- conceptSetToCohortCodelist(conceptSet)
  cohortCodelist <- conceptSetToCohortCodelist(
    rep(conceptSet, nrow(cohortSet)) |> rlang::set_names(cohortSet$cohort_name)
  ) |>
    dplyr::mutate(codelist_name = names(conceptSet))
  cdm <- uploadCohortCodelistToCdm(
    cdm = cdm,
    cohortCodelist = tempCodelist,
    tableCohortCodelist = tableCohortCodelist,
    table = table
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = tableCohortSet,
    table = cohortSet
  )
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[tableCohortCodelist]],
      cols = "concept_id"
    )
  }

  # subsetCohort
  if (!is.null(subsetCohort)) {
    subsetName <- omopgenerics::uniqueTableName(prefix = tablePref)
    subsetIndividuals <- subsetCohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$subsetCohortId) |>
      dplyr::distinct(.data$subject_id) |>
      dplyr::compute(name = subsetName, temporary = FALSE,
                     logPrefix = "CohortConstructor_conceptCohort_subsetCohort_")
    if (omopgenerics::isTableEmpty(subsetIndividuals)) {
      cli::cli_warn("There are no individuals in the `subsetCohort` and `subsetCohortId` provided. Returning empty cohort.")
      cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
      cdm[[name]] <- cdm[[name]] |>
        omopgenerics::newCohortTable(
          cohortSetRef = cohortSet,
          cohortAttritionRef = dplyr::tibble(
            "cohort_definition_id" = cohortSet$cohort_definition_id,
            "number_records" = 0L, "number_subjects" = 0L,
            "reason_id" = 1L, "reason" = "Qualifying initial events",
            "excluded_records" = NA_integer_, "excluded_subjects" = NA_integer_
          )
        )
      return(cdm[[name]])
    }
    if (!isFALSE(useIndexes)) {
      addIndex(
        cohort = subsetIndividuals,
        cols = "subject_id"
      )
    }
  } else {
    subsetIndividuals <- NULL
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
    useSourceFields = useSourceFields,
    subsetIndividuals = subsetIndividuals,
    tablePrefix = tablePref
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
    return(cdm[[name]])
  }

  cli::cli_inform(c("i" = "Applying measurement requirements."))
  mutateExpr <- getMutateExpression(cohortSet, valueAsConcept, valueAsNumber)
  cdm[[name]] <- cdm[[name]] |>
    dplyr::mutate(!!!mutateExpr) |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_measurementCohort_mutate_"
    ) |>
    tidyr::pivot_longer(cols = cohortSet$cohort_name, names_to = "cohort_name") |>
    dplyr::select(!c("value_as_number", "value_as_concept_id", "unit_concept_id", "cohort_definition_id")) |>
    dplyr::inner_join(cdm[[tableCohortSet]], by = "cohort_name") |>
    dplyr::filter(.data$value == TRUE) |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_measurementCohort_filter_"
    ) |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohortCodelist,
      .softValidation = TRUE
    )

  if (cdm[[name]] |> dplyr::tally() |> dplyr::pull("n") == 0) {
    cli::cli_warn(
      "There are no subjects with the specified value_as_concept_id or value_as_number."
    )
  }

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
  cdm[[name]] <- fulfillCohortReqs(
    cdm, name,
    useRecordsBeforeObservation = useRecordsBeforeObservation,
    type = "start",
    useIndexes = useIndexes
  )

  cdm[[name]] <- cdm[[name]] |>
    dplyr::select("cohort_definition_id",
                  "subject_id",
                  "cohort_start_date",
                  "cohort_end_date") |>
    dplyr::distinct() |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_measurementCohort_distinct_") |>
    omopgenerics::recordCohortAttrition(reason = "Drop duplicate records")

  cli::cli_inform(c("i" = "Creating cohort attributes."))

  cdm[[name]] <- cdm[[name]] |> omopgenerics::newCohortTable()

  cli::cli_inform(c("v" = "Cohort {.strong {name}} created."))

  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cdm[[name]])
}

getMutateExpression <- function(cohortSet, valueAsConcept, valueAsNumber) {
  exprMutate <- NULL
  for (cohortName in cohortSet$cohort_name) {
    cohortFilter <- character()
    numberFilter <- valueAsNumber[[cohortName]]
    conceptFilter <- valueAsConcept[[cohortName]]

    # numeric filter
    if (!is.null(numberFilter)) {
      for (ii in seq_along(numberFilter)) {
        if(!is.null(names(numberFilter)[ii])){
          cohortFilter[ii] <- glue::glue(
            "(.data$unit_concept_id %in% {as.integer(names(numberFilter)[ii])} &
      .data$value_as_number >= {numberFilter[[ii]][1]} &
      .data$value_as_number <= {numberFilter[[ii]][2]})"
          )
        } else {
          cohortFilter[ii] <- glue::glue(
            "(.data$value_as_number >= {numberFilter[[ii]][1]} &
          .data$value_as_number <= {numberFilter[[ii]][2]})"
          )
        }
      }
    } else {
      ii <- 0
    }
    # concept filter
    if (!is.null(valueAsConcept)) {
      cohortFilter[ii + 1] <- glue::glue(".data$value_as_concept_id %in% c({paste0(conceptFilter, collapse = ', ')})")
    }
    # union
    exprMutate <- c(exprMutate, paste0("dplyr::if_else(", paste0(cohortFilter, collapse = " | "), ", TRUE, FALSE)"))
  }

  return(exprMutate |> rlang::parse_exprs() |> rlang::set_names(cohortSet$cohort_name))
}

measurementConceptSet <- function(valueAsNumber, valueAsConcept, cdm) {
  cohortNames <- unique(c(names(valueAsNumber), names(valueAsConcept))) |> sort()
  dplyr::tibble(
    "cohort_definition_id" = as.integer(1:length(cohortNames)),
    "cohort_name" = cohortNames,
    "cdm_version" = attr(cdm, "cdm_version"),
    "vocabulary_version" = CodelistGenerator::vocabularyVersion(cdm)
  )
}
