#' Create cohorts based on a concept set
#'
#' @description
#' `conceptCohort()` creates a cohort table from patient records
#' from the clinical tables in the OMOP CDM.
#'
#' The following tables are currently supported for creating concept
#' cohorts:
#' * condition_occurrence
#' * device_exposure
#' * drug_exposure
#' * measurement
#' * observation
#' * procedure_occurrence
#' * visit_occurrence
#'
#' Cohort duration is based on record start and end (e.g.
#' condition_start_date and condition_end_date for records coming
#' from the condition_occurrence tables). So that the resulting table
#' satisfies the requirements of an OMOP CDM cohort table:
#' * Cohort entries will not overlap. Overlapping records will be
#' combined based on the overlap argument.
#' * Cohort entries will not go out of observation. If a record starts
#' outside of an observation period it will be silently ignored. If a
#' record ends outside of an observation period it will be trimmed so
#' as to end at the preceding observation period end date.
#'
#' @inheritParams cdmDoc
#' @inheritParams conceptSetDoc
#' @inheritParams nameDoc
#' @param exit How the cohort end date is defined. Can be either
#' "event_end_date" or "event_start_date".
#' @param overlap How to deal with overlapping records. In all
#' cases cohort start will be set as the earliest start date. If
#' "merge", cohort end will be the latest end date. If "extend",
#' cohort end date will be set by adding together the total days
#' from each of the overlapping records.
#' @param useSourceFields If TRUE, the source concept_id fields will also be
#' used when identifying relevant clinical records. If FALSE, only the standard
#' concept_id fields will be used.
#' @param subsetCohort  A character refering to a cohort table containing
#' individuals for whom cohorts will be generated. Only individuals in this
#' table will appear in the generated cohort.
#' @param subsetCohortId Optional. Specifies cohort IDs from the `subsetCohort`
#' table to include. If none are provided, all cohorts from the `subsetCohort`
#' are included.
#' @param table Name of OMOP tables to search for records of the concepts
#' provided. If NULL, each concept will be search at the assigned domain in
#' the concept table.
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
#'
#' cdm <- mockCohortConstructor(conditionOccurrence = TRUE, drugExposure = TRUE)
#'
#' cdm$cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 444074), name = "cohort")
#'
#' cdm$cohort |> attrition()
#'
#' # Create a cohort based on a concept set. The cohort exit is set to the event start date.
#' # If two records overlap, the cohort end date is set as the sum of the duration of
#' # all overlapping records. Only individuals included in the existing `cohort` will be considered.
#'
#' conceptSet <- list("nitrogen" = c(35604434, 35604439),
#' "potassium" = c(40741270, 42899580, 44081436))
#'
#' cohort_drugs <- conceptCohort(cdm,
#'                              conceptSet = conceptSet,
#'                              name = "cohort_drugs",
#'                              exit = "event_start_date",
#'                              overlap = "extend",
#'                              subsetCohort = "cohort"
#' )
#'
#' cohort_drugs |> attrition()
#' }
conceptCohort <- function(cdm,
                          conceptSet,
                          name,
                          exit = "event_end_date",
                          overlap = "merge",
                          inObservation = TRUE,
                          table = NULL,
                          useSourceFields = FALSE,
                          subsetCohort = NULL,
                          subsetCohortId = NULL) {

  # initial input validation
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(cdm)
  conceptSet <- omopgenerics::validateConceptSetArgument(conceptSet, cdm)
  omopgenerics::assertChoice(exit, c("event_start_date", "event_end_date"))
  omopgenerics::assertChoice(overlap, c("merge", "extend"), length = 1)
  omopgenerics::assertLogical(useSourceFields, length = 1)
  omopgenerics::assertLogical(inObservation, length = 1)
  omopgenerics::assertCharacter(subsetCohort, length = 1, null = TRUE)
  if (!is.null(subsetCohort)) {
    subsetCohort <- omopgenerics::validateCohortArgument(cdm[[subsetCohort]])
    subsetCohortId <- omopgenerics::validateCohortIdArgument({{subsetCohortId}}, subsetCohort, validation = "warning")
  }
  table <- validateTable(table)

  useIndexes <- getOption("CohortConstructor.use_indexes")

  # prefix for names
  tmpPref <- omopgenerics::tmpPrefix()

  # empty concept set
  cohortSet <- conceptSetToCohortSet(conceptSet, cdm)
  if (length(conceptSet) == 0) {
    cli::cli_inform(c("i" = "Empty codelist provided, returning empty cohort"))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    cdm[[name]] <- cdm[[name]] |>
      omopgenerics::newCohortTable(cohortSetRef = cohortSet)
    return(cdm[[name]])
  }

  # codelist attribute
  cohortCodelist <- conceptSetToCohortCodelist(conceptSet)
  tableCohortCodelist <- omopgenerics::uniqueTableName(prefix = tmpPref)
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

  # subsetCohort
  if (!is.null(subsetCohort)) {
    subsetName <- omopgenerics::uniqueTableName(prefix = tmpPref)
    subsetIndividuals <- subsetCohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$subsetCohortId) |>
      dplyr::distinct(.data$subject_id) |>
      dplyr::compute(name = subsetName, temporary = FALSE,
                     logPrefix = "CohortConstructor_conceptCohort_subsetCohort_")
    if (omopgenerics::isTableEmpty(subsetIndividuals)) {
      omopgenerics::dropSourceTable(cdm = cdm, name = subsetName)
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

  # report codes from unsupported domains
  reportConceptsFromUnsopportedDomains(
    cdm = cdm,
    tableCohortCodelist = tableCohortCodelist,
    supportedDomains = domainsData$domain_id
  )

  # get cohort entries from omop records
  cdm[[name]] <- unerafiedConceptCohort(
    cdm = cdm,
    conceptSet = conceptSet,
    cohortSet = cohortSet,
    cohortCodelist = cohortCodelist,
    tableCohortCodelist = tableCohortCodelist,
    name = name,
    extraCols = NULL,
    exit = exit,
    useSourceFields = useSourceFields,
    subsetIndividuals = subsetIndividuals
  )

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tmpPref))
  cdm[[tableCohortCodelist]] <- NULL

  if (cdm[[name]] |>
      utils::head(1) |>
      dplyr::tally() |>
      dplyr::pull("n") == 0) {
    cli::cli_inform(c("i" = "No cohort entries found, returning empty cohort table."))
    cdm[[name]] <- cdm[[name]] |>
      dplyr::select(
        "cohort_definition_id",
        "subject_id",
        "cohort_start_date",
        "cohort_end_date"
      ) |>
      omopgenerics::newCohortTable(
        cohortSetRef = cohortSet,
        cohortAttritionRef = NULL,
        cohortCodelistRef = cohortCodelist,
        .softValidation = TRUE
      )

    return(cdm[[name]])
  }

  cli::cli_inform(c("i" = "Creating cohort attributes."))
  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet,
      cohortAttritionRef = NULL,
      cohortCodelistRef = cohortCodelist,
      .softValidation = TRUE
    )

  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  cli::cli_inform(c("i" = "Applying cohort requirements."))
  cdm[[name]] <- fulfillCohortReqs(cdm = cdm, name = name, inObservation = inObservation, useIndexes = useIndexes)

  if(overlap == "merge"){
    cli::cli_inform(c("i" = "Merging overlapping records."))
    cdm[[name]] <- cdm[[name]] |>
      joinOverlap(name = name, gap = 0)  |>
      omopgenerics::recordCohortAttrition(reason = "Merge overlapping records")
  }

  if(overlap == "extend"){
    cli::cli_inform(c("i" = "Adding overlapping records."))
    cdm[[name]] <- cdm[[name]] |>
      extendOverlap(name = name)  |>
      omopgenerics::recordCohortAttrition(reason = "Add overlapping records")

    # adding days might mean we no longer satisfy cohort requirements
    cli::cli_inform(c("i" = "Re-appplying cohort requirements."))
    cdm[[name]] <- fulfillCohortReqs(cdm = cdm, name = name, inObservation = TRUE, useIndexes = useIndexes)
  }

  cdm[[name]] <- omopgenerics::newCohortTable(table = cdm[[name]])

  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

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
                                   extraCols,
                                   exit,
                                   useSourceFields,
                                   subsetIndividuals) {

  domains <- sort(cdm[[tableCohortCodelist]] |>
                    dplyr::select("domain_id") |>
                    dplyr::distinct() |>
                    dplyr::pull())

  tableRef <- domainsData |>
    dplyr::filter(.data$domain_id %in% .env$domains)

  cohorts <- list()
  workingTblNames <- paste0(
    omopgenerics::uniqueTableName(),
    "_",
    seq_along(tableRef$domain_id)
  )
  for (k in seq_along(tableRef$domain_id)) {

    table <- tableRef$table[k]
    domain <- tableRef$domain_id[k]
    n <- cdm[[tableCohortCodelist]] |>
      dplyr::filter(.data$domain_id %in% .env$domain) |>
      dplyr::tally() |>
      dplyr::pull()

    if (table %in% names(cdm)) {
      nameK <- paste(workingTblNames[k])
      start <- tableRef$start[k]
      if (exit == "event_start_date") {
        end <- start
      } else {
        end <- tableRef$end[k]
      }
      cli::cli_inform(
        c("i" = "Subsetting table {.strong {table}} using {n} concept{?s} with domain: {.strong {domain}}.")
      )

      ## Get standard
      concept <- tableRef$concept[k]
      tempCohort <- getDomainCohort(
        cdm, table, concept, start, end, extraCols, tableCohortCodelist,
        domain, nameK, subsetIndividuals
      )
      ## Get source
      if (isTRUE(useSourceFields)) {
        concept <- tableRef$source[k]
        tempCohort <- tempCohort |>
          dplyr::union_all(
            getDomainCohort(
              cdm, table, concept, start, end, extraCols,
              tableCohortCodelist, domain, nameK, subsetIndividuals, TRUE
            )
          ) |>
          dplyr::compute(name = nameK, temporary = FALSE,
                         logPrefix = "CohortConstructor_conceptCohort_domainCohort_")
      }

      if (tempCohort |>
          utils::head(1) |>
          dplyr::tally() |>
          dplyr::pull("n") > 0) {
        cohorts[[k]] <- tempCohort
      }
    } else {
      cli::cli_warn(
        c("x" = "Domain {.strong {domain}} ({n} concept{?s}) excluded because table {table} is not present in the cdm.")
      )
    }
  }

  cohorts <- cohorts |>
    purrr::discard(is.null)

  if (length(cohorts) == 0) {
    omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(workingTblNames))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  cli::cli_inform(c("i" = "Combining tables."))
  cohort <- Reduce(dplyr::union_all, cohorts) |>
    dplyr::select(dplyr::any_of(c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date",
      extraCols
    ))) |>
    dplyr::mutate(cohort_end_date = dplyr::coalesce(.data$cohort_end_date, .data$cohort_start_date)) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_conceptCohort_reduce_")

  omopgenerics::dropSourceTable(cdm, name = dplyr::starts_with(workingTblNames))

  return(cohort)
}

fulfillCohortReqs <- function(cdm, name, inObservation, type = "start_end", useIndexes) {
  # 1) inObservation == TRUE and start is out of observation, drop cohort entry.
  #    inObservation == FALSE and start is out of observation move to observation start
  # 2) inObservation == TRUE and end is after observation end, set cohort end as observation end,
  #    inObservation == FALSE and end is after observation end set cohort end as observation start

  # start by inner join with observation to use indexes
  cdm[[name]] <- cdm[[name]] |>
    dplyr::left_join(
      cdm$observation_period |>
        dplyr::select(
          "person_id",
          "observation_period_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = c("subject_id" = "person_id")
    ) |>
    dplyr::compute(temporary = FALSE, name = name,
                   logPrefix = "CohortConstructor_fulfillCohortReqs_observationJoin_")

  if (!inObservation) {
    cdm[[name]] <- cdm[[name]] %>%
      dplyr::mutate(
        in_observation_start = .data$observation_period_start_date <= .data$cohort_start_date & .data$observation_period_end_date >= .data$cohort_start_date,
        in_observation_end = .data$observation_period_start_date <= .data$cohort_end_date & .data$observation_period_end_date >= .data$cohort_end_date,
        days_start_obs = !!CDMConnector::datediff("cohort_start_date", "observation_period_start_date"),
        days_start_obs = dplyr::if_else(.data$days_start_obs < 0, NA, .data$days_start_obs)
      ) |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date, .data$cohort_end_date) |>
      # which records to trim
      dplyr::mutate(
        trim_record =  all(!.data$in_observation_start) & min(.data$days_start_obs) == .data$days_start_obs & !is.na(.data$days_start_obs)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        cohort_start_date = dplyr::if_else(
          .data$trim_record == TRUE,
          .data$observation_period_start_date,
          .data$cohort_start_date
        ),
        cohort_end_date = dplyr::if_else(
          .data$trim_record == TRUE & !.data$in_observation_end,
          dplyr::if_else(
            .data$cohort_end_date > .data$observation_period_end_date,
            .data$observation_period_end_date,
            .data$observation_period_start_date
          ),
          .data$cohort_end_date
        )
      ) |>
      dplyr::select(!dplyr::any_of(c("in_observation_start", "in_observation_end", "days_start_obs", "trim_record"))) |>
      dplyr::compute(
        temporary = FALSE, name = name,
        logPrefix = "CohortConstructor_fulfillCohortReqs_trimRecords_"
      )
  }

  cdm[[name]] <- cdm[[name]] |>
    dplyr::filter(
      .data$cohort_start_date >= .data$observation_period_start_date,
      .data$cohort_start_date <= .data$observation_period_end_date
    ) |>
    dplyr::mutate(
      cohort_end_date = dplyr::if_else(
        .data$observation_period_end_date >= .data$cohort_end_date,
        .data$cohort_end_date,
        .data$observation_period_end_date
      )
    ) |>
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::compute(temporary = FALSE, name = name,
                   logPrefix = "CohortConstructor_fulfillCohortReqs_inObservation_") |>
    omopgenerics::recordCohortAttrition(reason = "Record in observation")

  if (type == "start_end") {
    cdm[[name]] <- cdm[[name]] |>
      dplyr::filter(
        !is.na(.data$cohort_start_date),
        .data$cohort_start_date <= .data$cohort_end_date
      ) |>
      dplyr::compute(temporary = FALSE, name = name,
                     logPrefix = "CohortConstructor_fulfillCohortReqs_filterStartEnd_") |>
      omopgenerics::recordCohortAttrition(reason = "Record start <= record end")
  } else if (type == "start") {
    cdm[[name]] <- cdm[[name]] |>
      dplyr::filter(
        !is.na(.data$cohort_start_date)
      ) |>
      dplyr::compute(temporary = FALSE, name = name,
                     logPrefix = "CohortConstructor_fulfillCohortReqs_filterStart_") |>
      omopgenerics::recordCohortAttrition(reason = "Not missing record date")
  }

  # remove missing sex or date of birth
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id")
    )
  }
  cdm[[name]] <- cdm[[name]] |>
    dplyr::inner_join(
      cdm$person |>
        dplyr::select("subject_id" = "person_id", "gender_concept_id", "year_of_birth"),
      by = "subject_id"
    ) |>
    dplyr::filter(!is.na(.data$gender_concept_id)) |>
    dplyr::compute(temporary = FALSE, name = name,
                   logPrefix = "CohortConstructor_fulfillCohortReqs_sex_") |>
    omopgenerics::recordCohortAttrition(reason = "Non-missing sex") |>
    dplyr::filter(!is.na(.data$year_of_birth)) |>
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::compute(temporary = FALSE, name = name,
                   logPrefix = "CohortConstructor_fulfillCohortReqs_birth_year_") |>
    omopgenerics::recordCohortAttrition(reason = "Non-missing year of birth")
}


conceptSetToCohortSet <- function(conceptSet, cdm) {
  cohSet <- dplyr::tibble("cohort_name" = names(conceptSet)) |>
    dplyr::mutate(
      "cohort_definition_id" = as.integer(dplyr::row_number()),
      "cdm_version" = attr(cdm, "cdm_version"),
      "vocabulary_version" = CodelistGenerator::getVocabVersion(cdm)
    )
  if (length(conceptSet) == 0) {
    cohSet <- cohSet |>
      dplyr::mutate("cohort_name" = character())
  }
  return(cohSet)
}

conceptSetToCohortCodelist <- function(conceptSet) {
  cohortSet <- dplyr::tibble("cohort_name" = names(conceptSet)) |>
    dplyr::mutate("cohort_definition_id" = as.integer(dplyr::row_number()))

  lapply(conceptSet, dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "cohort_name") |>
    dplyr::inner_join(cohortSet, by = "cohort_name") |>
    dplyr::mutate("type" = "index event", "value" = as.integer(.data$value)) |>
    dplyr::select("cohort_definition_id",
                  "codelist_name" = "cohort_name",
                  "concept_id" = "value",
                  "type"
    )
}

# upload codes to cdm and add domain
uploadCohortCodelistToCdm <- function(cdm, cohortCodelist, tableCohortCodelist, table) {
  if (is.null(table)) {
    cdm <- omopgenerics::insertTable(
      cdm = cdm,
      name = tableCohortCodelist,
      table = cohortCodelist |>
        dplyr::select("cohort_definition_id", "concept_id")
    )

    cdm[[tableCohortCodelist]] <- cdm[[tableCohortCodelist]] |>
      dplyr::left_join(cdm[["concept"]] |>
                         dplyr::select("concept_id", "domain_id"), by = "concept_id") |>
      dplyr::mutate(
        "concept_id" = as.integer(.data$concept_id),
        "domain_id" = tolower(.data$domain_id)
      ) |>
      dplyr::compute(
        name = tableCohortCodelist,
        temporary = FALSE,
        overwrite = TRUE,
        logPrefix = "CohortConstructor_uploadCohortCodelist"
      )
  } else {
    domains <- domainsData |>
      dplyr::filter(.data$table %in% .env$table) |>
      dplyr::select("domain_id")
    cohortCodelist <- cohortCodelist |>
      dplyr::select("cohort_definition_id", "concept_id") |>
      dplyr::cross_join(domains) |>
      dplyr::mutate(
        "concept_id" = as.integer(.data$concept_id),
        "domain_id" = tolower(.data$domain_id)
      )

    cdm <- omopgenerics::insertTable(
      cdm = cdm,
      name = tableCohortCodelist,
      table = cohortCodelist
    )
  }

  cdm
}

reportConceptsFromUnsopportedDomains <- function(cdm,
                                                 tableCohortCodelist,
                                                 supportedDomains) {
  ud <- cdm[[tableCohortCodelist]] |>
    dplyr::group_by(.data$domain_id) |>
    dplyr::tally() |>
    dplyr::collect() |>
    dplyr::filter(!.data$domain_id %in% .env$supportedDomains)
  for (k in seq_len(nrow(ud))) {
    cli::cli_inform(
      c("x" = "Domain {.strong {ud$domain_id[k]}} ({ud$n[k]} concept{?s}) excluded because it is not supported.")
    )
  }
}

getDomainCohort <- function(cdm,
                            table,
                            concept,
                            start,
                            end,
                            extraCols,
                            tableCohortCodelist,
                            domain,
                            name,
                            subsetIndividuals,
                            source = FALSE) {
  if (source) {
    name = paste0(name, "_source")
  }
  tempCohort <- cdm[[table]] |>
    dplyr::select(
      "subject_id" = "person_id",
      "concept_id" = dplyr::all_of(.env$concept),
      "cohort_start_date" = dplyr::all_of(.env$start),
      "cohort_end_date" = dplyr::all_of(.env$end),
      dplyr::all_of(extraCols)
    )
  if (!is.null(subsetIndividuals)) {
    tempCohort <- tempCohort |>
      dplyr::inner_join(subsetIndividuals,  by = "subject_id") |>
      dplyr::compute(temporary = FALSE, name = name,
                     logPrefix = "CohortConstructor_subsetIndividuals_")
  }
  tempCohort <- tempCohort  |>
    dplyr::inner_join(
      cdm[[tableCohortCodelist]] |>
        dplyr::filter(.data$domain_id %in% .env$domain) |>
        dplyr::select("concept_id", "cohort_definition_id"),
      by = "concept_id"
    ) |>
    dplyr::compute(temporary = FALSE, name = name,
                   logPrefix = "CohortConstructor_tempCohort_")
}

extendOverlap  <- function(cohort,
                           name){


  cdm <- omopgenerics::cdmReference(cohort)

  # Because once we add to a record this may cause a new overlap
  # will do a while loop until all overlaps are resolved
  while(hasOverlap(cohort)){
    cli::cli_inform("Recursively adding overlapping records")
    workingTblNames <- paste0(omopgenerics::uniqueTableName(), "_", c(1:4))
    cohort <- cohort %>%
      dplyr::mutate(record_id = dplyr::row_number()) |>
      dplyr::compute(temporary = FALSE,
                     name = workingTblNames[1],
                     logPrefix = "CohortConstructor_extendOverlap_hasOverlap_")

    # keep overlapping records
    cohort_overlap <- cohort %>%
      dplyr::inner_join(cohort,
                        by = c("cohort_definition_id", "subject_id"),
                        suffix = c("", "_overlap")) |>
      dplyr::filter(
        .data$record_id != .data$record_id_overlap,
        .data$cohort_start_date <= .data$cohort_end_date_overlap &
          .data$cohort_end_date >= .data$cohort_start_date_overlap
      )  |>
      dplyr::select("cohort_definition_id", "subject_id",
                    "cohort_start_date", "cohort_end_date",
                    "record_id") |>
      dplyr::distinct() |>
      dplyr::compute(temporary = FALSE,
                     name = workingTblNames[2],
                     logPrefix = "CohortConstructor_extendOverlap_keepOverlap_")

    cohort_no_overlap <- cohort |>
      dplyr::anti_join(cohort_overlap |>
                         dplyr::select("record_id"),
                       by = "record_id") |>
      dplyr::select(!"record_id")  |>
      dplyr::compute(temporary = FALSE,
                     name = workingTblNames[3],
                     logPrefix = "CohortConstructor_extendOverlap_noOverlap_")

    cohort_overlap <- cohort_overlap %>%
      dplyr::mutate(days = !!CDMConnector::datediff("cohort_start_date",
                                                    "cohort_end_date")) |>
      dplyr::group_by(dplyr::pick("cohort_definition_id",
                                  "subject_id")) |>
      dplyr::summarise(cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE),
                       days  = as.integer(sum(.data$days, na.rm = TRUE)))  %>%
      dplyr:: ungroup() %>%
      dplyr::mutate(cohort_end_date = as.Date(
        !!CDMConnector::dateadd(
          date = "cohort_start_date",
          number = "days",
          interval = "day"
        ))) |>
      dplyr::select(!"days")  |>
      dplyr::compute(temporary = FALSE,
                     name = workingTblNames[4],
                     logPrefix = "CohortConstructor_extendOverlap_update_")

    cohort <- dplyr::union_all(cohort_overlap, cohort_no_overlap) |>
      dplyr::compute(name = name, temporary = FALSE)

    CDMConnector::dropTable(cdm = cdm, name = workingTblNames)
  }

  cohort
}

hasOverlap <- function(cohort){
  overlaps <- cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(
      "next_cohort_start_date" = dplyr::lead(.data$cohort_start_date)
    ) |>
    dplyr::filter(.data$cohort_end_date >= .data$next_cohort_start_date) |>
    dplyr::ungroup() |>
    dplyr::tally() |>
    dplyr::collect()

  if (overlaps$n > 0) {
    cli::cli_inform(" - {overlaps$n} overlapping record{?s} found")
    return(TRUE)
  } else {
    return(FALSE)
  }

}

