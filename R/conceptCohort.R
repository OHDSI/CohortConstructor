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
#' * Overlapping records are collapsed into a single cohort entry.
#' * If a record starts outside of an observation period it will be
#'  silently ignored.
#' * If a record ends outside of an observation period it will be
#'  trimmed so as to end at the preceding observation period end date.
#'
#' @inheritParams cdmDoc
#' @inheritParams conceptSetDoc
#' @inheritParams nameDoc
#' @param exit How the cohort end date is defined. Can be either
#' "event_end_date" or "event_start_date".
#' @param useSourceFields If TRUE, the source concept_id fields will also be
#' used when identifying relevant clinical records. If FALSE, only the standard
#' concept_id fields will be used.
#'
#' @export
#'
#' @return A cohort table
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(conditionOccurrence = TRUE)
#'
#' cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort")
#'
#' cohort |> attrition()
#' }
conceptCohort <- function(cdm,
                          conceptSet,
                          name,
                          exit = "event_end_date",
                          useSourceFields = FALSE) {
  # initial input validation
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(cdm)
  conceptSet <- omopgenerics::validateConceptSetArgument(conceptSet, cdm)
  omopgenerics::assertChoice(exit, c("event_start_date", "event_end_date"))
  omopgenerics::assertLogical(useSourceFields, length = 1)

  useIndexes <- getOption("CohortConstructor.use_indexes")

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
  tableCohortCodelist <- omopgenerics::uniqueTableName()
  cdm <- uploadCohortCodelistToCdm(
    cdm = cdm,
    cohortCodelist = cohortCodelist,
    tableCohortCodelist = tableCohortCodelist
  )

  if (!isFALSE(useIndexes)) {
    addIndex(
      cdm = cdm,
      name = tableCohortCodelist,
      cols = "concept_id"
    )
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
    useSourceFields = useSourceFields
  )

  omopgenerics::dropTable(cdm = cdm, name = tableCohortCodelist)
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
      cdm = cdm,
      name = name,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  cli::cli_inform(c("i" = "Applying cohort requirements."))
  cdm[[name]] <- fulfillCohortReqs(cdm = cdm, name = name)
  cdm[[name]] <- omopgenerics::newCohortTable(table = cdm[[name]],
                                              cohortAttritionRef = NULL,
                                .softValidation = TRUE)

  cli::cli_inform(c("i" = "Collapsing records."))
  cdm[[name]] <- cdm[[name]] |>
    joinOverlap(name = name, gap = 0)
  cdm[[name]] <- omopgenerics::newCohortTable(table = cdm[[name]],
                                              cohortAttritionRef = NULL,
                                .softValidation = TRUE)

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
                                   useSourceFields) {

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
        domain, nameK
      )
      ## Get source
      if (isTRUE(useSourceFields)) {
        concept <- tableRef$source[k]
        tempCohort <- tempCohort |>
          dplyr::union_all(
            getDomainCohort(
              cdm, table, concept, start, end, extraCols,
              tableCohortCodelist, domain, nameK, TRUE
            )
          ) |>
          dplyr::compute(name = nameK, temporary = FALSE)
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

  cohorts <- cohorts %>%
    purrr::discard(is.null)

  if (length(cohorts) == 0) {
    omopgenerics::dropTable(cdm, name = dplyr::starts_with(workingTblNames))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  cli::cli_inform(c("i" = "Combining tables."))
  cohort <- Reduce(dplyr::union_all, cohorts) |>
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::mutate(cohort_end_date = dplyr::coalesce(.data$cohort_end_date, .data$cohort_start_date)) |>
    dplyr::filter(
      !is.na(.data$cohort_start_date),
      .data$cohort_start_date <= .data$cohort_end_date
    ) |>
    dplyr::compute(name = name, temporary = FALSE)

  omopgenerics::dropTable(cdm, name = dplyr::starts_with(workingTblNames))

  return(cohort)
}

fulfillCohortReqs <- function(cdm, name) {
  # 1) if start is out of observation, drop cohort entry
  # 2) if end is after observation end, set cohort end as observation end
  cdm[[name]] |>
    dplyr::left_join(
      cdm$observation_period |>
        dplyr::select(
          "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = c("subject_id" = "person_id")
    ) |>
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
    dplyr::compute(temporary = FALSE, name = name)
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
uploadCohortCodelistToCdm <- function(cdm, cohortCodelist, tableCohortCodelist) {
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
      overwrite = TRUE
    )

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

addIndex <- function(cdm, name, cols) {
  tblSource <- attr(cdm[[name]], "tbl_source")
  if(is.null(tblSource)){
    return(invisible(NULL))
  }
  dbType <- attr(tblSource, "source_type")
  if(is.null(dbType)){
    return(invisible(NULL))
  }

  if (dbType == "postgresql") {
    cli::cli_inform("Adding indexes to table")
    con <- attr(cdm, "dbcon")
    schema <- attr(cdm, "write_schema")
    if(length(schema) > 1){
      prefix <- attr(cdm, "write_schema")["prefix"]
      schema <- attr(cdm, "write_schema")["schema"]
    } else {
      prefix <- NULL
    }

    cols <- paste0(cols, collapse = ",")

    query <- paste0(
      "CREATE INDEX ON ",
      paste0(schema, ".", prefix, name),
      " (",
      cols,
      ");"
    )
    suppressMessages(DBI::dbExecute(con, query))
  }

    return(invisible(NULL))

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
    ) |>
    dplyr::inner_join(
      cdm[[tableCohortCodelist]] |>
        dplyr::filter(.data$domain_id %in% .env$domain) |>
        dplyr::select("concept_id", "cohort_definition_id"),
      by = "concept_id"
    ) |>
    dplyr::compute(temporary = FALSE, name = name)
}
