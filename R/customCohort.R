#' Create cohorts based on a arbitrary fields in the OMOP CDM
#'
#' @description
#' `customCohort()` creates a cohort table from patient records
#' from the clinical tables in the OMOP CDM using
#'
#' @inheritParams cdmDoc
#' @param table Name of OMOP tables to search for records of the concepts
#' provided. If NULL, each concept will be search at the assigned domain in
#' the concept table.
#' @param field Field within which to search for the specified values
#' @param values description
#' @inheritParams nameDoc
#' @inheritParams baseCohortDoc
#' @param exit How the cohort end date is defined. Can be either
#' "event_end_date" or "event_start_date".
#' @param overlap How to deal with overlapping records. In all
#' cases cohort start will be set as the earliest start date. If
#' "merge", cohort end will be the latest end date. If "extend",
#' cohort end date will be set by adding together the total days
#' from each of the overlapping records.
#'
#' @export
#'
#' @return A cohort table
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 444074), name = "cohort")
#'
#' cdm$cohort |>
#'   attrition()
#'
#' # Create a cohort based on a concept set. The cohort exit is set to the event start date.
#' # If two records overlap, the cohort end date is set as the sum of the duration of
#' # all overlapping records. Only individuals included in the existing `cohort` will be considered.
#'
#' conceptSet <- list(
#'   "nitrogen" = c(35604434, 35604439),
#'   "potassium" = c(40741270, 42899580, 44081436)
#' )
#'
#' cdm$study_cohort <- conceptCohort(cdm = cdm,
#'                                   conceptSet = conceptSet,
#'                                   name = "study_cohort",
#'                                   exit = "event_start_date",
#'                                   overlap = "extend",
#'                                   subsetCohort = "cohort")
#'
#' cdm$study_cohort |>
#'   attrition()
#' }
customCohort <- function(cdm,
                         table,
                         field,
                         values,
                         name,
                         exit = "event_end_date",
                         overlap = "merge",
                         useRecordsBeforeObservation = FALSE,
                         subsetCohort = NULL,
                         subsetCohortId = NULL){

  # initial input validation
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertChoice(exit, c("event_start_date", "event_end_date"))
  omopgenerics::assertChoice(overlap, c("merge", "extend"), length = 1)
  omopgenerics::assertLogical(useRecordsBeforeObservation, length = 1)
  omopgenerics::assertCharacter(subsetCohort, length = 1, null = TRUE)
  if (!is.null(subsetCohort)) {
    subsetCohort <- omopgenerics::validateCohortArgument(cdm[[subsetCohort]])
    subsetCohortId <- omopgenerics::validateCohortIdArgument({{subsetCohortId}}, subsetCohort, validation = "warning")
  }
  table <- validateTable(table)
  omopgenerics::assertCharacter(field, length = 1)
  if(!field %in% colnames(cdm[[table]])){
    cli::cli_abort("{field} not found in table {table}")
  }
  omopgenerics::assertList(values, named = TRUE)

  useIndexes <- getOption("CohortConstructor.use_indexes")

  # prefix for names
  tmpPref <- omopgenerics::tmpPrefix()
  on.exit(omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tmpPref)))

  # empty concept set
  cohortSet <- conceptSetToCohortSet(values, cdm)
  if (length(values) == 0) {
    cli::cli_inform(c("i" = "Empty values provided, returning empty cohort"))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    cdm[[name]] <- cdm[[name]] |>
      omopgenerics::newCohortTable(cohortSetRef = cohortSet)
    return(cdm[[name]])
  }

  # codelist attribute
  cohortCodelist <- valuesToCohortCodelist(values)
  tableCohortCodelist <- omopgenerics::uniqueTableName(prefix = tmpPref)
  cdm <- uploadValuesCohortCodelistToCdm(
    cdm = cdm,
    cohortCodelist = cohortCodelist,
    tableCohortCodelist = tableCohortCodelist,
    table = table
  )

  # subsetCohort
  if (!is.null(subsetCohort)) {
    subsetName <- omopgenerics::uniqueTableName(prefix = tmpPref)
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
        cols = "subject_id",
        unique = TRUE
      )
    }
  } else {
    subsetIndividuals <- NULL
  }

cdm[[name]] <- unerafiedValuesCohort(
    cdm = cdm,
    cohortSet = cohortSet,
    cohortCodelist = cohortCodelist,
    tableCohortCodelist = tableCohortCodelist,
    field = field,
    name = name,
    exit = exit,
    subsetIndividuals = subsetIndividuals,
    tablePrefix = tmpPref
  )


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
      cohortCodelistRef = NULL,
      .softValidation = TRUE
    )

  return(cdm[[name]])
}

cli::cli_inform(c("i" = "Creating cohort attributes."))
cdm[[name]] <- cdm[[name]] |>
  omopgenerics::newCohortTable(
    cohortSetRef = cohortSet,
    cohortAttritionRef = NULL,
    cohortCodelistRef = NULL,
    .softValidation = TRUE
  )

if (!isFALSE(useIndexes)) {
  addIndex(
    cohort = cdm[[name]],
    cols = c("subject_id", "cohort_start_date")
  )
}

cli::cli_inform(c("i" = "Applying cohort requirements."))
cdm[[name]] <- fulfillCohortReqs(cdm = cdm,
                                 name = name,
                                 useRecordsBeforeObservation = useRecordsBeforeObservation,
                                 useIndexes = useIndexes)

if(overlap == "merge"){
  if (exit == "event_end_date") {
    cli::cli_inform(c("i" = "Merging overlapping records."))
    reason <- "Merge overlapping records"
    cdm[[name]] <- cdm[[name]] |>
      joinOverlap(name = name, gap = 0)  |>
      omopgenerics::recordCohortAttrition(reason = reason)
  } else if (exit == "event_start_date") {
    cli::cli_inform(c("i" = "Dropping duplicated records."))
    reason <- "Drop duplicate records"
    cdm[[name]] <- cdm[[name]] |>
      dplyr::distinct(
        .data$cohort_definition_id, .data$subject_id, .data$cohort_start_date,
        .data$cohort_end_date
      ) |>
      dplyr::compute(name = name) |>
      omopgenerics::recordCohortAttrition(reason = reason)
  }
}

if(overlap == "extend"){
  cli::cli_inform(c("i" = "Adding overlapping records."))
  cdm[[name]] <- cdm[[name]] |>
    extendOverlap(name = name)  |>
    omopgenerics::recordCohortAttrition(reason = "Add overlapping records")

  # adding days might mean we no longer satisfy cohort requirements
  cli::cli_inform(c("i" = "Re-appplying cohort requirements."))
  cdm[[name]] <- fulfillCohortReqs(cdm = cdm,
                                   name = name,
                                   useRecordsBeforeObservation = useRecordsBeforeObservation,
                                   useIndexes = useIndexes)
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


valuesToCohortCodelist <- function(values) {
  cohortSet <- dplyr::tibble("cohort_name" = names(values)) |>
    dplyr::mutate("cohort_definition_id" = as.integer(dplyr::row_number()))

  lapply(values, dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "cohort_name") |>
    dplyr::inner_join(cohortSet, by = "cohort_name") |>
    dplyr::mutate("value" = .data$value) |>
    dplyr::select("cohort_definition_id",
                  "codelist_name" = "cohort_name",
                  "value"
    )
}
uploadValuesCohortCodelistToCdm <- function(cdm, cohortCodelist, tableCohortCodelist, table) {

    cohortCodelist <- cohortCodelist |>
      dplyr::select("cohort_definition_id", "value") |>
      dplyr::mutate(table = tolower(.env$table))

    cdm <- omopgenerics::insertTable(
      cdm = cdm,
      name = tableCohortCodelist,
      table = cohortCodelist
    )


  cdm
}

unerafiedValuesCohort <- function(cdm,
                                  cohortSet,
                                  cohortCodelist,
                                  tableCohortCodelist,
                                  field,
                                  name,
                                  exit,
                                  subsetIndividuals,
                                  tablePrefix) {

  tables <- sort(cdm[[tableCohortCodelist]] |>
                    dplyr::select("table") |>
                    dplyr::distinct() |>
                    dplyr::pull())

  tableRef <- domainsData |>
    dplyr::filter(.data$table %in% .env$tables)

  cohorts <- list()
  workingTblNames <- paste0(
    tablePrefix,
    omopgenerics::uniqueTableName(),
    "_",
    seq_along(tableRef$domain_id)
  )
  for (k in seq_along(tableRef$domain_id)) {
    table <- tableRef$table[k]
    domain <- tableRef$domain_id[k]
    n <- cdm[[tableCohortCodelist]] |>
      dplyr::filter(.data$table %in% .env$table) |>
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

      valuesCohort <- getValuesCohort(
        cdm, table, field, start, end, tableCohortCodelist,
        domain, nameK, subsetIndividuals, tablePrefix = tablePrefix
      )

      if (valuesCohort |>
          utils::head(1) |>
          dplyr::tally() |>
          dplyr::pull("n") > 0) {
        cohorts[[k]] <- valuesCohort
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
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  cli::cli_inform(c("i" = "Combining tables."))
  cohort <- Reduce(dplyr::union_all, cohorts) |>
    dplyr::select(dplyr::any_of(c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ))) |>
    dplyr::mutate(cohort_end_date = dplyr::coalesce(.data$cohort_end_date, .data$cohort_start_date)) |>
    dplyr::compute(name = name, temporary = FALSE,
                   logPrefix = "CohortConstructor_conceptCohort_reduce_")

  return(cohort)
}

getValuesCohort <- function(cdm,
                            table,
                            field,
                            start,
                            end,
                            tableCohortCodelist,
                            domain,
                            name,
                            subsetIndividuals,
                            tablePrefix) {

  if(!field %in% colnames(cdm[[table]])){
    cli::cli_abort("{field} not found in {table} table")
  }
  if(!start %in% colnames(cdm[[table]])){
    cli::cli_abort("{start} not found in {table} table")
  }
  if(!end %in% colnames(cdm[[table]])){
    cli::cli_abort("{end} not found in {table} table")
  }

  # codelist + cohort filtered to table
  cdm[[paste0(tablePrefix, "temp_codelist_cohort_id")]] <- cdm[[tableCohortCodelist]] |>
    dplyr::filter(.data$table %in% .env$table) |>
    dplyr::select("cohort_definition_id", "value") |>
    dplyr::distinct() |>
    dplyr::compute(temporary = FALSE,
                   name = paste0(tablePrefix, "temp_codelist_cohort_id"),
                   logPrefix = "CohortConstructor_tempCodelistCohortId_")
  # codelist only (will use for main join)
  cdm[[paste0(tablePrefix, "temp_codelist")]] <-cdm[[paste0(tablePrefix, "temp_codelist_cohort_id")]] |>
    dplyr::select("value") |>
    dplyr::distinct() |>
    dplyr::compute(temporary = FALSE,
                   name = paste0(tablePrefix, "temp_codelist"),
                   logPrefix = "CohortConstructor_tempCodelist_")

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[paste0(tablePrefix, "temp_codelist_cohort_id")]],
      cols = c("cohort_definition_id", "value")
    )
    addIndex(
      cohort = cdm[[paste0(tablePrefix, "temp_codelist")]],
      cols = "value",
      unique = TRUE
    )
  }
  if (is.null(subsetIndividuals)) {
    tempCohort <- cdm[[table]] |>
      dplyr::select(
        "subject_id" = "person_id",
        "value" = dplyr::all_of(.env$field),
        "cohort_start_date" = dplyr::all_of(.env$start),
        "cohort_end_date" = dplyr::all_of(.env$end)) |>
      dplyr::inner_join(
        cdm[[paste0(tablePrefix, "temp_codelist")]],
        by = "value"
      ) |>
      dplyr::compute(temporary = FALSE, name = name,
                     logPrefix = "CohortConstructor_tempCohort_")

  } else {
    tempCohort <- cdm[[table]] |>
      dplyr::select(
        "subject_id" = "person_id",
        "value" = dplyr::all_of(.env$field),
        "cohort_start_date" = dplyr::all_of(.env$start),
        "cohort_end_date" = dplyr::all_of(.env$end)) |>
      dplyr::inner_join(subsetIndividuals,  by = "subject_id") |>
      dplyr::compute(temporary = FALSE, name = name,
                     logPrefix = "CohortConstructor_subsetIndividuals_")

    tempCohort <- tempCohort |>
      dplyr::inner_join(
        cdm[[paste0(tablePrefix, "temp_codelist")]],
        by =  "value"
      ) |>
      dplyr::compute(temporary = FALSE, name = name,
                     logPrefix = "CohortConstructor_tempCohort_")
  }
  tempCohort <- tempCohort |>
    dplyr::inner_join(
      cdm[[paste0(tablePrefix, "temp_codelist_cohort_id")]],
      by = "value") |>
    dplyr::select("cohort_definition_id",
                  "subject_id",
                  "cohort_start_date",
                  "cohort_end_date") |>
    dplyr::compute(temporary = FALSE, name = name,
                   logPrefix = "CohortConstructor_tempCohort_")
}
