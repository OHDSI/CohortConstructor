#' Require cohort subjects to have (or not have) events of a concept list
#'
#' @description
#' `requireConceptIntersect()` filters a cohort table based on a requirement
#' that an individual is seen (or not seen) to have events related to a concept
#' list in some time window around an index date.
#'
#' @inheritParams requireIntersectDoc
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @inheritParams windowDoc
#' @inheritParams nameDoc
#' @inheritParams conceptSetDoc
#' @inheritParams atFirstDoc
#'
#' @return Cohort table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort2 <-  requireConceptIntersect(
#'   cohort = cdm$cohort1,
#'   conceptSet = list(a = 194152),
#'   window = c(-Inf, 0),
#'   name = "cohort2")
#' }
requireConceptIntersect <- function(cohort,
                                    conceptSet,
                                    window,
                                    intersections = c(1, Inf),
                                    cohortId = NULL,
                                    indexDate = "cohort_start_date",
                                    targetStartDate = "event_start_date",
                                    targetEndDate = "event_end_date",
                                    inObservation = TRUE,
                                    censorDate = NULL,
                                    atFirst = FALSE,
                                    name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  window <- omopgenerics::validateWindowArgument(window)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  intersections <- validateIntersections(intersections)
  conceptSet <- omopgenerics::validateConceptSetArgument(conceptSet, cdm)
  omopgenerics::assertLogical(atFirst, length = 1)

  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireConceptIntersect_entry_1_")
    return(cdm[[name]])
  }

  if (length(conceptSet) == 0) {
    cli::cli_inform("Returning entry cohort as `conceptSet` is empty.")
    # return entry cohort as cohortId is used to modify not subset
    cdm[[name]] <- cohort |> dplyr::compute(name = name, temporary = FALSE,
                                            logPrefix = "CohortConstructor_requireConceptIntersect_entry_2_")
    return(cdm[[name]])
  }

  lower_limit <- as.integer(intersections[[1]])
  upper_limit <- intersections[[2]]
  upper_limit[is.infinite(upper_limit)] <- 999999L
  upper_limit <- as.integer(upper_limit)

  window_start <- window[[1]][1]
  window_end <- window[[1]][2]

  if (length(conceptSet) > 1) {
    cli::cli_abort("We currently suport 1 concept set.")
  }

  if (length(conceptSet) == 0) {
    cli::cli_inform(c("i" = "Empty codelist provided, returning input cohort"))
    return(
      cohort |>
        dplyr::compute(
          name = name, temporary = FALSE,
          logPrefix = "CohortConstructor_requireConceptIntersect_emptyConcept_"
        )
    )
  }

  # temp tables
  tablePrefix <- omopgenerics::tmpPrefix()
  tmpNewCohort <- omopgenerics::uniqueTableName(tablePrefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  intersectCol <- uniqueColumnName(newCohort)
  newCohort <- newCohort |>
    PatientProfiles::addConceptIntersectCount(
      conceptSet = conceptSet,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      censorDate = censorDate,
      inObservation = inObservation,
      nameStyle = intersectCol,
      name = tmpNewCohort
    )

  newCohort <- applyRequirement(
    newCohort, atFirst, tmpNewCohort, intersectCol, lower_limit, upper_limit, cdm
  )

  # attrition reason
  if (all(intersections == 0)) {
    reason <- glue::glue(
      "Not in concept {names(conceptSet)} between {window_start} & ",
      "{window_end} days relative to {indexDate}"
    )
  } else if (intersections[[1]] != intersections[[2]]) {
    reason <- glue::glue(
      "Concept {names(conceptSet)} between {window_start} & ",
      "{window_end} days relative to {indexDate} between ",
      "{intersections[[1]]} and {intersections[[2]]}"
    )
  } else {
    reason <- glue::glue(
      "Concept {names(conceptSet)} between {window_start} & ",
      "{window_end} days relative to {indexDate} ",
      "{intersections[[1]]} times"
    )
  }
  reason <- completeAttritionReason(reason, censorDate, atFirst)

  # codelist
  newCodelist <- getIntersectionCodelist(
    cohort, cohortId, conceptSetToCohortCodelist(conceptSet)
  )

  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(
        cdm[[tmpUnchanged]] |>
          dplyr::select(dplyr::all_of(colnames(newCohort)))
      ) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireConceptIntersect_union_")
  }

  # cohort
  newCohort <- newCohort |>
    dplyr::compute(
      name = name, temporary = FALSE,
      logPrefix = "CohortConstructor_requireConceptIntersect_join_"
    ) |>
    omopgenerics::newCohortTable(
      .softValidation = TRUE, cohortCodelistRef = newCodelist
    ) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}

getIntersectionCodelist <- function(cohort, cohortId, codelist) {
  criteria <- "inclusion criteria"
  intersectCodelist <- lapply(
    as.list(cohortId),
    function(x, tab = codelist) {
      tab |>
        dplyr::mutate(cohort_definition_id = .env$x) |>
        dplyr::select(!dplyr::any_of(c("type", "codelist_type")))
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(type = .env$criteria)
  newCodelist <- attr(cohort, "cohort_codelist") |>
    dplyr::collect() |>
    dplyr::rename(type = dplyr::any_of("codelist_type")) |>
    dplyr::union(intersectCodelist) |>
    dplyr::arrange(.data$cohort_definition_id)
  return(newCodelist)
}

applyRequirement <- function(newCohort, atFirst, tmpNewCohort, intersectCol, lower_limit, upper_limit, cdm) {
  if (atFirst) {
    tmpNewCohortFirst <- paste0(tmpNewCohort, "_1")
    newCohortFirst <- newCohort |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::filter(.data$cohort_start_date == base::min(.data$cohort_start_date)) |>
      dplyr::ungroup() |>
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyRequirement_subset_arrange_") |>
      dplyr::filter(
        .data$rec_id_1234 == 1 & .data[[intersectCol]] >= .env$lower_limit & .data[[intersectCol]] <= .env$upper_limit
      ) |>
      dplyr::select(dplyr::all_of(c("cohort_definition_id", "subject_id"))) |>
      dplyr::compute(name = tmpNewCohortFirst, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyRequirement_subset_first_")
    newCohort <- newCohort |>
      dplyr::inner_join(newCohortFirst, by = c("cohort_definition_id", "subject_id")) |>
      dplyr::select(!dplyr::all_of(intersectCol)) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyRequirement_requirement_first_")
    omopgenerics::dropSourceTable(cdm = cdm, name = tmpNewCohortFirst)
  } else {
    newCohort <- newCohort |>
      dplyr::filter(
        .data[[intersectCol]] >= .env$lower_limit & .data[[intersectCol]] <= .env$upper_limit
      ) |>
      dplyr::select(!dplyr::all_of(intersectCol)) |>
      dplyr::compute(name = tmpNewCohort, temporary = FALSE,
                     logPrefix = "CohortConstructor_applyRequirement_subset_")
  }
  return(newCohort)
}

completeAttritionReason <- function(reason, censorDate, atFirst) {
  if (!is.null(censorDate)) {
    reason <- glue::glue("{reason}, censoring at {censorDate}")
  }
  if (atFirst) {
    reason <- glue::glue("{reason}. Requirement applied to the first entry")
  }
  return(reason)
}
