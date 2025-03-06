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
#'
#' @return Cohort table with only those  with the events in the concept list
#' kept (or those without the event if negate = TRUE)
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor(conditionOccurrence = TRUE)
#' cdm$cohort2 <-  requireConceptIntersect(
#'   cohort = cdm$cohort1,
#'   conceptSet = list(a = 194152),
#'   window = c(-Inf, 0),
#'   name = "cohort2")
#'   }
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
                                    name = tableName(cohort)) {
  # checks
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cohort <- omopgenerics::validateCohortArgument(cohort)
  validateCohortColumn(indexDate, cohort, class = "Date")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  window <- omopgenerics::validateWindowArgument(window)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  intersections <- validateIntersections(intersections)
  conceptSet <- omopgenerics::validateConceptSetArgument(conceptSet, cdm)

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

  cols <- unique(
    c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date",
      indexDate
    )
  )

  window_start <- window[[1]][1]
  window_end <- window[[1]][2]

  if (length(conceptSet) > 1) {
    cli::cli_abort("We currently suport 1 concept set.")
  }

  if (length(conceptSet) == 0) {
    cli::cli_inform(c("i" = "Empty codelist provided, returning input cohort"))
  } else {
    subsetName <- omopgenerics::uniqueTableName()
    subsetCohort <- cohort |>
      dplyr::select(dplyr::all_of(.env$cols)) |>
      PatientProfiles::addConceptIntersectCount(
        conceptSet = conceptSet,
        indexDate = indexDate,
        targetStartDate = targetStartDate,
        targetEndDate = targetEndDate,
        window = window,
        censorDate = censorDate,
        inObservation = inObservation,
        nameStyle = "intersect_concept",
        name = subsetName
      )

    subsetCohort <- subsetCohort |>
      dplyr::mutate(lower_limit = .env$lower_limit,
                    upper_limit = .env$upper_limit) |>
      dplyr::filter((
        .data$intersect_concept >= .data$lower_limit &
          .data$intersect_concept <= .data$upper_limit
      ) |
        (!.data$cohort_definition_id %in% .env$cohortId)
      ) |>
      dplyr::select(dplyr::all_of(cols)) |>
      dplyr::compute(name = subsetName, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireConceptIntersect_subset_")

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
    if (!is.null(censorDate)) {
      reason <- glue::glue("{reason}, censoring at {censorDate}")
    }

    #codelist
    newCodelist <- getIntersectionCodelist(
      cohort, cohortId, conceptSetToCohortCodelist(conceptSet)
    )

    # cohort
    cohort <- cohort |>
      dplyr::inner_join(subsetCohort, by = c(cols)) |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_requireConceptIntersect_join_") |>
      omopgenerics::newCohortTable(
        .softValidation = TRUE, cohortCodelistRef = newCodelist
      ) |>
      omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

    omopgenerics::dropTable(cdm = cdm, name = subsetName)
  }

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cohort)
}

getIntersectionCodelist <- function(cohort, cohortId, codelist) {
  criteria <- "inclusion criteria"
  intersectCodelist <- lapply(
    as.list(cohortId),
    function(x, tab = codelist) {
      tab |> dplyr::mutate(cohort_definition_id = .env$x)
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(type = .env$criteria)
  newCodelist <- attr(cohort, "cohort_codelist") |>
    dplyr::collect() |>
    dplyr::union(intersectCodelist) |>
    dplyr::arrange(.data$cohort_definition_id)
  return(newCodelist)
}
