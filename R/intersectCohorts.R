#' Generate a combination cohort set between the intersection of different
#' cohorts.
#'
#' @description
#' `intersectCohorts()` combines different cohort entries, with those records
#' that overlap combined and kept. Cohort entries are when an individual was in
#' _both_ of the cohorts.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdSubsetDoc
#' @inheritParams gapDoc
#' @inheritParams nameDoc
#' @inheritParams keepOriginalCohortsDoc
#' @param returnNonOverlappingCohorts Whether the generated cohorts are mutually
#' exclusive or not.
#' @inheritParams softValidationDoc
#'
#' @export
#'
#' @return A cohort table.
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' cdm$cohort3 <- intersectCohorts(
#'   cohort = cdm$cohort2,
#'   name = "cohort3",
#' )
#'
#' settings(cdm$cohort3)
#'
#' }
intersectCohorts <- function(cohort,
                             cohortId = NULL,
                             gap = 0,
                             returnNonOverlappingCohorts = FALSE,
                             keepOriginalCohorts = FALSE,
                             name = tableName(cohort),
                             .softValidation = FALSE) {
  # checks
  cohort <- omopgenerics::validateCohortArgument(cohort)
  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(omopgenerics::cdmReference(cohort))
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort, validation = "warning")
  omopgenerics::assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  omopgenerics::assertLogical(returnNonOverlappingCohorts, length = 1)
  omopgenerics::assertLogical(keepOriginalCohorts, length = 1)
  omopgenerics::assertLogical(.softValidation)

  if (length(cohortId) < 2) {
    cli::cli_abort("Settings of cohort table must contain at least two cohorts.")
  }

  tablePrefix <- omopgenerics::tmpPrefix()
  if (keepOriginalCohorts) {
    originalNm <- omopgenerics::uniqueTableName(prefix = tablePrefix)
    originalCohorts <- subsetCohorts(
      cohort = cohort,
      cohortId = cohortId,
      name = originalNm
    )
  }

  tblName <- omopgenerics::uniqueTableName(prefix = tablePrefix)
  newCohort <- copyCohorts(cohort,
                           name = tblName,
                           cohortId = cohortId,
                           .softValidation = .softValidation)
  # get intersections between cohorts
  lowerWindow <- ifelse(gap != 0, -gap, gap)
  newCohort <- newCohort |>
    dplyr::select(-"cohort_definition_id") |>
    splitOverlap(by = "subject_id", name = tblName, tmp = paste0(tblName)) |>
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = omopgenerics::tableName(cohort),
      targetCohortId = cohortId,
      window = c(lowerWindow, gap),
      nameStyle = "{cohort_name}",
      name = tblName
    )

  # create intersect cohort set
  cohortNames <- omopgenerics::settings(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::pull("cohort_name")
  x <- rep(list(c(0, 1)), length(cohortNames))
  names(x) <- cohortNames
  cohSet <- expand.grid(x) |>
    dplyr::as_tibble() |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ . != 0)) |>
    addNames() |>
    dplyr::mutate(cohort_definition_id = as.integer(dplyr::row_number())) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      sum = sum(dplyr::c_across(-dplyr::all_of(
        c("cohort_definition_id", "cohort_name")
      )), na.rm = TRUE),
      gap = gap
    ) |>
    dplyr::ungroup()  |>
    dplyr::left_join(
      settings(cohort) |>
        dplyr::select("id_in" = "cohort_definition_id", "cohort_name"),
      by = "cohort_name"
    )
  # filter to cohorts of interest
  if (returnNonOverlappingCohorts) {
    cohSet <- cohSet |>
      dplyr::filter(.data$sum == 1 | .data$sum == length(.env$cohortId)) |>
      dplyr::mutate(
        cohort_name = dplyr::if_else(.data$sum == 1, paste0("only_in_", .data$cohort_name), .data$cohort_name),
        non_overlapping = dplyr::if_else(.data$sum == 1, TRUE, NA)
      )
  } else {
    cohSet <- cohSet |>
      dplyr::filter(.data$sum == length(.env$cohortId))
  }
  # reset cohort ids
  cohSet <- cohSet |>
    dplyr::arrange(dplyr::desc(.data$sum)) |>
    dplyr::mutate(cohort_definition_id = as.integer(dplyr::row_number()))

  # intersect cohort
  setName <- omopgenerics::uniqueTableName(prefix = tablePrefix)
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = setName,
    table = cohSet
  )
  newCohort <- newCohort |>
    dplyr::inner_join(cdm[[setName]], by = cohortNames) |>
    dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort"))) |>
    dplyr::compute(name = tblName, temporary = FALSE,
                   logPrefix = "CohortConstructor_intersectCohorts_out_")
  if (newCohort |> dplyr::tally() |> dplyr::pull("n") > 0) {
    newCohort <- newCohort |>
      getObservationPeriodId(name = tblName) |>
      joinOverlap(
        name = tblName, gap = gap,
        by = c("observation_period_id", "cohort_definition_id", "subject_id")
      ) |>
      dplyr::select(!"observation_period_id")
  }

  # attributes
  counts <- newCohort |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(
      number_records = dplyr::n() |> as.integer(),
      number_subjects = dplyr::n_distinct(.data$subject_id) |> as.integer()
    ) |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id)
  countsInt <- counts |> dplyr::filter(.data$cohort_definition_id == 1)
  intersectCodelist <- attr(cohort, "cohort_codelist") |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::mutate(cohort_definition_id = 1L)
  intersectAttrition <- dplyr::tibble(
    cohort_definition_id = 1L, number_records = countsInt$number_records[1],
    number_subjects = countsInt$number_subjects[1], reason_id = 1L,
    reason = "Initial qualifying events", excluded_records = 0L,
    excluded_subjects = 0L
  ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("number_"), ~dplyr::if_else(is.na(.x), 0L, .x)))

  if (returnNonOverlappingCohorts) {
    intersectCodelist <- intersectCodelist |>
      dplyr::union_all(
        attr(cohort, "cohort_codelist") |>
          dplyr::rename("id_in" = "cohort_definition_id") |>
          dplyr::inner_join(
            cdm[[setName]] |> dplyr::select("cohort_definition_id", "id_in"),
            by = "id_in"
          ) |>
          dplyr::select(!"id_in")
      )
    intersectAttrition <- intersectAttrition |>
      dplyr::union_all(
        attrition(cohort) |>
          dplyr::rename("id_in" = "cohort_definition_id") |>
          dplyr::inner_join(
            cohSet |> dplyr::select("cohort_definition_id", "id_in"),
            by = "id_in"
          ) |>
          dplyr::select(!"id_in") |>
          addAttritionReason(
            counts = counts |> dplyr::filter(.data$cohort_definition_id != 1),
            reason = "Trim to non overlapping entries"
          )
      )
  }
  cohSet <- cohSet |>
    dplyr::select(dplyr::any_of(c(
      "cohort_definition_id", "cohort_name", "gap", "non_overlapping", cohortNames
    )))

  # intersect cohort
  if (keepOriginalCohorts) {
    newCohort <- omopgenerics::newCohortTable(
      table = newCohort,
      cohortSetRef = cohSet,
      cohortAttritionRef = intersectAttrition,
      cohortCodelistRef = intersectCodelist,
      .softValidation = .softValidation
    )
    cdm <- bind(originalCohorts, newCohort, name = name)
  } else {
    newCohort <- newCohort |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_intersectCohorts_keepOriginalCohorts_")
    cdm[[name]] <- omopgenerics::newCohortTable(
      table = newCohort,
      cohortSetRef = cohSet,
      cohortAttritionRef = intersectAttrition,
      cohortCodelistRef = intersectCodelist,
      .softValidation = .softValidation
    )
  }

  CDMConnector::dropTable(cdm, name = dplyr::starts_with(tablePrefix))

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cdm[[name]])
}

#' To split overlapping periods in non overlapping period.
#'
#' @param x Table in the cdm.
#' @param tmp Temp table stem
#' @param start Column that indicates the start of periods.
#' @param end Column that indicates the end of periods.
#' @param by Variables to group by.
#'
#' @noRd
#'
#' @return Table in the cdm with start, end and by as columns. Periods are not
#' going to overlap between each other.
#'
splitOverlap <- function(x,
                         name,
                         tmp,
                         start = "cohort_start_date",
                         end = "cohort_end_date",
                         by = c("cohort_definition_id", "subject_id")) {
  # initial checks
  checkmate::assertCharacter(start,
                             len = 1,
                             min.chars = 1,
                             any.missing = FALSE)
  checkmate::assertCharacter(end,
                             len = 1,
                             min.chars = 1,
                             any.missing = FALSE)
  checkmate::assertCharacter(by,
                             min.len = 1,
                             min.chars = 1,
                             any.missing = FALSE)
  checkmate::assertClass(x, "tbl")
  checkmate::assertTRUE(all(c(start, end, by) %in% colnames(x)))

  ids <- getIdentifier(x, 3)
  id <- ids[1]
  is <- ids[2]
  ie <- ids[3]

  tmpTable_1 <- paste0(tmp, "_1")
  x_a <- x |>
    dplyr::select(dplyr::all_of(by), !!is := dplyr::all_of(start)) |>
    dplyr::union_all(
      x |>
        dplyr::select(dplyr::all_of(by), !!is := dplyr::all_of(end)) |>
        dplyr::mutate(!!is := as.Date(clock::add_days(.data[[is]], 1L)))
    ) |>
    dplyr::distinct() |>
    dplyr::compute(temporary = FALSE, name = tmpTable_1,
                   logPrefix = "CohortConstructor_intersectCohorts_tmpTable_1_")

  tmpTable_2 <- paste0(tmp, "_2")
  x_a <-  x_a |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dbplyr::window_order(.data[[is]]) |>
    dplyr::mutate(!!id := dplyr::row_number()) |>
    dbplyr::window_order() |>
    dplyr::ungroup() |>
    dplyr::compute(temporary = FALSE, name = tmpTable_2,
                   logPrefix = "CohortConstructor_intersectCohorts_tmpTable_2_")

  tmpTable_3 <- paste0(tmp, "_3")
  x_b <- x |>
    dplyr::select(dplyr::all_of(by), !!ie := dplyr::all_of(end)) |>
    dplyr::union_all(
      x |>
        dplyr::select(dplyr::all_of(by), !!ie := dplyr::all_of(start)) |>
        dplyr::mutate(!!ie := as.Date(clock::add_days(.data[[ie]], -1L)))
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dbplyr::window_order(.data[[ie]]) |>
    dplyr::mutate(!!id := dplyr::row_number() - 1) |>
    dbplyr::window_order() |>
    dplyr::ungroup() |>
    dplyr::compute(temporary = FALSE, name = tmpTable_3,
                   logPrefix = "CohortConstructor_intersectCohorts_tmpTable_3_")

  x <-  x_a |>
    dplyr::inner_join(
      x_b,
      by = c(by, id)
    ) |>
    dplyr::select(dplyr::all_of(by),
                  !!start := dplyr::all_of(is),
                  !!end := dplyr::all_of(ie)) |>
    dplyr::compute(temporary = FALSE, name = name,
                   logPrefix = "CohortConstructor_intersectCohorts_inner_join_")
}

#' Join overlapping periods in single periods using gap.
#'
#' @param x Table in the cdm.
#' @param name Table name
#' @param gap Distance between exposures to consider that they overlap.
#' @param startDate Column that indicates the start of periods.
#' @param endDate Column that indicates the end of periods.
#' @param by Variables to group by.
#'
#' @noRd
#'
#' @return Table in the cdm with startDate, endDate and by as columns. Periods are not
#' going to overlap between each other.
#'
joinOverlap <- function(cohort,
                        name,
                        gap = 0,
                        startDate = "cohort_start_date",
                        endDate = "cohort_end_date",
                        by = c("cohort_definition_id", "subject_id")) {

  if (cohort |> dplyr::tally() |> dplyr::pull("n") == 0) {
    return(
      cohort |>
        dplyr::select(dplyr::all_of(c(by, startDate, endDate))) |>
        dplyr::compute(name = name, temporary = FALSE,
                       logPrefix = "CohortConstructor_joinOverlap_input_")
    )
  }

  gap <- as.integer(gap)
  cdm <- omopgenerics::cdmReference(cohort)

  start <- cohort |>
    dplyr::select(dplyr::all_of(by), "date" := !!startDate) |>
    dplyr::mutate("date_id" = -1)
  end <- cohort |>
    dplyr::select(dplyr::all_of(by), "date" := !!endDate) |>
    dplyr::mutate("date_id" = 1)
  if (gap > 0) {
    end <- end |>
      dplyr::mutate("date" = as.Date(clock::add_days(x = .data$date, n = .env$gap)))
  }
  workingTbl <- omopgenerics::uniqueTableName()
  x <- start |>
    dplyr::union_all(end) |>
    dplyr::compute(temporary = FALSE, name = workingTbl,
                   logPrefix = "CohortConstructor_joinOverlap_workingTbl_")

  x <- x |>
    dplyr::group_by(dplyr::pick(dplyr::all_of(by))) |>
    dplyr::arrange(.data$date, .data$date_id) |>
    dplyr::mutate(
      "cum_id" = cumsum(.data$date_id),
      "name" = dplyr::if_else(.data$date_id == -1, .env$startDate, .env$endDate),
      "era_id" = dplyr::if_else(.data$date_id == -1, 1, 0)
    ) |>
    dplyr::filter(.data$cum_id == 0 |
                    (.data$cum_id == -1 & .data$date_id == -1)) |>
    dplyr::mutate("era_id" = cumsum(as.numeric(.data$era_id))) |>
    dplyr::ungroup() |>
    dplyr::arrange() |>
    dplyr::select(dplyr::all_of(c(by, "era_id", "name", "date"))) |>
    dplyr::compute(temporary = FALSE, name = workingTbl,
                   logPrefix = "CohortConstructor_joinOverlap_ids_") |>
    tidyr::pivot_wider(names_from = "name", values_from = "date") |>
    dplyr::select(-"era_id") |>
    dplyr::compute(temporary = FALSE, name = workingTbl,
                   logPrefix = "CohortConstructor_joinOverlap_pivot_wider_")
  if (gap > 0) {
    x <- x |>
      dplyr::mutate(!!endDate := as.Date(clock::add_days(x = .data[[endDate]], n = -gap)))
  }

  x <- x |>
    dplyr::relocate(dplyr::all_of(c(by, startDate, endDate))) |>
    dplyr::distinct() |>
    dplyr::compute(temporary = FALSE, name = name,
                   logPrefix = "CohortConstructor_joinOverlap_relocate_")

  omopgenerics::dropSourceTable(cdm = cdm, name = workingTbl)

  return(x)
}

#' Join all periods into single periods (joinOverlap with gap = Inf).
#'
#' @param x Table in the cdm.
#' @param startDate Column that indicates the start of periods.
#' @param endDate Column that indicates the end of periods.
#' @param by Variables to group by.
#'
#' @noRd
#'
#' @return Table in the cdm with startDate, endDate and by as columns. Periods are not
#' going to overlap between each other.
#'
joinAll <- function(cohort,
                    startDate = "cohort_start_date",
                    endDate = "cohort_end_date",
                    by = c("cohort_definition_id", "subject_id")) {
  if (cohort |> dplyr::tally() |> dplyr::pull("n") == 0) {
    return(cohort)
  }

  x <- cohort |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      cohort_start_date =
        min(.data$cohort_start_date, na.rm = TRUE),
      cohort_end_date =
        max(.data$cohort_end_date, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  return(x)
}


#' Get random identifiers not present in a table based on a prefix.
#'
#' @param x Table.
#' @param len Number of identifiers.
#' @param prefix Character vector with the prefix of the identifiers.
#' @param nchar Number of random characters added to the prefix.
#'
#' @noRd
#'
#' @return Character vector of identifiers not present in x.
#'
getIdentifier <- function(x,
                          len = 1,
                          prefix = "",
                          nchar = 5) {
  checkmate::assertClass(x, "tbl")
  checkmate::assertIntegerish(len,
                              lower = 1,
                              len = 1,
                              any.missing = FALSE)
  checkmate::assertCharacter(prefix, any.missing = FALSE, len = 1)
  checkmate::assertIntegerish(nchar,
                              len = 1,
                              lower = 1,
                              any.missing = FALSE)

  cols <- colnames(x)

  x <- character()
  for (k in seq_len(len)) {
    r <- paste0(prefix, getRandom(nchar))
    while (r %in% c(x, cols)) {
      r <- paste0(prefix, getRandom(nchar))
    }
    x <- c(x, r)
  }

  return(x)
}

getRandom <- function(n) {
  sample(x = letters,
         size = n,
         replace = TRUE) |> paste0(collapse = "")
}
addNames <- function(cs) {
  cols <- colnames(cs)[colnames(cs) != "cohort_definition_id"]
  cs <- cs |> dplyr::mutate("cohort_name" = NA_character_)
  for (col in cols) {
    cs <- cs |>
      dplyr::mutate(
        "cohort_name" = dplyr::case_when(
          .data[[col]] == 1 & is.na(.data$cohort_name) ~ .env$col,
          .data[[col]] == 1 &
            !is.na(.data$cohort_name) ~ paste0(.data$cohort_name, "_", .env$col),
          TRUE ~ .data$cohort_name
        )
      )
  }
  return(cs)
}

getPriorCohortCount <- function(attr) {
  attr |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::filter(.data$reason_id == max(.data$reason_id)) |>
    dplyr::summarise(
      "previous_number_records" = sum(.data$number_records),
      "previous_number_subjects" = sum(.data$number_subjects),
      .groups = "drop"
    )
}

addAttritionReason <- function(att, counts, reason) {
  counts <- att |>
    dplyr::distinct(.data$cohort_definition_id) |>
    dplyr::left_join(counts, by = "cohort_definition_id") |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("number_"), ~dplyr::if_else(is.na(.x), 0L, .x))
    )
  dplyr::bind_rows(
    att |>
      dplyr::select(dplyr::all_of(
        omopgenerics::cohortColumns("cohort_attrition")
      )),
    counts |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(c("number_records", "number_subjects")),
        ~ dplyr::if_else(is.na(.x), 0L, as.integer(.x))
      )) |>
      dplyr::inner_join(att |> getPriorCohortCount(), by = "cohort_definition_id") |>
      dplyr::mutate(
        "excluded_records" = .data$previous_number_records - .data$number_records,
        "excluded_subjects" = .data$previous_number_subjects - .data$number_subjects
      ) |>
      dplyr::inner_join(
        att |>
          dplyr::filter(
            .data$reason_id == max(.data$reason_id)
          ) |>
          dplyr::select("cohort_definition_id", "reason_id") |>
          dplyr::rowwise() |>
          dplyr::mutate("reason_id" = .data$reason_id + 1L, "reason" = reason),
        by = "cohort_definition_id"
      ) |>
      dplyr::select(dplyr::all_of(
        omopgenerics::cohortColumns("cohort_attrition")
      ))
  )
}
