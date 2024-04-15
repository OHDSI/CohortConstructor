#' Generate a combination cohort set between the intersection of different
#' cohorts.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param gap Number of days between two subsequent cohort entries to be merged
#' in a single cohort record.
#' @param mutuallyExclusive Whether the generated cohorts are mutually
#' exclusive or not.
#' @param returnOnlyComb Whether to only get the combination cohort back
#' @param name Name of the new cohort with the demographic requirements.
#'
#' @export
#'
#' @return The cdm object with the new generated cohort set
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort3 <- intersectCohort(
#'   cohort = cdm$cohort1,
#'   name = "cohort3",
#' )
#'
#' cdm$cohort3
#'
#' CDMConnector::settings(cdm$cohort3)
#'
#' }

intersectCohorts <- function(cohort,
                            cohortId = NULL,
                            gap = 0,
                            mutuallyExclusive = FALSE,
                            returnOnlyComb = FALSE,
                            name = omopgenerics::tableName(cohort)) {

  # checks
  name <- validateName(name)
  validateCohortTable(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  validateCDM(cdm)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  cohortId <- validateCohortId(cohortId, ids)
  assertNumeric(gap, integerish = TRUE, min = 0, length = 1)
  assertLogical(mutuallyExclusive, length = 1)
  assertLogical(returnOnlyComb, length = 1)

  # check targetCohortId
  if (is.null(cohortId)) {
    cohortId <- CDMConnector::settings(cohort) %>%
      dplyr::pull("cohort_definition_id")
  }
  if (length(cohortId) < 2) {
    cli::cli_warn("At least 2 cohort id must be provided to do the intersection.")
    # update properly
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohortId) %>%
      dplyr::compute(name = name, temporary = FALSE) %>%
      omopgenerics::newCohortTable(
        cohortSetRef = cohort %>%
          omopgenerics::settings() %>%
          dplyr::filter(.data$cohort_definition_id == .env$cohortId) %>%
          dplyr::compute(name = paste0(name, "_set"), temporary = FALSE),
        cohortAttritionRef = cohort %>%
          omopgenerics::attrition() %>%
          dplyr::filter(.data$cohort_definition_id == .env$cohortId) %>%
          dplyr::compute(name = paste0(name, "_attrition"), temporary = FALSE)
      )
    return(cohort)
  }

  # generate cohort
  cohortOut <- cohort %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) %>%
    dplyr::select(-"cohort_definition_id") %>%
    splitOverlap(by = "subject_id") %>%
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = omopgenerics::tableName(cohort),
      targetCohortId = cohortId,
      window = c(0, 0),
      nameStyle = "{cohort_name}"
    )

  # create cohort_definition_id
  cohortNames <- omopgenerics::settings(cohort) %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) %>%
    dplyr::pull("cohort_name")
  x <- rep(list(c(0, 1)), length(cohortNames))
  names(x) <- cohortNames
  cohSet <- expand.grid(x) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ . != 0)) %>%
    addNames() %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number())

  if (!mutuallyExclusive) {
    dic <- cohSet %>%
      dplyr::mutate(cohort_definition_id = dplyr::row_number()) %>%
      dplyr::select("cohort_name", "cohort_definition_id")
    cohSet <- cohSet %>%
      dplyr::select(-"cohort_name", -"cohort_definition_id") %>%
      notMutuallyEclusiveCohortSet() %>%
      dplyr::inner_join(dic, by = "cohort_definition_id")
  }

  individualId <- cohSet %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sum = sum(dplyr::c_across(-dplyr::all_of(
      c("cohort_definition_id", "cohort_name"))), na.rm = TRUE)) %>%
    dplyr::filter(.data$sum == 1) %>%
    dplyr::pull("cohort_definition_id")
  if (returnOnlyComb) {
    cohSet <- cohSet |>
      dplyr::filter(!.data$cohort_definition_id %in% .env$individualId) %>%
      dplyr::group_by(.data$cohort_name) %>%
      dplyr::mutate(cohort_definition_id = dplyr::cur_group_id()) %>%
      dplyr::ungroup()
  }

  ## intersect cohort
  tempName <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = tempName,
    table = cohSet %>%
      dplyr::left_join(
        settings(cohort) %>%
          dplyr::select("id_in" = "cohort_definition_id", "cohort_name"),
        by = "cohort_name")
  )
  if (!returnOnlyComb & !mutuallyExclusive & gap < 1) {
    # if not mutually exclusive --> cohorts in = individual cohorts out:
    # cohort in cannot be recover after splitting (if joinOverlap with gap = 1 is
    # done we might be joining different input entries)
    cohortOut <- cohortOut %>%
      dplyr::inner_join(
        cdm[[tempName]] |>
          dplyr::filter(!.data$cohort_definition_id %in% .env$individualId),
        by = cohortNames
      ) %>%
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date"
      ) %>%
      dplyr::union_all(
        cohort %>%
          dplyr::rename("id_in" = "cohort_definition_id") %>%
          dplyr::inner_join(
            cdm[[tempName]] |>
              dplyr::filter(.data$cohort_definition_id %in% .env$individualId),
            by = "id_in") %>%
          dplyr::select(
            "cohort_definition_id", "subject_id", "cohort_start_date",
            "cohort_end_date")
      ) %>%
      dplyr::compute(name = name, temporary = FALSE)
  } else {
    cohortOut <- cohortOut %>%
      dplyr::inner_join(cdm[[tempName]], by = cohortNames) %>%
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date"
      ) %>%
      dplyr::compute(name = name, temporary = FALSE)
  }
  cdm <- omopgenerics::dropTable(cdm = cdm, name = tempName)

  if (cohortOut |> dplyr::tally() |> dplyr::pull("n") > 0) {
    cohortOut <- joinOverlap(x = cohortOut, gap = gap) %>%
      dplyr::compute(name = name, temporary = FALSE)
  }

  if (!mutuallyExclusive) {
    cohSet <- cohSet %>%
      dplyr::group_by(.data$cohort_definition_id, .data$cohort_name) %>%
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(dplyr::n_distinct(.x) == 1, 1, 0)
      )) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
  }

  # attrition
  counts <- cohortOut |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(number_records = dplyr::n(),
                     number_subjects = dplyr::n_distinct(.data$subject_id)) |>
    dplyr::collect() |>
    dplyr::right_join(cohSet |> dplyr::select("cohort_definition_id"), by = "cohort_definition_id") |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("number"), ~dplyr::if_else(is.na(.x), 0, .x)))
  cohAtt <- intersectCohortAttrition(cohort, cohSet, counts, returnOnlyComb, mutuallyExclusive)

  # concept codelists
  codelist <- attr(cohort, "cohort_codelist")
  codelist <- cohSet |>
    dplyr::select(dplyr::all_of(c(cohortNames, "cohort_definition_id"))) |>
    tidyr::pivot_longer(cols = dplyr::all_of(cohortNames), names_to = "cohort_name") |>
    dplyr::filter(.data$value == 1) |>
    dplyr::select("cohort_definition_id", "cohort_name") |>
    dplyr::inner_join(
      settings(cohort) |>
        dplyr::inner_join(
          codelist,
          copy = TRUE,
          by = "cohort_definition_id") |>
        dplyr::select(-"cohort_definition_id"),
      by = "cohort_name"
      ) |>
    dplyr::select("cohort_definition_id", "codelist_name", "concept_id", "type")

  cohSet <- cohSet %>%
    dplyr::mutate("mutually_exclusive" = mutuallyExclusive, "gap" = gap) %>%
    dplyr::relocate(c("cohort_definition_id", "cohort_name"))


  cohortOut <- omopgenerics::newCohortTable(
    table = cohortOut, cohortSetRef = cohSet,
    cohortAttritionRef = cohAtt, cohortCodelistRef = codelist
  )

  return(cohortOut)
}

#' To split overlaping periods in non overlaping period.
#'
#' @param x Table in the cdm.
#' @param start Column that indicates the start of periods.
#' @param end Column that indicates the end of periods.
#' @param by Variables to group by.
#'
#' @export
#'
#' @return Table in the cdm with start, end and by as columns. Periods are not
#' going to overlap between each other.
#'
splitOverlap <- function(x,
                         start = "cohort_start_date",
                         end = "cohort_end_date",
                         by = c("cohort_definition_id", "subject_id")) {
  # initial checks
  checkmate::assertCharacter(start, len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assertCharacter(end, len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assertCharacter(by, min.len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assertClass(x, "tbl")
  checkmate::assertTRUE(all(c(start, end, by) %in% colnames(x)))

  ids <- getIdentifier(x, 3)
  id <- ids[1]
  is <- ids[2]
  ie <- ids[3]

  x %>%
    dplyr::select(dplyr::all_of(by), !!is := dplyr::all_of(start)) %>%
    dplyr::union_all(
      x %>%
        dplyr::select(dplyr::all_of(by), !!is := dplyr::all_of(end)) %>%
        dplyr::mutate(!!is := as.Date(!!CDMConnector::dateadd(is, 1)))
    ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dbplyr::window_order(.data[[is]]) %>%
    dplyr::mutate(!!id := dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(
      x %>%
        dplyr::select(dplyr::all_of(by), !!ie := dplyr::all_of(end)) %>%
        dplyr::union_all(
          x %>%
            dplyr::select(dplyr::all_of(by), !!ie := dplyr::all_of(start)) %>%
            dplyr::mutate(!!ie := as.Date(!!CDMConnector::dateadd(ie, -1)))
        ) %>%
        dplyr::distinct() %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
        dbplyr::window_order(.data[[ie]]) %>%
        dplyr::mutate(!!id := dplyr::row_number() - 1) %>%
        dbplyr::window_order() %>%
        dplyr::ungroup(),
      by = c(by, id)
    ) %>%
    dplyr::select(
      dplyr::all_of(by),
      !!start := dplyr::all_of(is),
      !!end := dplyr::all_of(ie)
    ) %>%
    dplyr::compute()
}

#' Join overlapping periods in single periods.
#'
#' @param x Table in the cdm.
#' @param start Column that indicates the start of periods.
#' @param end Column that indicates the end of periods.
#' @param by Variables to group by.
#' @param gap Distance between exposures to consider that they overlap.
#'
#' @export
#'
#' @return Table in the cdm with start, end and by as columns. Periods are not
#' going to overlap between each other.
joinOverlap <- function(x,
                        start = "cohort_start_date",
                        end = "cohort_end_date",
                        by = c("cohort_definition_id", "subject_id"),
                        gap = 0) {
  # initial checks
  checkmate::assertCharacter(start, len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assertCharacter(end, len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assertCharacter(by, min.len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assertNumeric(gap, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assertClass(x, "tbl")
  checkmate::assertTRUE(all(c(start, end, by) %in% colnames(x)))

  ids <- getIdentifier(x, 5)
  dat <- ids[1]
  id <- ids[2]
  cid <- ids[3]
  nam <- ids[4]
  era <- ids[5]

  x %>%
    dplyr::select(dplyr::all_of(by), !!dat := dplyr::all_of(start)) %>%
    dplyr::mutate(!!id := -1) %>%
    dplyr::union_all(
      x %>%
        dplyr::mutate(
          !!dat := as.Date(!!CDMConnector::dateadd(
            date = end, number = gap
          )),
          !!id := 1
        ) %>%
        dplyr::select(dplyr::all_of(c(by, dat, id)))
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dbplyr::window_order(.data[[dat]], .data[[id]]) %>%
    dplyr::mutate(!!cid := cumsum(.data[[id]])) %>%
    dplyr::filter(
      .data[[cid]] == 0 | (.data[[cid]] == -1 & .data[[id]] == -1)
    ) %>%
    dplyr::mutate(
      !!nam := dplyr::if_else(.data[[id]] == -1, .env$start, .env$end),
      !!era := dplyr::if_else(.data[[id]] == -1, 1, 0)
    ) %>%
    dplyr::mutate(!!era := cumsum(as.numeric(.data[[era]]))) %>%
    dplyr::ungroup() %>%
    dbplyr::window_order() %>%
    dplyr::select(dplyr::all_of(c(by, era, nam, dat))) %>%
    tidyr::pivot_wider(names_from = .env$nam, values_from = .env$dat) %>%
    dplyr::mutate(!!end := as.Date(!!CDMConnector::dateadd(
      date = end,
      number = -gap
    ))) %>%
    dplyr::select(dplyr::all_of(c(by, start, end))) %>%
    dplyr::compute()
}

#' Get random identifiers not present in a table based on a prefix.
#'
#' @param x Table.
#' @param len Number of identifiers.
#' @param prefix Character vector with the prefix of the identifiers.
#' @param nchar Number of random characters added to the prefix.
#'
#' @export
#'
#' @return Character vector of identifiers not present in x.
#'
getIdentifier <- function(x, len = 1, prefix = "", nchar = 5) {
  checkmate::assertClass(x, "tbl")
  checkmate::assertIntegerish(len, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(prefix, any.missing = FALSE, len = 1)
  checkmate::assertIntegerish(nchar, len = 1, lower = 1, any.missing = FALSE)

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
  sample(x = letters, size = n, replace = TRUE) %>% paste0(collapse = "")
}
addNames <- function(cs) {
  cols <- colnames(cs)[colnames(cs) != "cohort_definition_id"]
  cs <- cs %>% dplyr::mutate("cohort_name" = as.character(NA))
  for (col in cols) {
    cs <- cs %>%
      dplyr::mutate("cohort_name" = dplyr::case_when(
        .data[[col]] == 1 & is.na(.data$cohort_name) ~ .env$col,
        .data[[col]] == 1 & !is.na(.data$cohort_name) ~ paste0(.data$cohort_name, "_", .env$col),
        TRUE ~ .data$cohort_name
      ))
  }
  return(cs)
}
notMutuallyEclusiveCohortSet <- function(cs) {
  logic <- cs %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(!"cohort_definition_id") %>%
    dplyr::filter(.data$value == 1)
  cohset <- list()
  for (k in logic$cohort_definition_id) {
    logi <- logic %>%
      dplyr::filter(.data$cohort_definition_id == .env$k) %>%
      tidyr::pivot_wider()
    cohset[[k]] <- cs %>%
      dplyr::inner_join(
        logi, by = colnames(logi)[colnames(logi) != "cohort_definition_id"]
      )
  }
  cs <- dplyr::bind_rows(cohset)
  return(cs)
}

intersectCohortAttrition <- function(cohort, cohortSet, counts, returnOnlyComb, mutuallyExclusive) {
  # attrition
  # intersect cohorts
  intersectId <- cohortSet |>
    dplyr::rowwise() %>%
    dplyr::mutate(sum = sum(dplyr::c_across(-dplyr::all_of(
      c("cohort_definition_id", "cohort_name"))), na.rm = TRUE)) %>%
    dplyr::filter(.data$sum > 1) |>
    dplyr::pull("cohort_definition_id")
  cohAtt <- counts |>
    dplyr::filter(.data$cohort_definition_id %in% .env$intersectId) |>
    dplyr::mutate(
      "reason_id" = 1,
      "reason" = "Initial qualifying events",
      "excluded_records" = 0,
      "excluded_subjects" = 0
    )
  if (!returnOnlyComb) {
    # individual cohorts
    individualId <- cohortSet$cohort_definition_id[!cohortSet$cohort_definition_id %in% .env$intersectId]
    cohAtt <- cohAtt |>
      dplyr::union_all(
        cohortSet |>
          dplyr::inner_join(
            omopgenerics::attrition(cohort) |>
              dplyr::inner_join(
                omopgenerics::settings(cohort) |>
                  dplyr::select("cohort_definition_id", "cohort_name"),
                by = "cohort_definition_id") |>
              dplyr::select(-"cohort_definition_id"),
            by = "cohort_name"
          )|>
          dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort_attrition")))
      )
    if (mutuallyExclusive) {
      cohAtt <- cohAtt %>%
        addAttritionReason(
          counts = counts, ids = individualId,
          reason = "Mutually exclusive cohorts"
        )
    }
  }
  cohAtt <- cohAtt |>
    dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort_attrition"))) |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id)
  return(cohAtt)
}

getPriorCohortCount <- function(attr, ids) {
  attr |>
    dplyr::filter(.data$cohort_definition_id %in% ids) |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::filter(.data$reason_id == max(.data$reason_id)) |>
    dplyr::summarise(
      "previous_number_records" = sum(.data$number_records),
      "previous_number_subjects" = sum(.data$number_subjects),
      .groups = "drop"
    )
}

addAttritionReason <- function(att, counts, ids, reason) {
  dplyr::bind_rows(
    att |>
      dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort_attrition"))),
    counts |>
      dplyr::filter(.data$cohort_definition_id %in% ids) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("number_records", "number_subjects")),
          ~ dplyr::if_else(is.na(.x), as.integer(0), as.integer(.x))
        )) |>
      dplyr::inner_join(att |> getPriorCohortCount(ids), by = "cohort_definition_id") |>
      dplyr::mutate(
        "excluded_records" = .data$previous_number_records - .data$number_records,
        "excluded_subjects" = .data$previous_number_subjects - .data$number_subjects
      ) |>
      dplyr::inner_join(
        att |>
          dplyr::filter(.data$cohort_definition_id %in% ids, .data$reason_id == max(.data$reason_id)) |>
          dplyr::select("cohort_definition_id", "reason_id") |>
          dplyr::rowwise() |>
          dplyr::mutate(
            "reason_id" = .data$reason_id + 1,
            "reason" = reason
          ),
        by = "cohort_definition_id"
      ) |>
      dplyr::select(dplyr::all_of(omopgenerics::cohortColumns("cohort_attrition")))
  )
}
