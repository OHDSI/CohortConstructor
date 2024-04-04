#' Generate a combination cohort set between the intersection of different
#' cohorts.
#'
#' @param cdm A cdm reference.
#' @param name Name of the new generated cohort.
#' @param targetCohortName Name of an existent cohort in the cdm to create the
#' combinations.
#' @param targetCohortId Ids to combine of the target cohort. If NULL all
#' cohort present in the table will be used.
#' @param mutuallyExclusive Whether the generated cohorts are mutually
#' exclusive or not.
#' @param returnOnlyComb Whether to only get the combination cohort back
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
#' cdm <- generateIntersectCohortSet(
#'   cdm = cdm,
#'   name = "cohort3",
#'   targetCohortName = "cohort2"
#' )
#'
#' cdm$cohort3
#'
#' CDMConnector::settings(cdm$cohort3)
#'
#' }

generateIntersectCohortSet <- function(cdm,
                                       name,
                                       targetCohortName,
                                       targetCohortId = NULL,
                                       mutuallyExclusive = FALSE,
                                       returnOnlyComb = FALSE) {
  # initial checks
  checkmate::checkClass(cdm, "cdm_reference")
  checkmate::checkCharacter(name, len = 1, any.missing = FALSE, min.chars = 1)
  checkmate::checkCharacter(targetCohortName, len = 1, any.missing = FALSE, min.chars = 1)
  checkmate::checkTRUE(targetCohortName %in% names(cdm))
  checkmate::checkIntegerish(targetCohortId, null.ok = TRUE, any.missing = FALSE)
  checkmate::checkLogical(mutuallyExclusive, len = 1, any.missing = FALSE)

  # check targetCohortId
  if (is.null(targetCohortId)) {
    targetCohortId <- CDMConnector::settings(cdm[[targetCohortName]]) %>%
      dplyr::pull("cohort_definition_id")
  }
  if (length(targetCohortId) < 2) {
    cli::cli_warn("At least 2 cohort id must be provided to do the combination")
    # update properly
    cdm[[name]] <- cdm[[targetCohortName]] %>%
      dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
      dplyr::compute(name = name, temporary = FALSE) %>%
      omopgenerics::newCohortTable(
        cohortSetRef = cdm[[targetCohortName]] %>%
          omopgenerics::settings() %>%
          dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
          dplyr::compute(name = paste0(name, "_set"), temporary = FALSE),
        cohortAttritionRef = cdm[[targetCohortName]] %>%
          omopgenerics::attrition() %>%
          dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
          dplyr::compute(name = paste0(name, "_attrition"), temporary = FALSE)
      )
    return(cdm)
  }

  # generate cohort
  cohort <- cdm[[targetCohortName]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) %>%
    dplyr::select(-"cohort_definition_id") %>%
    splitOverlap(by = "subject_id") %>%
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = targetCohortName,
      targetCohortId = targetCohortId,
      window = c(0, 0),
      nameStyle = "{cohort_name}"
    )

  # create cohort_definition_id
  cohortNames <- omopgenerics::settings(cdm[[targetCohortName]]) %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) %>%
    dplyr::pull("cohort_name")
  x <- rep(list(c(0, 1)), length(cohortNames))
  names(x) <- cohortNames
  cohSet <- expand.grid(x) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ . != 0)) %>%
    addNames() %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number())

  if (! mutuallyExclusive) {
    dic <- cohSet %>%
      dplyr::mutate(cohort_definition_id = dplyr::row_number()) %>%
      dplyr::select("cohort_name", "cohort_definition_id")
    cohSet <- cohSet %>%
      dplyr::select(-"cohort_name", -"cohort_definition_id") %>%
      notMutuallyEclusiveCohortSet() %>%
      dplyr::inner_join(dic, by = "cohort_definition_id")
  }

  if (returnOnlyComb) {
  toEliminate <- cohSet %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      sum = sum(dplyr::c_across(-dplyr::all_of(c("cohort_definition_id", "cohort_name"))),
                na.rm = TRUE)
    ) %>%
    dplyr::filter(.data$sum == 1) %>%
    dplyr::pull("cohort_definition_id")
  cohSet <- cohSet |>
    dplyr::filter(!.data$cohort_definition_id %in% .env$toEliminate) %>%
    dplyr::group_by(.data$cohort_name) %>%
    dplyr::mutate(cohort_definition_id = dplyr::cur_group_id())
  }

  # add cohort definition id
  tempName <- "tmp_cohset"
  cdm <- omopgenerics::insertTable(cdm = cdm, name = tempName, table = cohSet)
  cohort <- cohort %>%
    dplyr::inner_join(cdm[[tempName]], by = cohortNames) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::compute(name = name, temporary = FALSE)

  cdm <- omopgenerics::dropTable(cdm = cdm, name = tempName)


  if (!mutuallyExclusive) {
    cohort <- joinOverlap(x = cohort, gap = 1) %>%
      dplyr::compute(name = name, temporary = FALSE)
    cohSet <- cohSet %>%
      dplyr::group_by(.data$cohort_definition_id, .data$cohort_name) %>%
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(dplyr::n_distinct(.x) == 1, 1, as.numeric(NA))
      )) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
  }

  # TODO
  # create attrition

  cohSet <- cohSet %>%
    dplyr::mutate("mutually_exclusive" = mutuallyExclusive) %>%
    dplyr::relocate(c("cohort_definition_id", "cohort_name"))

  cdm[[name]] <- omopgenerics::newCohortTable(
    table = cohort, cohortSetRef = cohSet, cohortAttritionRef = NULL
  )


  # TODO
  # add a not exposed option cohort

  return(cdm)
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
#'
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
