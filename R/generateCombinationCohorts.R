# generateCombinationCohortSet <- function(cdm,
#                                          name,
#                                          targetCohortName,
#                                          targetCohortId = NULL,
#                                          mutuallyExclusive = FALSE) {
#   # initial checks
#   # checkInput()
#
#   # check targetCohortId
#   if (is.null(targetCohortId)) {
#     targetCohortId <- OMOPGenerics::cohortSet(cdm[[targetCohortName]]) %>%
#       dplyr::pull("cohort_definition_id")
#   }
#   if (length(targetCohortId) < 2) {
#     cli::cli_warn("At least 2 cohort id must be provided to do the combination")
#     # update properly
#     cdm[[name]] <- cdm[[targetCohortName]]
#     return(cdm)
#   }
#
#   # generate cohort
#   names <- OMOPGenerics::cohortSet(cdm[[targetCohortName]]) %>%
#     dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) %>%
#     dplyr::pull("cohort_definition_id")
#   cohort <- periodSplit(cdm, targetCohortName, targetCohortId) %>%
#     PatientProfiles::addCohortIntersectFlag(
#       targetCohortTable = targetCohortName,
#       targetCohortId = targetCohortId,
#       indexDate = "start",
#       window = c(0, 0),
#       nameStyle = "{cohort_name}"
#     ) %>%
#     getCohort()
# }

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
#' going to overlpa between each other.
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
    dplyr::group_by(dplyr::across(by)) %>%
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
        dplyr::group_by(dplyr::across(by)) %>%
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
    CDMConnector::computeQuery()
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
#' going to overlpa between each other.
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
    CDMConnector::computeQuery()
}

#' Get ramdon identifies not present in a table based on a prefix.
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
  sample(x = letters, size = n, replace = TRUE) |> paste0(collapse = "")
}

# getCohortSetMutuallyEclusive <- function(names) {
#   lapply(names, function(x){c(0, 1)}) |>
#     expand.grid() |>
#     rlang::set_names(names) |>
#     dplyr::as_tibble() |>
#     dplyr::fi
#     dplyr::mutate("cohort_definition_id" = dplyr::row_number())
# }
# getCohortSetNotMutuallyEclusive <- function(names) {
#   lapply(names, function(x){c(0, 1)}) |>
#     expand.grid() |>
#     rlang::set_names(names) |>
#     dplyr::as_tibble() |>
#     dplyr::mutate("cohort_definition_id" = dplyr::row_number())
# }
