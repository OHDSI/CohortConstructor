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
#
# periodSplit <- function(x, start, end, id = "subject_id") {
#   x %>%
#     dplyr::select(dplyr::all_of(id), "start" = dplyr::all_of(start)) %>%
#     dplyr::union_all(
#       x %>%
#         dplyr::select(dplyr::all_of(id), "start" = dplyr::all_of(end)) %>%
#         dplyr::mutate("start" = as.Date(!!CDMConnector::dateadd("start", +1)))
#     ) %>%
#     dplyr::distinct() %>%
#     dplyr::group_by(dplyr::across(id)) %>%
#     dbplyr::window_order(.data$start) %>%
#     dplyr::mutate("a1b2c3" = dplyr::row_number()) %>%
#     dbplyr::window_order() %>%
#     dplyr::ungroup() %>%
#     dplyr::inner_join(
#       x %>%
#         dplyr::select(dplyr::all_of(id), "end" = dplyr::all_of(end)) %>%
#         dplyr::union_all(
#           x %>%
#             dplyr::select(dplyr::all_of(id), "end" = dplyr::all_of(start)) %>%
#             dplyr::mutate("end" = as.Date(!!CDMConnector::dateadd("end", -1)))
#         ) %>%
#         dplyr::distinct() %>%
#         dplyr::group_by(dplyr::across(id)) %>%
#         dbplyr::window_order(.data$start) %>%
#         dplyr::mutate("a1b2c3" = dplyr::row_number() - 1) %>%
#         dbplyr::window_order() %>%
#         dplyr::ungroup(),
#       by = c(id, "a1b2c3")
#     ) %>%
#     dplyr::select(-"a1b2c3") %>%
#     CDMConnector::computeQuery()
# }
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
