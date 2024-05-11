#' Crate a new cohort table with the specified stratifications.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param strata A strata list that point to columns in cohort table.
#' @param removeStrata Whether to remove strata columns from final cohort table.
#' @param name Name of the new cohort.
#'
#' @return Cohort table stratified.
#'
#' @export
#'
#' @examples
#' library(CohortConstructor)
#' library(PatientProfiles)
#'
#' # cdm <- mockCohortConstructor()
#' #
#' # cdm$my_cohort <- conceptCohort(
#' #   cdm = cdm,
#' #   conceptSet = list(x = c(1, 2), y = c(2, 7, 8)),
#' #   name = "my_cohort"
#' # ) |>
#' #  addAge(ageGroups = list("child" = c(0, 17), "adult" = c(18, Inf))) |>
#' #  addSex() |>
#' #  stratifyCohorts(
#' #    strata = list("sex, c("sex", "age_group")), name = "my_cohort"
#' #  )
#' #
#' # cdm$mycohort
#' #
#' # settings(cdm$my_cohort)
#' #
#' # attrition(cdm$my_cohort)
#'
stratifyCohorts <- function(cohort,
                            cohortId = NULL,
                            strata = list(),
                            removeStrata = TRUE,
                            name = tableName(cohort)) {
  # initial checks
  cohort <- validateCohortTable(cohort = cohort)
  cohortId <- validateCohortId(cohortId, settings(cohort)$cohort_name)
  strata <- validateStrata(strata, cohort)
  name <- validateName(name)

  cdm <- omopgenerics::cdmReference(cohort)

  if (length(strata) == 0) {
    if (name != tableName(cohort)) {
      return(
        cohort |>
          dplyr::compute(name = name, temporary = FALSE) |>
          omopgenerics::newCohortTable()
      )
    } else {
      return(cohort)
    }
  }

  strataCols <- unique(unlist(strata))

  set <- settings(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohrotId) |>
    dplyr::mutate("target_cohort_table_name" = tableName(cohort)) |>
    dplyr::rename(
      "target_cohort_id" = "cohort_definition_id",
      "target_cohort_name" = "cohort_name"
    )

  # drop columns from set
  dropCols <- colnames(set)[colnames(set) %in% strataCols]
  if (length(dropCols) > 0) {
    cli::cli_inform(c(
      "!" = "{dropCols} {?is/are} present in settings and strata. Settings
      column will be not considered."
    ))
    set <- set |> dplyr::select(!dplyr::all_of(dropCols))
  }

  # get counts for attrition
  counts <- cohort |>
    dplyr::group_by(dplyr::across(dplyr::all_of(strataCols))) |>
    dplyr::tally() |>
    dplyr::collect()

  newSettings <- getNewSettingsStrata(set, strata, counts)

  id <- uniqueId(c(colnames(cohort), colnames(set), strataCols))
  nm <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = nm,
    table = newSettings |>
      dplyr::bind_rows(.id = id) |>
      dplyr::select(
        "cohort_definition_id",
        "target_cohort_id",
        dplyr::all_of(c(strataCols, id))
      )
  )

  newCohort <- list()
  for (k in seq_along(strata)) {
    newCohort[[k]] <- cohort |>
      dplyr::rename("target_cohort_id" = "cohort_definition_id") |>
      dplyr::inner_join(
        cdm[[nm]] |>
          dplyr::filter(.data[[id]] == .env$k) |>
          dplyr::select(
            "cohort_definition_id",
            "target_cohort_id",
            dplyr::all_of(strata[[k]])
          ),
        by = c("target_cohort_id", strata[[k]])
      )
  }
  newCohort <- purrr::map(newCohort, dplyr::union_all) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSettings |> dplyr::bind_rows(),
      cohortAttritionRef = NULL,
      cohortCodelistRef = NULL
    )

  omopgenerics::dropTable(cdm = cdm, name = nm)

  return(newCohort)
}

getNewSettingsStrata <- function(set, strata, counts) {
  id <- uniqueId(c(colnames(set), colnames(counts)))
  lapply(strata, function(x) {
    set |>
      dplyr::cross_join(
        counts |>
          dplyr::select(dplyr::all_of(x)) |>
          dplyr::distinct() |>
          tidyr::unite(
            col = "cohort_name", dplyr::all_of(x), sep = "_", remove = FALSE
          )
      )
  }) |>
    dplyr::bind_rows(.id = id) |>
    dplyr::mutate(
      "cohort_name" = paste0(.data$target_cohort_name, "_", .data$cohort_name),
      "cohort_definition_id" = dplyr::row_number()
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id))) |>
    dplyr::group_split() |>
    as.list()
}
uniqueId <- function(cols = character(), n = 1) {
  ids <- tidyr::expand_grid(x = letters, y = letters, z = letters) |>
    dplyr::mutate("id" = paste0("id_", .data$x, .data$y, .data$z)) |>
    dplyr::pull("id")
  cols <- cols[nchar(cols) == 5]
  ids <- ids[!ids %in% cols]
  sample(x = ids, size = n)
}
