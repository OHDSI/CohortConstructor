#' Copy a cohort table
#'
#' @description
#' `copyCohorts()` copies an existing cohort table to a new location.
#'
#' @inheritParams cohortDoc
#' @inheritParams nameDoc
#' @inheritParams cohortIdSubsetDoc
#' @param n Number of times to duplicate the selected cohorts.
#'
#' @return A new cohort table containing cohorts from the original cohort table.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#' cdm$cohort3 <- copyCohorts(cdm$cohort1, n = 2, cohortId = 1, name = "cohort3")
#' }
copyCohorts <- function(cohort, name, n = 1, cohortId = NULL) {

  omopgenerics::validateCohortArgument(cohort)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort = cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  omopgenerics::validateNameArgument(name, cdm = cdm, validation = "warning")
  omopgenerics::assertNumeric(x = n, integerish = TRUE, min = 1, length = 1)
  if (is.infinite(n)){
    cli::cli_abort("`n` cannot be infinite.")
  }

  newCohort <- duplicateCohort(cohort = cohort,
                               name = name,
                               cohortId = cohortId)

  if (n == 1) {
    return(newCohort)
  }

  # create equivalence between old cohort set and new cohorts
  set <- omopgenerics::settings(newCohort) |>
    dplyr::select("cohort_definition_id", "cohort_name") |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  newSet <- list()
  for (k in seq_len(n)) {
    if (k > 1) {
      newSet[[k]] <- set |>
        dplyr::rename(original_cohort_name = "cohort_name") |>
        dplyr::mutate(cohort_name = paste0(cohort_name, "_", k - 1))
    } else {
      newSet[[k]] <- set |>
        dplyr::mutate(original_cohort_name = .data$cohort_name)
    }
  }
  newSet <- dplyr::bind_rows(newSet) |>
    dplyr::mutate(new_cohort_definition_id = dplyr::row_number())
  joinSet <- newSet |>
    dplyr::select("cohort_definition_id", "new_cohort_definition_id")

  # create new attrition
  newAttrition <-  omopgenerics::attrition(newCohort) |>
    dplyr::inner_join(joinSet, by = "cohort_definition_id", relationship = "many-to-many") |>
    dplyr::select(!"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")

  # create new codelist attribute
  newCodelist <- attr(newCohort, "cohort_codelist") |>
    dplyr::collect() |>
    dplyr::inner_join(joinSet, by = "cohort_definition_id", relationship = "many-to-many") |>
    dplyr::select(!"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")

  # copy cohort
  nm <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::cdmReference(newCohort) |>
    omopgenerics::insertTable(name = nm, table = joinSet)
  newCohort <- newCohort |>
    dplyr::inner_join(cdm[[nm]], by = "cohort_definition_id", relationship = "many-to-many") |>
    dplyr::select(!"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::compute(name = name)

  # new settings
  newSet <- newSet |>
    dplyr::rename(
      original_cohort_id = "cohort_definition_id",
      cohort_definition_id = "new_cohort_definition_id"
    )

  cdm[[name]] <- newCohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet,
      cohortAttritionRef = newAttrition,
      cohortCodelistRef = newCodelist,
      .softValidation = TRUE
    )

  omopgenerics::dropSourceTable(cdm = cdm, name = nm)

  return(cdm[[name]])
}

# create single copy
duplicateCohort  <- function(cohort, name, cohortId = NULL) {

  if (isFALSE(needsIdFilter(cohort = cohort, cohortId = cohortId))){

    if(name == omopgenerics::tableName(cohort)){
    # same as input so return as is
    return(cohort)
    } else {
      newCohort <- cohort |>
        dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE,
                       logPrefix = "CohortConstructor_copyCohors_subset_")
      newCohort <- newCohort |>
        omopgenerics::newCohortTable(
          cohortSetRef = attr(cohort, "cohort_set"),
          cohortAttritionRef = attr(cohort, "cohort_attrition"),
          cohortCodelistRef = attr(cohort, "cohort_codelist"),
          .softValidation = TRUE
        )
    }
  } else {
    newCohort <- cohort |>
      CohortConstructor::subsetCohorts(cohortId = cohortId, name = name)
  }

  return(newCohort)

}
