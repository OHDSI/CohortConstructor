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
  if (is.infinite(n)) cli::cli_abort("`n` cannot be infinite.")

  newCohort <- duplicateCohort(cohort = cohort,
                                 name = name,
                                 cohortId = cohortId)

  if(n == 1){
    return(newCohort)
  }

  start_set <- attr(newCohort, "cohort_set") |>
    dplyr::collect()

  tmp1 <- omopgenerics::uniqueTableName()
  cdm[[tmp1]] <- duplicateCohort(cohort = newCohort,
                                 name = tmp1)
  cdm[[tmp1]] <- cdm[[tmp1]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = start_set,
      cohortAttritionRef = attr(newCohort, "cohort_attrition"),
      cohortCodelistRef = attr(newCohort, "cohort_codelist"),
      .softValidation = TRUE
    )

  tmp2 <- omopgenerics::uniqueTableName()
  for(i in 2:n) {
    cdm[[tmp2]] <- duplicateCohort(cohort = newCohort, name = tmp2)
    cdm[[tmp2]] <- cdm[[tmp2]] |>
      omopgenerics::newCohortTable(
        cohortSetRef = start_set |>
          dplyr::mutate(original_cohort_id = cohort_definition_id,
                        original_cohort_name = cohort_name) |>
          dplyr::mutate(cohort_name = paste0(cohort_name, "_", as.integer(i-1))),
        cohortAttritionRef = attr(newCohort, "cohort_attrition"),
        cohortCodelistRef = attr(newCohort, "cohort_codelist"),
        .softValidation = TRUE
      )
    cdm <- omopgenerics::bind(cdm[[tmp1]],
                             cdm[[tmp2]],
                             name = tmp1)
  }

  multipleCohorts <- cdm[[tmp1]] |>
    duplicateCohort(name)

  omopgenerics::dropSourceTable(cdm = cdm, name = tmp2)
  omopgenerics::dropSourceTable(cdm = cdm, name = tmp1)


  return(multipleCohorts)
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
