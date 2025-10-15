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
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#' cdm$cohort3 <- copyCohorts(cdm$cohort1, n = 2, cohortId = 1, name = "cohort3")
#' }
#' }
copyCohorts <- function(cohort, name, n = 1, cohortId = NULL) {
  omopgenerics::validateCohortArgument(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  omopgenerics::validateNameArgument(name, cdm = cdm, validation = "warning")
  omopgenerics::assertNumeric(x = n, integerish = TRUE, min = 1, length = 1)
  if (is.infinite(n)) cli::cli_abort("`n` cannot be infinite.")
  cohortId <- omopgenerics::validateCohortIdArgument(cohortId, cohort = cohort)

  # subset cohort
  if (isFALSE(needsIdFilter(cohort = cohort, cohortId = cohortId))){
    newCohort <- cohort |>
      dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE,
                     logPrefix = "CohortConstructor_copyCohors_subset_")
  } else {
    newCohort <- cohort |>
      CohortConstructor::subsetCohorts(cohortId = cohortId, name = name)
  }

  # duplicate
  if (n != 1) {
    set <- copySettings(settings(newCohort), n)
    att <- copyFromSet(attrition(newCohort), set)
    codes <- copyFromSet(attr(newCohort, "cohort_codelist") |> dplyr::collect(), set)
    newCohort <- copyFromSet(newCohort, set)
    newCohort <- newCohort |>
      dplyr::compute(name = name, temporary = FALSE,
                     logPrefix = "CohortConstructor_copyCohors_duplicate_") |>
      omopgenerics::newCohortTable(
        cohortSetRef = set,
        cohortAttritionRef = att,
        cohortCodelistRef = codes,
        .softValidation = TRUE
      )
  }

  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}

copySettings <- function(tab, n) {
  ids <- tab |> dplyr::pull("cohort_definition_id")
  numCohorts <- ids |> length()
  idmax <- max(ids)
  copies <- purrr::map(as.list(2:n-1), \(x){
    tab |>
      dplyr::mutate(
        original_cohort_id = .data$cohort_definition_id,
        original_cohort_name = .data$cohort_name,
        cohort_definition_id = idmax + 1:numCohorts + numCohorts*(x-1),
        cohort_name = paste0(.data$cohort_name, "_", x)
      )
  })
  set <- list(tab, copies) |> dplyr::bind_rows()
  return(set)
}

copyFromSet <- function(tab, set) {
  dplyr::union_all(
    tab,
    tab |>
      dplyr::rename("original_cohort_id" = "cohort_definition_id") |>
      dplyr::inner_join(
        set |>
          dplyr::select(c("cohort_definition_id", "original_cohort_id")),
        by = "original_cohort_id",
        relationship = "many-to-many",
        copy = TRUE
      ) |>
      dplyr::select(!"original_cohort_id")
  )
}
