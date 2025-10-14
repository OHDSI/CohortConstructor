#' Function to create a mock cdm reference for CohortConstructor
#'
#' @description
#' `mockCohortConstructor()` creates an example dataset that can be used for
#' demonstrating and testing the package
#'
#' @param source Source for the mock cdm, it can either be 'local' or 'duckdb'.
#'
#' @return cdm object
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#'
#' cdm
#' }
#' }
mockCohortConstructor <- function(source = "local") {
  rlang::check_installed("omock")
  if(isFALSE(omock::isMockDatasetDownloaded("GiBleed"))){
    cli::cli_abort(c("Synthetic GiBleed dataset must already be downloaded to use mockCohortConstructor()",
                     i = "Use {.code omock::downloadMockDataset(datasetName = 'GiBleed')} to download"))
  }
  omopgenerics::assertChoice(source, c("local", "duckdb"), length = 1)

  cdm <- omock::mockVocabularySet(vocabularySet = "GiBleed") |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockDrugExposure() |>
    omock::mockConditionOccurrence() |>
    omock::mockObservation() |>
    omock::mockMeasurement() |>
    omock::mockDeath(recordPerson = 0.1) |>
    omock::mockCohort(name = "cohort1") |>
    omock::mockCohort(name = "cohort2", numberCohorts = 2)

  cdm$cohort1 <- cdm$cohort1 |>
    dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
    dplyr::filter(.data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::compute(name = "cohort1") |>
    omopgenerics::newCohortTable(cohortAttritionRef = NULL, .softValidation = TRUE)

  if (source == "duckdb") {
    rlang::check_installed("duckdb")
    rlang::check_installed("CDMConnector")
    con <- duckdb::dbConnect(drv = duckdb::duckdb())
    src <- CDMConnector::dbSource(con = con, writeSchema = "main")
    cdm <- omopgenerics::insertCdmTo(cdm = cdm, to = src)
  }

  return(cdm)
}
