#' Function to create a mock cdm reference for CohortConstructor
#'
#' @description
#' `mockCohortConstructor()` creates an example dataset that can be used for
#' demonstrating and testing the package
#'
#' @param source Source for the mock cdm, it can either be 'local' or 'duckdb'.
#' @param seed 	An optional integer used to set the seed for random number
#' generation, ensuring reproducibility of the generated data. If provided,
#' this seed allows the function to produce consistent results each time.
#' If 'NULL', the seed is not set, which can lead to different outputs on
#' each run.
#'
#' @return cdm object
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' cdm <- mockCohortConstructor()
#'
#' cdm
#' }
mockCohortConstructor <- function(source = "local", seed = 1) {
  rlang::check_installed("omock")
  checkGiBleed()
  omopgenerics::assertChoice(source, c("local", "duckdb"), length = 1)

  cdm <- omock::mockVocabularySet(vocabularySet = "GiBleed") |>
    omock::mockPerson(nPerson = 100, seed = seed) |>
    omock::mockObservationPeriod(seed = seed) |>
    omock::mockDrugExposure(seed = seed) |>
    omock::mockConditionOccurrence(seed = seed) |>
    omock::mockObservation(seed = seed) |>
    omock::mockMeasurement(seed = seed) |>
    omock::mockDeath(recordPerson = 0.1, seed = seed) |>
    omock::mockCohort(name = "cohort1", seed = seed) |>
    omock::mockCohort(name = "cohort2", numberCohorts = 2, seed = seed)

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

checkGiBleed <- function() {
  if (!omock::isMockDatasetDownloaded("GiBleed")) {
    # Non interactive --> download
    if (!interactive()) {
      omock::downloadMockDataset("GiBleed")
      return(invisible(TRUE))
    }

    # Interactive --> ask user
    cli::cli_inform("Synthetic GiBleed dataset not found. Download now?")
    download_now <- utils::menu(c("Yes", "No"))

    if (download_now == 1) {
      omock::downloadMockDataset("GiBleed")
      cli::cli_alert_success("GiBleed dataset downloaded.")
    } else {
      cli::cli_abort(c(
        "Synthetic GiBleed dataset must be downloaded.",
        i = "Run {.code omock::downloadMockDataset('GiBleed')}."
      ))
    }
  }
}
