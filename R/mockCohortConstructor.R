#' Function to create a mock cdm reference for CohortConstructor
#'
#' @description
#' `mockCohortConstructor()` creates an example dataset that can be used for
#' demonstrating and testing the package
#'
#' @param nPerson number of person in the cdm
#' @param conceptTable user defined concept table
#' @param tables list of tables to include in the cdm
#' @param conceptId list of concept id
#' @param conceptIdClass the domain class of the conceptId
#' @param drugExposure T/F include drug exposure table in the cdm
#' @param conditionOccurrence T/F include condition occurrence in the cdm
#' @param measurement T/F include measurement in the cdm
#' @param death T/F include death table in the cdm
#' @param otherTables it takes a list of single tibble with names to include other tables in the cdm
#' @param con A DBI connection to create the cdm mock object.
#' @param writeSchema Name of an schema on the same connection with writing
#' permissions.
#' @param seed Seed passed to omock::mockCdmFromTable
#'
#' @return cdm object
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor()
#'
#' cdm
#' }
mockCohortConstructor <- function(nPerson = 10,
                                  conceptTable = NULL,
                                  tables = NULL,
                                  conceptId = NULL,
                                  conceptIdClass = NULL,
                                  drugExposure = FALSE,
                                  conditionOccurrence = FALSE,
                                  measurement = FALSE,
                                  death = FALSE,
                                  otherTables = NULL,
                                  con = DBI::dbConnect(duckdb::duckdb()),
                                  writeSchema = "main",
                                  seed = 123) {

  rlang::check_installed("omock")
  rlang::check_installed("duckdb")

  if (is.null(tables)) {
    cdm <- omock::mockCdmReference() |>
      omock::mockVocabularyTables(concept = conceptTable) |>
      omock::mockPerson(nPerson = nPerson,seed = seed) |>
      omock::mockObservationPeriod(seed = seed) |>
      omock::mockCohort(name = "cohort1", seed = seed) |>
      omock::mockCohort(name = "cohort2", numberCohorts = 2, seed = seed)
  } else {
    cdm <- omock::mockCdmFromTables(tables = tables, seed = seed) |>
      omock::mockVocabularyTables(concept = conceptTable)
  }

  if (!is.null(conceptIdClass) && !is.null(conceptId)) {
    cdm <- cdm |> omock::mockConcepts(conceptSet = conceptId, domain = conceptIdClass, seed = seed)
  }

  if (drugExposure) {
    cdm <- cdm |> omock::mockDrugExposure(seed = seed)
  }

  if (conditionOccurrence) {
    cdm <- cdm |> omock::mockConditionOccurrence(seed = seed)
  }

  if (death) {
    cdm <- cdm |> omock::mockDeath(seed = seed)
  }

  if (measurement) {
    cdm <- cdm |> omock::mockMeasurement(seed = seed)
  }

  if (!is.null(otherTables)) {
    cdm <- cdm |> omopgenerics::insertTable(name = names(otherTables), table = otherTables[[1]])
  }

  cdm$person <- cdm$person |>
    dplyr::mutate(
      gender_concept_id = as.integer(.data$gender_concept_id),
      year_of_birth = as.integer(.data$year_of_birth),
      month_of_birth = as.integer(.data$month_of_birth),
      day_of_birth = as.integer(.data$day_of_birth),
      race_concept_id = as.integer(.data$race_concept_id),
      ethnicity_concept_id = as.integer(.data$ethnicity_concept_id)
    )

  cdm$observation_period  <- cdm$observation_period  |>
    dplyr::mutate(period_type_concept_id = as.integer(.data$period_type_concept_id))



  if (!is.null(con)) {
    cdm <- CDMConnector::copyCdmTo(
      con = con,
      cdm = cdm,
      schema = writeSchema,
      overwrite = TRUE
    )
  }

  return(cdm)
}
