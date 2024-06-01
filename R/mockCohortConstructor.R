#' Function to create a mock cdm reference for CohortConstructor
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
#' permisions.
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


  if (is.null(tables)) {
    cdm <- omock::mockCdmReference() |>
      omock::mockVocabularyTables(concept = conceptTable) |>
      omock::mockPerson(nPerson = nPerson) |>
      omock::mockObservationPeriod() |>
      omock::mockCohort(name = "cohort1") |>
      omock::mockCohort(name = "cohort2", numberCohorts = 2)
  } else {
    cdm <- omock::mockCdmFromTables(tables = tables,
                                    seed = seed) |>
      omock::mockVocabularyTables(concept = conceptTable)
  }

  if (!is.null(conceptIdClass) & !is.null(conceptId)) {
    cdm <- cdm |> omock::mockConcepts(conceptSet = conceptId, domain = conceptIdClass)
  }

  if (drugExposure) {
    cdm <- cdm |> omock::mockDrugExposure()
  }

  if (conditionOccurrence) {
    cdm <- cdm |> omock::mockConditionOccurrence()
  }

  if (death) {
    cdm <- cdm |> omock::mockDeath()
  }

  if (measurement) {
    cdm <- cdm |> omock::mockMeasurement()
  }

  if (!is.null(otherTables)) {
    cdm <- cdm |> omopgenerics::insertTable(name = names(otherTables), table = otherTables[[1]])
  }

  if (!is.null(con)) {
    cdm <- CDMConnector::copyCdmTo(con = con, cdm = cdm, schema = writeSchema, overwrite = TRUE)
  }

  return(cdm)
}
