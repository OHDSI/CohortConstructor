

#' Function to create a mock cdm reference for CohortConstructor
#'
#' @param nPerson number of person in the cdm
#' @param conceptTable user defined concept table
#' @param tables list of tables to include in the cdm
#' @param conceptId list of concept id
#' @param conceptIdClass the domain class of the conceptId
#' @param drugExposure T/F include drug exposure table in the cdm
#' @param conditionOccurrence T/F include condition occurrence in the cdm
#' @param death T/F include death table in the cdm
#' @param con db connection detail for copy to databases
#'
#' @return cdm object
#' @export
#'
#' @examples
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor()
#'
#' cdm
#'
mockCohortConstructor <- function(nPerson = 10,
                                  conceptTable = NULL,
                                  tables = NULL,
                                  conceptId = NULL,
                                  conceptIdClass = NULL,
                                  drugExposure = F,
                                  conditionOccurrence = F,
                                  death = F,
                                  con = DBI::dbConnect(duckdb::duckdb())) {


  if (is.null(tables)) {
    cdm <-
      omock::mockCdmReference() |> omock::mockVocabularyTables(concept = conceptTable) |>
      omock::mockPerson(nPerson = nPerson) |> omock::mockObservationPeriod() |> omock::mockCohort(name = "cohort1") |>
      omock::mockCohort(name = "cohort2", numberCohorts = 2)
  } else {
    cdm <-
      omock::mockCdmFromTables(tables = tables) |>
      omock::mockVocabularyTables(concept = conceptTable)

  }


  if(!is.null(conceptIdClass) & !is.null(conceptId)){

    cdm <- cdm |> omock::addConcept(conceptSet = conceptId, domain = conceptIdClass)

  }

  if(drugExposure == T){

    cdm <- cdm |> omock::mockDrugExposure()
  }

  if(conditionOccurrence == T){
    cdm <- cdm |> omock::mockConditionOccurrence()
  }

  if(death == T){
    cdm <- cdm |> omock::mockDeath()
  }


  if (!is.null(con)){
  cdm <- CDMConnector::copyCdmTo(con = con, cdm = cdm, schema = "main")
}

  return(cdm)



}


#
# library(omock)
#
#
# cdm <- mockCohortConstructor(nPerson = 100)
#
#
# cdm$cohort3 <- intersectCohorts(
# cohort = cdm$cohort2,
# name = "cohort3",
# )
#
# cdm$year_restricted <- cdm$cohort |>
# yearCohorts(years = 2010:2019, name = "year_restricted")
# cdm$year_restricted
# cdm$year_restricted |> settings()
# cdm$year_restricted |> attrition()