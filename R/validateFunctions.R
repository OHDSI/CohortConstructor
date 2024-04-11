validateCDM <- function(cdm) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cohort must be part of a cdm reference")
  }
  return(invisible(cdm))
}

validateCdm <- function(cdm) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a cdm_reference object.")
  }
  return(invisible(cdm))
}

validateCohortTable <- function(cohort) {
  if(!"cohort_table" %in% class(cohort) ||
     !all(c("cohort_definition_id", "subject_id",
            "cohort_start_date", "cohort_end_date") %in%
          colnames(cohort))){
    cli::cli_abort("cohort must be a `cohort_table`")
  }
}

validateIndexDate <- function(indexDate, cohort) {
  assertCharacter(indexDate)
  if(!indexDate %in% colnames(cohort)){
    cli::cli_abort("{indexDate} must be a date column in the cohort table")
  }
}

validateCohortId <- function(cohortId, ids) {
  assertNumeric(cohortId, null = TRUE, integerish = TRUE)
  if (is.null(cohortId)) {
    cohortId <- ids
  } else {
    indNot <- !cohortId %in% ids
    if (sum(indNot)>0) {
      cli::cli_warn("{paste0(cohortId[indNot], collapse = ', ')} {?is/are} not in the cohort table and won't be used.")
      cohortId <- cohortId[!indNot]
    }
  }
  return(cohortId)
}

validateDateRange<-function(dateRange){
  if(!"Date" %in% class(dateRange)){
    cli::cli_abort("dateRange is not a date")
  }
  if(length(dateRange) != 2){
    cli::cli_abort("dateRange must be length two")
  }
  if(dateRange[1]>dateRange[2]){
    cli::cli_abort("First date in dateRange cannot be after second")
  }
  return(invisible(dateRange))
}

validateName <- function(name) {
  em <- c(
    "x" = "{name} it is not a valida value for name.",
    "i" = "It must be:",
    "*" = "lowercase character vector of length 1",
    "*" = "NA or NULL values are not allowed"
  )
  if (!is.character(name) | length(name) != 1 | is.na(name)) {
    cli::cli_abort(em)
  }
  if (tolower(name) != name){
    cli::cli_abort(em)
  }
  return(invisible(name))
}

validateConceptSet <- function(conceptSet) {
  omopgenerics::newCodelist(conceptSet)
}
