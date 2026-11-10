#' Generate R code from a cohort definition
#'
#' @description
#' `codeFromCohortDefinition()` converts structured cohort definitions into
#' executable R pipeline code.
#'
#'
#' @return A character string containing one assignment per cohort definition.
#' @export
#'
#' @examples
#' code <- 'cdm$cohort <- conceptCohort(cdm = cdm, name = "cohort", conceptSet = codelist["condition"])'
#' definition <- cohortDefinitionFromCode(code)
#' codeFromCohortDefinition(definition)
codeFromCohortDefinition <- function(x) {
  x <- validateCohortDefinition(x)
  purrr::map_chr(x, codeFromSingleCohortDefinition) |>
    paste(collapse = "\n\n")
}
