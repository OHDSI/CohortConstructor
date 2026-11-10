
#' Create a cohort definition from R code
#'
#' @description
#' `cohortDefinitionFromCode()` parses a cohort-construction assignment and
#' stores its pipeline as structured function calls. Only literal values and
#' named codelist references are parsed; the supplied code is not evaluated.
#'
#' @param x A single character string, expression, or call containing an
#'   assignment such as `cdm$cohort <- conceptCohort(...) |> requireSex(...)`.
#'
#' @return A `cohort_definition` object.
#' @export
#'
#' @examples
#' code <- paste0(
#'   'cdm$cohort <- conceptCohort(cdm = cdm, name = "cohort", ',
#'   'conceptSet = codelist["condition"])'
#' )
#' cohortDefinitionFromCode(code)
cohortDefinitionFromCode <- function(x, name = NULL) {
  expression <- parseCohortDefinitionCode(x)

  if (!rlang::is_call(expression, "<-", n = 2)) {
    cli::cli_abort("Code must contain an assignment of the form `cdm$cohort_name <- ...`.")
  }

  cohortName <- cohortNameFromAssignment(expression[[2]])
  steps <- stepsFromPipeline(expression[[3]])
  definition <- stats::setNames(
    list(list(name = cohortName, definition = steps)),
    cohortName
  )

  newCohortDefinition(definition)
}
