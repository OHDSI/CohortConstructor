
#' Utility function to change the name of a cohort.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdModifyDoc
#' @param newCohortName Character vector with same
#' @inheritParams softValidationDoc
#'
#' @return A cohort_table object.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor(nPerson = 100)
#'
#' settings(cdm$cohort1)
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   renameCohort(cohortId = 1, newCohortName = "new_name")
#'
#' settings(cdm$cohort1)
#' }
renameCohort <- function(cohort,
                         cohortId,
                         newCohortName,
                         .softValidation = TRUE) {
  # check input
  cohort <- omopgenerics::validateCohortArgument(cohort = cohort)
  cohortId <- omopgenerics::validateCohortIdArgument(
    cohortId = cohortId, cohort = cohort, null = FALSE
  )
  omopgenerics::assertCharacter(
    newCohortName, unique = TRUE, minNumCharacter = 1
  )
  if (length(cohortId) != length(newCohortName)) {
    cli::cli_abort(c(x = "`cohortId` and `newCohortName` must have the same length."))
  }
  omopgenerics::assertLogical(.softValidation)

  # new settings
  set <- omopgenerics::settings(cohort)
  id <- omopgenerics::uniqueId(exclude = colnames(set))
  newNames <- list(cohortId, newCohortName) |>
    rlang::set_names(c("cohort_definition_id", id)) |>
    dplyr::as_tibble()
  set <- set |>
    dplyr::left_join(newNames, by = "cohort_definition_id") |>
    dplyr::mutate(cohort_name = dplyr::if_else(
      is.na(.data[[id]]), .data$cohort_name, .data[[id]]
    )) |>
    dplyr::select(!dplyr::all_of(id))

  # update cohort attributes
  cohort |>
    omopgenerics::newCohortTable(cohortSetRef = set, .softValidation = .softValidation) # validate new names
}
