
#' Utility function to change the name of a cohort.
#'
#' @inheritParams cohortDoc
#' @param newCohortName Character vector with same
#' @inheritParams cohortIdModifyDoc
#'
#' @return A cohort_table object.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#' if(isTRUE(omock::isMockDatasetDownloaded("GiBleed"))){
#' cdm <- mockCohortConstructor()
#'
#' settings(cdm$cohort1)
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   renameCohort(newCohortName = "new_name")
#'
#' settings(cdm$cohort1)
#' }
#' }
renameCohort <- function(cohort,
                         newCohortName,
                         cohortId = NULL) {
  # check input
  cohort <- omopgenerics::validateCohortArgument(cohort = cohort)
  cohortId <- omopgenerics::validateCohortIdArgument(
    cohortId = cohortId, cohort = cohort
  )
  omopgenerics::assertCharacter(
    newCohortName, unique = TRUE, minNumCharacter = 1
  )

  if(length(newCohortName) == 1 &&
     stringr::str_detect(string = newCohortName,
                         pattern = "\\{cohort_name\\}")){
    originalNames <- omopgenerics::settings(cohort) |>
      dplyr::pull("cohort_name")
    newCohortName <- stringr::str_replace_all(
      string = newCohortName,
      pattern = "\\{cohort_name\\}",
      replacement = originalNames
    )
  }

  if (length(cohortId) != length(newCohortName)) {
    cli::cli_abort(c(x = "`cohortId` and `newCohortName` must have the same length."))
  }

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
    omopgenerics::newCohortTable(cohortSetRef = set, .softValidation = TRUE)
}
