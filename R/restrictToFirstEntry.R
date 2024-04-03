#' Restrict cohort to first entry by index date
#'
#' @param cohort A cohort table in a cdm reference
#' @param cohortId Vector of cohort definition ids to include. If NULL, all
#' cohort definition ids will be used.
#' @param indexDate indexDate variable in cohort that contains the date to
#' restrict on
#' @return A cohort table in a cdm reference
#' @export
#'
#'#' @examples
#' \donttest{
#' library(CohortConstructor)
#' library(omock)
#' cdm <- mockCdmReference() |>
#'   mockPerson(nPerson = 2) |>
#'   mockObservationPeriod() |>
#'   mockCohort(recordPerson = 2)
#' cdm <- restrictToFirstEntry(cdm$cohort)
#' }
#'
restrictToFirstEntry <- function(cohort,
                                 cohortId = NULL,
                                 indexDate = "cohort_start_date"){

  # checks
  assertCharacter(indexDate)
  assertNumeric(cohortId, null = TRUE, integerish = TRUE)

  # validate input
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cohort must be part of a cdm reference")
  }
  cdm <- attr(cohort, "cdm_reference")
  name <- attr(cohort, "tbl_name")

  if(!"GeneratedCohortSet" %in% class(cohort) ||
     !all(c("cohort_definition_id", "subject_id",
            "cohort_start_date", "cohort_end_date") %in%
          colnames(cohort))){
    cli::cli_abort("cohort must be a GeneratedCohortSet")
  }

  if(!indexDate %in% colnames(cohort)){
    cli::cli_abort("indexDate must be a date column in the cohort table")
  }

  ids <- omopgenerics::settings(cdm[[name]])$cohort_definition_id
  if (is.null(cohortId)) {
    cohortId <- ids
  } else {
    indNot <- which(!cohortId %in% ids)
    if (length(indNot)>0) {
      cli::cli_warn("{paste0(cohortId[indNot], collapse = ', ')} {?is/are} not in the cohort table.")
      cohortId <- cohortId[!indNot]
    }
  }

  # restrict to first entry
  indexDateSym <- rlang::sym(indexDate)

  # TO DO : if cohort id son tots no fer filtres
  cdm[[name]] <- cdm[[name]] |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
    dplyr::filter(!!indexDateSym == min(!!indexDateSym, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::union_all(
      cdm[[name]] |>
        dplyr::filter(!.data$cohort_definition_id %in% .env$cohortId)
    ) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    CDMConnector::recordCohortAttrition("Restricted to first entry", cohortId = cohortId)

  return(cdm)
}
