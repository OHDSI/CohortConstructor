#' Restrict cohort to first entry by index date
#'
#' @param cohort A cohort table in a cdm reference
#' @param indexDate indexDate Variable in cohort that contains the date to
#' restrict on
#' @return a cohort table in a cdm reference
#' @export
#'
#'
restrictToFirstEntry <- function(cohort,
                                 indexDate = "cohort_start_date"){

  cdm <- attr(cohort, "cdm_reference")
  #validate input
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cohort must be part of a cdm reference")
  }

  if(!"GeneratedCohortSet" %in% class(cohort) ||
     !all(c("cohort_definition_id", "subject_id",
            "cohort_start_date", "cohort_end_date") %in%
          colnames(cohort))){
    cli::cli_abort("cohort must be a GeneratedCohortSet")
  }

  if(!indexDate %in% colnames(cohort)){
    cli::cli_abort("indexDate must be a date column in the cohort table")
  }

  #restrict to first entry
  indexDateSym <- rlang::sym(indexDate)

  cohort <- cohort |> dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
    dplyr::filter(!!indexDateSym == min(!!indexDateSym, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    CDMConnector::recordCohortAttrition("restrict to first entry")

  return(cohort)

}
