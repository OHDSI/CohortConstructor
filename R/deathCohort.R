#' Create cohort based on the death table
#'
#' @inheritParams cdmDoc
#' @inheritParams nameDoc
#' @param subsetCohort  A character refering to a cohort table containing
#' individuals for whom cohorts will be generated. Only individuals in this
#' table will appear in the generated cohort.
#' @param subsetCohortId Optional. Specifies cohort IDs from the `subsetCohort`
#' table to include. If none are provided, all cohorts from the `subsetCohort`
#' are included.
#'
#' @return A cohort table with a death cohort in cdm
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor()
#'
#' # Generate a death cohort
#' death_cohort <- deathCohort(cdm, name = "death_cohort")
#' death_cohort
#'
#' # Create a demographics cohort with age range and sex filters
#' cdm$my_cohort <- demographicsCohort(cdm, "my_cohort", ageRange = c(50,100), sex = "Female")
#' # Generate a death cohort, restricted to individuals in 'my_cohort'
#' death_cohort <- deathCohort(cdm, name = "death_cohort", subsetCohort = "my_cohort")
#' death_cohort |> attrition()
#' }
deathCohort <- function(
    cdm,
    name,
    subsetCohort = NULL,
    subsetCohortId = NULL){

  name <- omopgenerics::validateNameArgument(name, validation = "warning")
  cdm <- omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertCharacter(subsetCohort, length = 1, null = TRUE)
  if (!is.null(subsetCohort)) {
    omopgenerics::validateCohortArgument(cdm[[subsetCohort]])
    omopgenerics::validateCohortIdArgument(subsetCohortId,
                                           cdm[[subsetCohort]],
                                           validation = "error")
  }

  if (is.null(subsetCohort)) {
    subsetCohort <- as.character(NA)
  }
  if (is.null(subsetCohortId)) {
    subsetCohortId <- as.numeric(NA)
  }

  useIndexes <- getOption("CohortConstructor.use_indexes")

  cohortSetRef <- dplyr::tibble(
    "cohort_definition_id" = 1L,
    "cohort_name" = "death_cohort",
    "subset_cohort_table" = subsetCohort,
    "subset_cohort_id" = subsetCohortId
  )

  cdm[[name]] <-  cdm$death |>
    dplyr::mutate(cohort_definition_id = 1L) |>
    dplyr::select("cohort_definition_id",
                  "subject_id" = "person_id",
                  "cohort_start_date" = "death_date",
                  "cohort_end_date" ="death_date") |>
    dplyr::compute(temporary = FALSE, name = name)

  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(cohortSetRef = cohortSetRef,
                                 .softValidation = TRUE)

  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  cli::cli_inform(c("i" = "Applying cohort requirements."))
  cdm[[name]] <- fulfillCohortReqs(cdm, name,
                                   useRecordsBeforeObservation = FALSE,
                                   type = "start",
                                   useIndexes)

  if (!is.na(subsetCohort)){
    if (!is.na(subsetCohortId)){
      cdm[[name]] <- cdm[[name]] |>
        dplyr::inner_join(cdm[[subsetCohort]] |>
                            dplyr::filter(.data$cohort_definition_id %in% subsetCohortId) |>
                            dplyr::select("subject_id"),
                          by = c("subject_id")) |>
        dplyr::compute(
          name = name,
          temporary = FALSE,
          overwrite = TRUE) |>
        omopgenerics::recordCohortAttrition("In subset cohort")
    }else{
      cdm[[name]] <- cdm[[name]] |>
        dplyr::inner_join(cdm[[subsetCohort]] |>
                            dplyr::select("subject_id"),
                          by = c("subject_id")) |>
        dplyr::compute(
          name = name,
          temporary = FALSE,
          overwrite = TRUE) |>
        omopgenerics::recordCohortAttrition("In subset cohort")
    }
  }

  cdm[[name]] <- cdm[[name]] |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::filter(dplyr::row_number()==1) |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(
      name = name,
      temporary = FALSE,
      overwrite = TRUE) |>
    omopgenerics::recordCohortAttrition("First death record")

  cdm[[name]] <- omopgenerics::newCohortTable(table = cdm[[name]])
  cli::cli_inform(c("v" = "Cohort {.strong {name}} created."))

  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = cdm[[name]],
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(cdm[[name]])
}
