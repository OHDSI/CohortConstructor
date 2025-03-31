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

  x <-  cdm$death %>%
    PatientProfiles::addInObservation(indexDate = "death_date") %>%
    dplyr::filter(.data$in_observation==1) %>%
    dplyr::select("subject_id" = "person_id",
                  "death_date")
  if (!is.null(subsetCohort)){
    if (!is.null(subsetCohortId)){
      x <- x %>%
        dplyr::inner_join(cdm[[subsetCohort]] %>%
                            dplyr::filter(.data$cohort_definition_id %in% subsetCohortId) %>%
                            dplyr::select("subject_id", "cohort_definition_id"),
                          by = c("subject_id")) %>%
        dplyr::select("subject_id", "death_date")

    }else{
      x <- x %>%
        dplyr::inner_join(cdm[[subsetCohort]] %>%
                            dplyr::select("subject_id"),
                          by = c("subject_id")) %>%
        dplyr::select("subject_id", "death_date")
    }
  }

  cohortRef <- x %>%
    dplyr::group_by(.data$subject_id) %>%
    dbplyr::window_order(.data$death_date) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::rename("cohort_start_date" = "death_date") %>%
    dplyr::mutate(cohort_definition_id = 1L ,
                  cohort_end_date = .data$cohort_start_date)  %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::compute(
      name = name,
      temporary = FALSE,
      overwrite = TRUE)

  attr(cohortRef, "tbl_name") <- name

  if (is.null(subsetCohort)) {
    subsetCohort <- as.character(NA)
  }
  if (is.null(subsetCohortId)) {
    subsetCohortId <- as.numeric(NA)
  }
  cohortSetRef <- dplyr::tibble(
    "cohort_definition_id" = 1L,
    "cohort_name" = "death_cohort",
    "subset_cohort_table" = subsetCohort,
    "subset_cohort_id" = subsetCohortId
  )

  cdm[[name]] <- cohortRef %>%
    omopgenerics::newCohortTable(cohortSetRef = cohortSetRef)

  return(cdm[[name]])
}
