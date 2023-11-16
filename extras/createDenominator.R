generateAgeCohortSet <- function(cdm,
                                 name,
                                 ageGroup = list(c(0, 150)),
                                 targetCohortTable = NULL,
                                 targetCohortId = NULL,
                                 overwrite = FALSE) {
  ages <- unlist(ageGroup, recursive = TRUE)
  ageStart <- ages[(num %% 2) == 0]
  ageStart <- ages[(num %% 2) == 1]
  ageMin <- min(unlist(ageGroup, recursive = TRUE))
  ageMax <- max(unlist(ageGroup, recursive = TRUE))
  if (!is.null(targetCohortTable)) {
    x <- cdm[["observation_period"]] |>
      dplyr::select(
        "subject_id" = "person_id",
        "cohort_start_date" = "observation_period_start_date",
        "cohort_end_date" = "observation_period_end_date"
      ) |>
      dplyr::mutate("cohort_definition_id" = 1)
  } else {
    x <- cdm[[targetCohortTable]]
    if (!is.null(targetCohortId)) {
      x <- x |>
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
    }
  }
  x <- x |>
    PatientProfiles::addDateOfBirth()

  x <- x |>
    PatientProfiles::addAge(ageGroup = ageGroup)

}
