generateAgeCohortSet <- function(cdm,
                                 name,
                                 ageGroup = list(c(0, 19), c(20, Inf)),
                                 targetCohortTable = NULL,
                                 targetCohortId = NULL,
                                 overwrite = FALSE) {
  ages <- unlist(ageGroup, recursive = TRUE)
  ageBreak <- ages + rep(c(-1, 0), length(ages)/2)
  ageBreak <- ifelse(ageBreak < 0, 0, ageBreak) |> unique()

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
