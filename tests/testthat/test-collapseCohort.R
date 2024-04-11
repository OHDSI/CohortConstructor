test_that("simple example", {
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTable(cohortTable = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1,
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1,
      "concept_name" = "my concept",
      "domain_id" = "drUg",
      "vocabulary_id" = NA,
      "concept_class_id" = NA,
      "concept_code" = NA,
      "valid_start_date" = NA,
      "valid_end_date" = NA
    )
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = 1:11,
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1800, 1801, 1802, 1803),
      "drug_type_concept_id" = 1
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2020-01-01")
      )
  )

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort"))

  expect_no_error(sameCohort <- cohort |> collapseCohort(gap = 0))
  expect_identical(sameCohort, cohort)

  expect_no_error(newCohort <- cohort |> collapseCohort(gap = 1))


})

