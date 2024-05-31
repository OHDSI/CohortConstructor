test_that("simple example", {
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
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

  cdm <- cdm |> copyCdm()

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort"))

  expect_no_error(sameCohort <- cohort |> collapseCohorts(gap = 0, name = "new_cohort"))
  expect_identical(settings(sameCohort), settings(cohort))
  expect_identical(cohortCount(sameCohort), cohortCount(cohort))
  expect_identical(
    attrition(sameCohort),
    attrition(cohort) |>
      dplyr::union_all(dplyr::tibble(
        "cohort_definition_id" = 1L,
        "number_records" = 7L,
        "number_subjects" = 2L,
        "reason_id" = 2L,
        "reason" = "Collapse cohort with a gap of 0 days.",
        "excluded_records" = 0L,
        "excluded_subjects" = 0L
      ))
  )
  expect_true(tableName(sameCohort) == "new_cohort")
  expect_identical(
    omopgenerics::tableSource(sameCohort), omopgenerics::tableSource(cohort)
  )

  expect_no_error(newCohort <- cohort |> collapseCohorts(gap = 1, name = "my_cohort"))
  expect_identical(settings(newCohort), settings(cohort))
  expect_identical(cohortCount(newCohort), dplyr::tibble(
    "cohort_definition_id" = 1L, "number_records" = 4L, "number_subjects" = 2L
  ))
  expect_identical(
    attrition(newCohort),
    attrition(cohort) |>
      dplyr::union_all(dplyr::tibble(
        "cohort_definition_id" = 1L,
        "number_records" = 4L,
        "number_subjects" = 2L,
        "reason_id" = 2L,
        "reason" = "Collapse cohort with a gap of 1 days.",
        "excluded_records" = 3L,
        "excluded_subjects" = 0L
      ))
  )
  expect_true(tableName(newCohort) == "my_cohort")
  expect_identical(
    omopgenerics::tableSource(newCohort), omopgenerics::tableSource(cohort)
  )

  # expected behaviour
  cdm$cohort <- cdm$cohort |> dplyr::mutate(extra_column = 1)
  expect_message(cdm$cohort |> collapseCohorts())
  expect_error(cdm$cohort |> collapseCohorts(gap = Inf))
  expect_error(cdm$cohort |> collapseCohorts(gap = NA))
  expect_error(cdm$cohort |> collapseCohorts(gap = NULL))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("out of observation", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
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

  cdm <- cdm |> copyCdm()

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort"))

  expect_no_error(sameCohort <- cohort |> collapseCohorts(gap = 0, name = "new_cohort"))
  expect_identical(settings(sameCohort), settings(cohort))
  expect_identical(cohortCount(sameCohort), cohortCount(cohort))
  expect_identical(
    attrition(sameCohort),
    attrition(cohort) |>
      dplyr::union_all(dplyr::tibble(
        "cohort_definition_id" = 1L,
        "number_records" = 7L,
        "number_subjects" = 2L,
        "reason_id" = 2L,
        "reason" = "Collapse cohort with a gap of 0 days.",
        "excluded_records" = 0L,
        "excluded_subjects" = 0L
      ))
  )
  expect_true(tableName(sameCohort) == "new_cohort")
  expect_identical(
    omopgenerics::tableSource(sameCohort), omopgenerics::tableSource(cohort)
  )

  expect_no_error(newCohort <- cohort |> collapseCohorts(gap = 1, name = "my_cohort"))
  expect_identical(settings(newCohort), settings(cohort))
  expect_identical(cohortCount(newCohort), dplyr::tibble(
    "cohort_definition_id" = 1L, "number_records" = 4L, "number_subjects" = 2L
  ))
  expect_identical(
    attrition(newCohort),
    attrition(cohort) |>
      dplyr::union_all(dplyr::tibble(
        "cohort_definition_id" = 1L,
        "number_records" = 4L,
        "number_subjects" = 2L,
        "reason_id" = 2L,
        "reason" = "Collapse cohort with a gap of 1 days.",
        "excluded_records" = 3L,
        "excluded_subjects" = 0L
      ))
  )
  expect_true(tableName(newCohort) == "my_cohort")
  expect_identical(
    omopgenerics::tableSource(newCohort), omopgenerics::tableSource(cohort)
  )

  PatientProfiles::mockDisconnect(cdm)
})

