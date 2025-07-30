test_that("simple example", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("original_cohort"),
                      numberCohorts = 2, seed = 1)
  cdm <- cdm_local |> copyCdm()

  start_settings <- omopgenerics::settings(cdm$original_cohort)
  start_attrition <- omopgenerics::attrition(cdm$original_cohort)

  cdm$copy_cohort <- copyCohorts(cdm$original_cohort,
                                 n = 1,
                                 name = "copy_cohort")
  expect_identical(omopgenerics::settings(cdm$copy_cohort),
                   start_settings)
  expect_identical(omopgenerics::attrition(cdm$copy_cohort),
                   start_attrition)

  # attrition for original cohort table should be unaffected by applying filter to copy cohort
  cdm$copy_cohort <- cdm$copy_cohort |>
    dplyr::filter(subject_id == 99) |>
    omopgenerics::recordCohortAttrition("filter")
  expect_identical(omopgenerics::attrition(cdm$original_cohort),
                   start_attrition)
  # original cohort table should be unaffected by applying filter to copy cohort
  cdm$copy_cohort <- cdm$copy_cohort |>
    requireAge(c(0,80))
  expect_identical(omopgenerics::settings(cdm$original_cohort),
                   start_settings)

  # warning if the new cohort table already exists
  expect_warning(cdm$copy_cohort <- copyCohorts(cdm$original_cohort,
                                                n = 1,
                                                name = "copy_cohort"))

  # expected errors
  expect_error(cdm$copy_cohort <- copyCohorts(cdm$original_cohort,
                                              n = 1,
                                              name = "another_name"))
  expect_error(cdm$copy_cohort <- copyCohorts("a",
                                              n = 1,
                                              name = "copy_cohort"))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)

})

test_that("copy only specific cohort IDs", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("original_cohort"),
                      numberCohorts = 2, seed = 1)
  cdm <- cdm_local |> copyCdm()

  start_settings <- omopgenerics::settings(cdm$original_cohort)
  start_attrition <- omopgenerics::attrition(cdm$original_cohort)

  # keep just one cohort
  cdm$copy_cohort_a <- copyCohorts(cdm$original_cohort,
                                   cohortId = 1,
                                   n = 1,
                                   name = "copy_cohort_a")
  cdm$copy_cohort_b <- copyCohorts(cdm$original_cohort,
                                   n = 1,
                                   cohortId = "cohort_1",
                                   name = "copy_cohort_b")
  expect_identical(
    omopgenerics::settings(cdm$original_cohort) |>
      dplyr::filter(cohort_definition_id == 1),
    omopgenerics::settings(cdm$copy_cohort_a))
  expect_identical(
    omopgenerics::settings(cdm$original_cohort) |>
      dplyr::filter(cohort_name == "cohort_1"),
    omopgenerics::settings(cdm$copy_cohort_b))

  # keep both cohorts
  cdm$copy_cohort_c <- copyCohorts(cdm$original_cohort,
                                   n = 1,
                                   cohortId = c(1, 2),
                                   name = "copy_cohort_c")
  cdm$copy_cohort_d <- copyCohorts(cdm$original_cohort,
                                   n = 1,
                                   cohortId = c("cohort_1", "cohort_2"),
                                   name = "copy_cohort_d")

  # cohort not present
  expect_error(cdm$copy_cohort <- copyCohorts(cdm$original_cohort,
                                              n = 1,
                                              cohortId = c(3),
                                              name = "copy_cohort"))
  expect_error(cdm$copy_cohort <- copyCohorts(cdm$original_cohort,
                                              n = 1,
                                              cohortId = "not_a_cohort",
                                              name = "copy_cohort"))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("multiple copies", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("original_cohort"),
                      numberCohorts = 3, seed = 1)
  cdm <- cdm_local |> copyCdm()

  cdm$copy_cohort <- copyCohorts(
    cdm$original_cohort, n = 4, cohortId = 1, name = "copy_cohort"
  )
  expect_equal(collectCohort(cdm$copy_cohort, 1), collectCohort(cdm$copy_cohort, 2))
  expect_equal(collectCohort(cdm$copy_cohort, 1), collectCohort(cdm$copy_cohort, 3))
  expect_equal(collectCohort(cdm$copy_cohort, 1), collectCohort(cdm$copy_cohort, 4))

  cdm$copy_cohort2 <- copyCohorts(
    cdm$original_cohort, n = 3, cohortId = c(1, 3), name = "copy_cohort2"
  )
  expect_equal(collectCohort(cdm$copy_cohort2, 1), collectCohort(cdm$copy_cohort2, 4))
  expect_equal(collectCohort(cdm$copy_cohort2, 1), collectCohort(cdm$copy_cohort2, 6))
  expect_equal(collectCohort(cdm$copy_cohort2, 3), collectCohort(cdm$copy_cohort2, 5))
  expect_equal(collectCohort(cdm$copy_cohort2, 3), collectCohort(cdm$copy_cohort2, 7))
  expect_equal(
    settings(cdm$copy_cohort2),
    dplyr::tibble(
      cohort_definition_id = c(1, 3, 4:7),
      cohort_name = c("cohort_1", "cohort_3", "cohort_1_1", "cohort_3_1", "cohort_1_2", "cohort_3_2"),
      original_cohort_id = c(NA, NA, 1, 3, 1, 3),
      original_cohort_name = c(NA, NA, "cohort_1", "cohort_3", "cohort_1", "cohort_3")
    )
  )
  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

