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
                                name = "copy_cohort"))

 # expected errors
 expect_error(cdm$copy_cohort <- copyCohorts(cdm$original_cohort,
                                            name = "another_name"))
 expect_error(cdm$copy_cohort <- copyCohorts("a",
                                            name = "copy_cohort"))
 # not allowed to copy cohort to the same location
 expect_error(cdm$original_cohort <- copyCohorts(cdm$original_cohort,
                                                name = "original_cohort"))

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
                                name = "copy_cohort_a")
  cdm$copy_cohort_b <- copyCohorts(cdm$original_cohort,
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
                                cohortId = c(1, 2),
                                name = "copy_cohort_c")
  cdm$copy_cohort_d <- copyCohorts(cdm$original_cohort,
                                cohortId = c("cohort_1", "cohort_2"),
                                name = "copy_cohort_d")

  # cohort not present
  expect_error(cdm$copy_cohort <- copyCohorts(cdm$original_cohort,
                                cohortId = c(3),
                                name = "copy_cohort"))
  expect_error(cdm$copy_cohort <- copyCohorts(cdm$original_cohort,
                                             cohortId = "not_a_cohort",
                                             name = "copy_cohort"))

  PatientProfiles::mockDisconnect(cdm)

})

