test_that("input validation", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(18,40), sex = "Male")
  )

  expect_error(
    cdm$cohort4 <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(18,40), sex = "Male")
  )

  expect_error(
    cohort <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(40,18), sex = "Male")
  )

  expect_no_error(
    cohort <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(18,40), sex = "Male", minPriorObservation = 15)
  )

  expect_error(
    cohort <- cdm |>
      demographicsCohort(name = "cohort3", minPriorObservation = -15)
  )

  expect_error(
    cohort <- cdm |>
      demographicsCohort(name = "cohort3", minPriorObservation = "15")
  )

  PatientProfiles::mockDisconnect(cdm)
})

test_that("Example: sex", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3", sex = "Male")
  )

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addSex()

  expect_true(all(cdm$cohort3 |> dplyr::pull("sex")== "Male"))

  expect_true(
    setequal((cdm$cohort3 |>
                dplyr::pull("subject_id")),
             (cdm$person |> dplyr::filter(gender_concept_id==8507) |> dplyr::pull("person_id"))
    )
  )

  cdm$cohort3 <- cdm$cohort3 |>
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) |>
    dplyr::mutate(check1 = (cohort_start_date == observation_period_start_date),
                  check2 = (cohort_end_date == observation_period_end_date)) |>
    dplyr::compute()

  expect_true(all(cdm$cohort3 |> dplyr::pull("check1")== T))
  expect_true(all(cdm$cohort3 |> dplyr::pull("check2")== T))

  PatientProfiles::mockDisconnect(cdm)
})
