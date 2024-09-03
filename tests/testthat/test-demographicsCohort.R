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

  PatientProfiles::mockDisconnect(cdm)
})
