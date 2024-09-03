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

test_that("Example: ageRange", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(18, 40))
  )

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addAge()

  expect_true(all(cdm$cohort3 |> dplyr::pull("age") >= 18))
  expect_true(all(cdm$cohort3 |> dplyr::pull("age") <= 40))

  cdm$cohort3 <- cdm$cohort3 |>
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) |>
    dplyr::mutate(check1 = (cohort_start_date >= observation_period_start_date),
                  check2 = (cohort_end_date <= observation_period_end_date)) |>
    dplyr::compute()

  expect_true(all(cdm$cohort3 |> dplyr::pull("check1")== T))
  expect_true(all(cdm$cohort3 |> dplyr::pull("check2")== T))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("Example: priorObs", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3", minPriorObservation = 15)
  )

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addPriorObservation()

  expect_true(all(cdm$cohort3 |> dplyr::pull("prior_observation") == 15))

  cdm$cohort3 <- cdm$cohort3 |>
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) |>
    dplyr::mutate(check1 = (cohort_start_date > observation_period_start_date),
                  check2 = (cohort_end_date == observation_period_end_date)) |>
    dplyr::compute()

  expect_true(all(cdm$cohort3 |> dplyr::pull("check1")== T))
  expect_true(all(cdm$cohort3 |> dplyr::pull("check2")== T))

  loc_cohort3 <- cdm$cohort3 |>
    dplyr::collect() |>
    dplyr::mutate(check3 = observation_period_start_date + 15) |>
    dplyr::mutate(check3 = (check3 == cohort_start_date))

  expect_true(all(loc_cohort3 |> dplyr::pull("check3")== T))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("Example: mixture of parameters", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3",
                         ageRange = c(18,90),
                         sex = "Male",
                         minPriorObservation = 25)
  )

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addPriorObservation()

  expect_true(all(cdm$cohort3 |> dplyr::pull("prior_observation") >= 25))

  cdm$cohort3 <- cdm$cohort3 |>
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) |>
    dplyr::mutate(check1 = (cohort_start_date > observation_period_start_date),
                  check2 = (cohort_end_date == observation_period_end_date)) |>
    dplyr::compute()

  expect_true(all(cdm$cohort3 |> dplyr::pull("check1")== T))
  expect_true(all(cdm$cohort3 |> dplyr::pull("check2")== T))

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addAge()

  expect_true(all(cdm$cohort3 |> dplyr::pull("age") >= 18))
  expect_true(all(cdm$cohort3 |> dplyr::pull("age") <= 50))

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addSex()

  expect_true(all(cdm$cohort3 |> dplyr::pull("sex")== "Male"))

  PatientProfiles::mockDisconnect(cdm)
})
