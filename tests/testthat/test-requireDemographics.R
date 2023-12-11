test_that("simple example", {
  cdm <- PatientProfiles::mockPatientProfiles(
    patient_size = 100,
    drug_exposure_size = 100
  )
  cdm$cohort1 <- cdm$cohort1 %>%
    requireDemographics(
      ageRange = list(c(0, 50)),
      indexDate = "cohort_start_date",
      sex = "Both",
      minPriorObservation = 10,
      minFutureObservation = 5
    )

  expect_true("GeneratedCohortSet" %in% class(cdm$cohort1))

  cdm$cohort1 <- cdm$cohort1 %>%
    requireAge(ageRange = list(c(10, 40))) %>%
    requireSex(sex = "Male") %>%
    requirePriorObservation(minPriorObservation = 20) %>%
    requireFutureObservation(minFutureObservation = 10)

  # expect errors
  expect_error(requireDemographics(cohort = "cohort"))
  expect_error(requireDemographics(cohort = cdm$person))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    indexDate = "aaa"
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    ageRange = c(0, 50)
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    ageRange = list(c(50, 40))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    ageRange = list(c(-10, 40))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    ageRange = list(c(0, "a"))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    sex = "a"
  ))

  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    minPriorObservation = -10
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    minPriorObservation = "a"
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    minFutureObservation = -10
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    minFutureObservation = "a"
  ))

  # multiple options not currently supported
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    ageRange = list(
      c(0, 50),
      c(51, 100)
    )
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    sex = c("Both", "Male")
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    minPriorObservation = c(0, 10)
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    minFutureObservation = c(0, 10)
  ))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("restrictions applied to single cohort", {
  # one person, one observation periods
  personTable <- dplyr::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8532", "8507"),
    year_of_birth = c(2000, 2005, 2010),
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  cohortTable <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c("1", "2", "3"),
    cohort_start_date = as.Date(c("2010-06-06", "2010-06-06", "2010-06-06")),
    cohort_end_date = as.Date(c("2013-06-06", "2013-06-06", "2013-02-01"))
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    person = personTable,
    observation_period = observationPeriodTable,
    cohort1 = cohortTable
  )
  cdm$cohort1 <- cdm$cohort1 %>%
    requireDemographics(ageRange = list(c(0, 5)))

  expect_equal(
    c("2", "3"),
    sort(cdm$cohort1 %>%
      dplyr::pull("subject_id"))
  )

  cdm$cohort1 <- cdm$cohort1 %>%
    requireDemographics(sex = "Male")
  expect_equal(
    c("3"),
    sort(cdm$cohort1 %>%
      dplyr::pull("subject_id"))
  )


  CDMConnector::cdm_disconnect(cdm)
})

test_that("ignore existing cohort extra variables", {
  # ignore existing conflicting age column, but keep it in the output
})



test_that("requireDateRange", {
  # one person, one observation periods

  cohortTable <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c("1", "2", "3"),
    cohort_start_date = as.Date(c("2010-06-06", "2010-06-06", "2010-06-08")),
    cohort_end_date = as.Date(c("2013-06-06", "2013-06-06", "2013-02-01"))
  )

  cdm <- PatientProfiles::mockPatientProfiles(cohort1 = cohortTable)
  cdm$cohort1 <- cdm$cohort1 %>%
    requireDateRange(cohortDateRange = as.Date(c("2010-06-06", "2013-02-01")))

  expect_true(all(cdm$cohort1 %>% dplyr::pull(cohort_start_date) ==
    as.Date(c("2010-06-06", "2010-06-06", "2010-06-08"))))

  CDMConnector::cdm_disconnect(cdm)
})



test_that("external columns kept after requireDemographics", {
  cdm <- PatientProfiles::mockPatientProfiles(
    patient_size = 100,
    drug_exposure_size = 100
  )
  cdm$cohort1 <- cdm$cohort1 %>% dplyr::mutate(
    col_extra1 = as.numeric(subject_id) + 1,
    col_extra2 = as.numeric(subject_id) + 2
  )


  cdm$cohort1 <- cdm$cohort1 %>%
    requireDemographics()

  expect_true(all(c("col_extra1", "col_extra2") %in% colnames(cdm$cohort1)))
})
