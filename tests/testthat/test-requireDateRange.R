test_that("requireDateRange", {
    cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2) |>
    omock::mockCohort(tableName = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")
  # one person, one observation periods
  cohortTable <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c("1", "2", "3"),
    cohort_start_date = as.Date(c("2010-06-06", "2011-06-06", "2012-06-08")),
    cohort_end_date = as.Date(c("2013-06-06", "2013-06-06", "2013-02-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date("2005-01-01"),
    observation_period_end_date = as.Date("2022-12-31"),
    period_type_concept_id = 32880
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohortTable,
    observation_period = observation_period
  )
  cdm$cohort1 <- cdm$cohort1 %>%
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2011-01-01")))

  expect_true(cdm$cohort1 %>%
    dplyr::pull("subject_id") == 1L)

  # expect error
  expect_error(requireInDateRange(cohort = "a"))
  expect_error(cdm$cohort1 %>%
    requireInDateRange(dateRange = as.Date(c("2010-01-01"))))
  expect_error(cdm$cohort1 %>%
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2010-01-01",
                                             "2009-01-01"))))
  expect_error(cdm$cohort1 %>%
    requireInDateRange(dateRange = c("a", "b")))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("trim cohort dates", {
  # one person, one observation periods
  cohortTable <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c("1", "2", "3"),
    cohort_start_date = as.Date(c("2010-06-06", "2011-06-06", "2012-06-08")),
    cohort_end_date = as.Date(c("2013-06-06", "2011-09-06", "2013-02-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date("2005-01-01"),
    observation_period_end_date = as.Date("2022-12-31"),
    period_type_concept_id = 32880
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohortTable,
    observation_period = observation_period
  )

  cdm$cohort1 <- cdm$cohort1 %>%
    trimToDateRange(dateRange = as.Date(c("2011-01-01", "2012-01-01")))

  expect_equal(sort(cdm$cohort1 %>%
    dplyr::pull("subject_id")), c("1", "2"))
  expect_true(cdm$cohort1 %>%
    dplyr::filter(subject_id == "1") %>%
    dplyr::pull("cohort_start_date") == as.Date("2011-01-01"))
  expect_true(cdm$cohort1 %>%
                dplyr::filter(subject_id == "1") %>%
                dplyr::pull("cohort_end_date") == as.Date("2012-01-01"))
  expect_true(cdm$cohort1 %>%
                dplyr::filter(subject_id == "2") %>%
                dplyr::pull("cohort_start_date") == as.Date("2011-06-06"))
  expect_true(cdm$cohort1 %>%
                dplyr::filter(subject_id == "2") %>%
                dplyr::pull("cohort_end_date") == as.Date("2011-09-06"))

CDMConnector::cdm_disconnect(cdm)

  } )
