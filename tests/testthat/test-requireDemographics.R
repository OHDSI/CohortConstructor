test_that("test it works and expected errors", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort()
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort1 <- cdm$cohort %>%
    requireDemographics(
      ageRange = list(c(0, 35)),
      indexDate = "cohort_start_date",
      sex = "Both",
      minPriorObservation = 10,
      minFutureObservation = 40,
      name = "cohort1"
    )
  expect_true("cohort_table" %in% class(cdm$cohort1))
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") == c(1, 1, 3, 4)))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") ==
                    c("2001-05-30", "2003-05-02", "2015-01-27", "1996-06-30")))

  cdm$cohort <- cdm$cohort %>%
    requireAge(ageRange = list(c(0, 35))) %>%
    requireSex(sex = "Both") %>%
    requirePriorObservation(minPriorObservation = 10) %>%
    requireFutureObservation(minFutureObservation = 40)

  expect_true("cohort_table" %in% class(cdm$cohort))
  expect_equal(omopgenerics::attrition(cdm$cohort),
               omopgenerics::attrition(cdm$cohort1))
  expect_true(all(cdm$cohort |> dplyr::pull("subject_id") == c(1, 1, 3, 4)))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_start_date") ==
                    c("2001-05-30", "2003-05-02", "2015-01-27", "1996-06-30")))

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
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(recordPerson = 3)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort1 <- cdm$cohort %>%
    requireDemographics(ageRange = list(c(0, 5)), name = "cohort1")
  expect_true("2001-07-30" == cdm$cohort1 %>% dplyr::pull("cohort_start_date"))
  expect_true(all(
    c("Initial qualifying events", "Age requirement: 0 to 5", "Sex requirement: Both",
      "Prior observation requirement: 0 days", "Future observation requirement: 0 days") ==
    omopgenerics::attrition(cdm$cohort1)$reason))
  expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
                  == colnames(cdm$cohort1)))

  cdm$cohort2 <- cdm$cohort %>%
    requireDemographics(sex = "Male", name = "cohort2")
  expect_equal(dplyr::collect(cdm$cohort)$cohort_start_date,
               dplyr::collect(cdm$cohort2)$cohort_start_date)
  expect_true(all(
    c("Initial qualifying events", "Age requirement: 0 to 150", "Sex requirement: Male",
      "Prior observation requirement: 0 days", "Future observation requirement: 0 days") ==
      omopgenerics::attrition(cdm$cohort2)$reason))
  expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
                  == colnames(cdm$cohort2)))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("ignore existing cohort extra variables", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(recordPerson = 3)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")
  cdm$cohort <- cdm$cohort |>
    PatientProfiles::addDemographics() |>
    dplyr::compute(name = "cohort", temporary = FALSE)

  cdm$cohort <- cdm$cohort |>
    requirePriorObservation(minPriorObservation = 450)
  expect_true(all(colnames(cdm$cohort) ==
                c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date",
                  "age", "sex", "prior_observation", "future_observation")))
  expect_true(cdm$cohort |> dplyr::tally() |> dplyr::pull() == 2)
  expect_true(all(c("Initial qualifying events", "Prior observation requirement: 450 days") ==
                    omopgenerics::attrition(cdm$cohort)$reason))

  cdm$new_cohort <- cdm$cohort |>
    requirePriorObservation(minPriorObservation = 450, name = "new_cohort")
  expect_true(all(colnames(cdm$new_cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date",
                      "age", "sex", "prior_observation", "future_observation")))
  expect_true(cdm$new_cohort |> dplyr::tally() |> dplyr::pull() == 2)
  expect_true(all(c("Initial qualifying events", "Prior observation requirement: 450 days",
                    "Prior observation requirement: 450 days") ==
                    omopgenerics::attrition(cdm$new_cohort)$reason))
})

test_that("external columns kept after requireDemographics", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(recordPerson = 3)
  cdm_local$cohort <- cdm_local$cohort %>%
    dplyr::mutate(
      col_extra1 = as.numeric(subject_id) + 1,
      col_extra2 = as.numeric(subject_id) + 2,
      new_index_date = cohort_start_date + 1
    )
  cdm <- CDMConnector::copy_cdm_to(
    con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    cdm = cdm_local,
    schema = "main")

  cdm$cohort <- cdm$cohort %>%
    requireDemographics(indexDate = "new_index_date", ageRange = list(c(0,5)))

  expect_true(all(c("col_extra1", "col_extra2", "new_index_date") %in% colnames(cdm$cohort)))
})

test_that("cohortIds", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3)
  cdm <- CDMConnector::copy_cdm_to(
    con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    cdm = cdm_local,
    schema = "main")

 cdm$new_cohort <- requireSex(cohort = cdm$cohort, cohortId = 1, sex = "Male") |>
    requirePriorObservation(cohortId = 3, minPriorObservation = 1000, name = "new_cohort")
 expect_true(all(
   omopgenerics::attrition(cdm$new_cohort)$reason ==
     c("Initial qualifying events", "Sex requirement: Male", "Initial qualifying events" ,
       "Initial qualifying events", "Prior observation requirement: 1000 days")
 ))
 expect_true(all(cdm$new_cohort |> dplyr::pull("cohort_definition_id") == c(2,2,2,3)))
 expect_true(all(cdm$new_cohort |> dplyr::pull("subject_id") == c(2,2,2,1)))
})
