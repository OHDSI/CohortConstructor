test_that("test it works and expected errors", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort()
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- cdm$cohort %>%
    requireDemographics(
      ageRange = c(0, 35),
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
  expect_equal(
    settings(cdm$cohort1),
    dplyr::tibble(
      cohort_definition_id = 1L,
      cohort_name = "cohort_1",
      age_range = "0_35",
      sex = "Both",
      min_prior_observation = 10,
      min_future_observation = 40
    )
  )
  expect_true(all(
    attrition(cdm$cohort1)$reason ==
      c('Initial qualifying events', 'Age requirement: 0 to 35',
        'Sex requirement: Both', 'Prior observation requirement: 10 days',
        'Future observation requirement: 40 days')
  ))

  cdm$cohort <- cdm$cohort %>%
    requireAge(ageRange = list(c(0, 35))) %>%
    requireSex(sex = "Both") %>%
    requirePriorObservation(minPriorObservation = 10) %>%
    requireFutureObservation(minFutureObservation = 40)

  expect_true("cohort_table" %in% class(cdm$cohort))
  expect_equal(omopgenerics::attrition(cdm$cohort),
               omopgenerics::attrition(cdm$cohort1))
  expect_true(all(cdm$cohort |> dplyr::pull("subject_id") |> sort() == c(1, 1, 3, 4)))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1996-06-30", "2001-05-30", "2003-05-02", "2015-01-27")))
  expect_equal(
    settings(cdm$cohort),
    dplyr::tibble(
      cohort_definition_id = 1L,
      cohort_name = "cohort_1",
      min_future_observation = 40,
      min_prior_observation = 10,
      sex = "Both",
      age_range = "0_35"
    )
  )
  expect_true(all(
    attrition(cdm$cohort)$reason ==
      c('Initial qualifying events', 'Age requirement: 0 to 35',
        'Sex requirement: Both', 'Prior observation requirement: 10 days',
        'Future observation requirement: 40 days')
  ))

  # expect errors
  expect_error(requireDemographics(cohort = "cohort"))
  expect_error(requireDemographics(cohort = cdm$person))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    indexDate = "aaa"
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    ageRange = c(0, 50)
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    ageRange = list(c(0, 50, 100))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    ageRange = list(c(50, 0))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    sex = "all"
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    ageRange = list(c(-10, 40))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    ageRange = list(c(0, "a"))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    sex = "a"
  ))

  expect_error(requireDemographics(
    cohort = cdm$cohort,
    minPriorObservation = -10
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    minPriorObservation = "a"
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    minFutureObservation = -10
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    minFutureObservation = "a"
  ))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("restrictions applied to single cohort", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(recordPerson = 3)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- cdm$cohort %>%
    requireDemographics(ageRange = list(c(0, 5)), name = "cohort1")
  expect_true("2001-07-30" == cdm$cohort1 %>% dplyr::pull("cohort_start_date"))
  expect_true(all(
    c("Initial qualifying events", "Age requirement: 0 to 5", "Sex requirement: Both",
      "Prior observation requirement: 0 days", "Future observation requirement: 0 days") ==
      omopgenerics::attrition(cdm$cohort1)$reason))
  expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
                  == colnames(cdm$cohort1)))
  expect_true(settings(cdm$cohort1)$cohort_definition_id == 1)
  expect_true(settings(cdm$cohort1)$cohort_name == "cohort_1")
  expect_true(settings(cdm$cohort1)$age_range == "0_5")
  expect_true(settings(cdm$cohort1)$sex == "Both")
  expect_true(settings(cdm$cohort1)$min_prior_observation == 0)
  expect_true(settings(cdm$cohort1)$min_future_observation == 0)

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
  expect_true(settings(cdm$cohort2)$cohort_definition_id == 1)
  expect_true(settings(cdm$cohort2)$cohort_name == "cohort_1")
  expect_true(settings(cdm$cohort2)$age_range == "0_150")
  expect_true(settings(cdm$cohort2)$sex == "Male")
  expect_true(settings(cdm$cohort2)$min_prior_observation == 0)
  expect_true(settings(cdm$cohort2)$min_future_observation == 0)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("ignore existing cohort extra variables", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(recordPerson = 3)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

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
  expect_true(all(colnames(settings(cdm$cohort)) == c("cohort_definition_id", "cohort_name", "min_prior_observation")))

  cdm$new_cohort <- cdm$cohort |>
    requirePriorObservation(minPriorObservation = 450, name = "new_cohort")
  expect_true(all(colnames(cdm$new_cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date",
                      "age", "sex", "prior_observation", "future_observation")))
  expect_true(cdm$new_cohort |> dplyr::tally() |> dplyr::pull() == 2)
  expect_true(all(c("Initial qualifying events", "Prior observation requirement: 450 days",
                    "Prior observation requirement: 450 days") ==
                    omopgenerics::attrition(cdm$new_cohort)$reason))
  expect_true(all(colnames(settings(cdm$cohort)) == c("cohort_definition_id", "cohort_name", "min_prior_observation")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("external columns kept after requireDemographics", {
  testthat::skip_on_cran()
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
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort <- cdm$cohort %>%
    requireDemographics(indexDate = "new_index_date", ageRange = list(c(0,5)))

  expect_true(all(c("col_extra1", "col_extra2", "new_index_date") %in% colnames(cdm$cohort)))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("cohortIds", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$new_cohort <- requireSex(cohort = cdm$cohort, cohortId = 1, sex = "Male") |>
    requirePriorObservation(cohortId = 3, minPriorObservation = 1000, name = "new_cohort")
  expect_true(all(
    omopgenerics::attrition(cdm$new_cohort)$reason ==
      c("Initial qualifying events", "Sex requirement: Male", "Initial qualifying events" ,
        "Initial qualifying events", "Prior observation requirement: 1000 days")
  ))
  expect_true(all(cdm$new_cohort |> dplyr::pull("cohort_definition_id") == c(2,2,2,3)))
  expect_true(all(cdm$new_cohort |> dplyr::pull("subject_id") == c(2,2,2,1)))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("test more than one restriction", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  # keep = false
  cdm$cohort1 <- cdm$cohort |>
    requireAge(ageRange = list(c(0,19), c(20, 40), c(0, 40)), name = "cohort1")
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_definition_id") |> sort() ==
      c(1, 1, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 8, 8, 9, 9)
  ))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
      c('1999-08-27', '1999-08-27', '2000-01-07', '2000-01-07', '2000-04-10',
        '2000-04-10', '2001-02-13', '2001-02-13', '2001-07-30', '2001-07-30',
        '2003-03-02', '2003-03-02', '2004-01-21', '2004-01-21', '2015-01-25',
        '2015-01-25', '2015-04-09', '2015-04-09')
  ))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
      c('2000-04-09', '2000-04-09', '2001-02-12', '2001-02-12', '2001-03-23',
        '2001-03-23', '2001-07-15', '2001-07-15', '2004-01-20', '2004-01-20',
        '2004-07-30', '2004-07-30', '2005-05-04', '2005-05-04', '2015-06-16',
        '2015-06-16', '2015-06-26', '2015-06-26')
  ))
  expect_true(all(
    attrition(cdm$cohort1)$reason |> sort() ==
      c('Age requirement: 0 to 19', 'Age requirement: 0 to 19', 'Age requirement: 0 to 19',
        'Age requirement: 0 to 40', 'Age requirement: 0 to 40', 'Age requirement: 0 to 40',
        'Age requirement: 20 to 40', 'Age requirement: 20 to 40', 'Age requirement: 20 to 40',
        'Initial qualifying events', 'Initial qualifying events', 'Initial qualifying events',
        'Initial qualifying events', 'Initial qualifying events', 'Initial qualifying events',
        'Initial qualifying events', 'Initial qualifying events', 'Initial qualifying events')
  ))
  expect_true(all(
    attrition(cdm$cohort1)$number_records |> sort() ==
      c(0, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
  ))
  expect_true(all(settings(cdm$cohort1)$age_range |> unique() == c("0_19", "0_40", "20_40")))
  expect_true(all(settings(cdm$cohort1)$cohort_name |> unique() ==
                    c("cohort_1_1", "cohort_2_1", "cohort_3_1", "cohort_1_2", "cohort_2_2",
                      "cohort_3_2", "cohort_1_3", "cohort_2_3", "cohort_3_3")))

  # keep = true
  cdm$cohort2 <- cdm$cohort |>
    requireAge(ageRange = list(c(0,19), c(20, 40), c(0, 40), c(0, 150)), name = "cohort2")
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_definition_id") |> sort() ==
      c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 11, 11, 11, 12, 12)
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort() ==
      c('1999-08-27', '1999-08-27', '1999-08-27', '2000-01-07', '2000-01-07', '2000-01-07',
        '2000-04-10', '2000-04-10', '2000-04-10', '2001-02-13', '2001-02-13', '2001-02-13',
        '2001-07-30', '2001-07-30', '2001-07-30', '2003-03-02', '2003-03-02', '2003-03-02',
        '2004-01-21', '2004-01-21', '2004-01-21', '2015-01-25', '2015-01-25', '2015-01-25',
        '2015-04-09', '2015-04-09', '2015-04-09')
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_end_date") |> sort() ==
      c('2000-04-09', '2000-04-09', '2000-04-09', '2001-02-12', '2001-02-12', '2001-02-12',
        '2001-03-23', '2001-03-23', '2001-03-23', '2001-07-15', '2001-07-15', '2001-07-15',
        '2004-01-20', '2004-01-20', '2004-01-20', '2004-07-30', '2004-07-30', '2004-07-30',
        '2005-05-04', '2005-05-04', '2005-05-04', '2015-06-16', '2015-06-16', '2015-06-16',
        '2015-06-26', '2015-06-26', '2015-06-26')
  ))
  expect_true(all(settings(cdm$cohort2)$age_range |> unique() == c("0_150", "0_19", "0_40", "20_40")))
  expect_true(all(colnames(settings(cdm$cohort2)) %in% c("cohort_definition_id", "cohort_name", "age_range")))
  expect_true(all(
    settings(cdm$cohort2)$cohort_name |> sort() ==
      c('cohort_1_1', 'cohort_1_2', 'cohort_1_3', 'cohort_1_4', 'cohort_2_1',
        'cohort_2_2', 'cohort_2_3', 'cohort_2_4', 'cohort_3_1', 'cohort_3_2',
        'cohort_3_3', 'cohort_3_4')
  ))

  PatientProfiles::mockDisconnect(cdm)

  # one empty output cohort
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3, seed = 4)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  # empty cohort return (no males in the cohort)
  cdm$cohort1 <- cdm$cohort |>
    requireSex(sex = c("Both", "Male"), name = "cohort1") |>
    requirePriorObservation(minPriorObservation = 3, name = "cohort1")

  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_definition_id") |> sort() ==
      cdm$cohort |> dplyr::pull("cohort_definition_id") |> sort()
  ))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
      cdm$cohort |> dplyr::pull("cohort_end_date") |> sort()
  ))
  expect_true(all(
    attrition(cdm$cohort1)$reason |> sort() ==
      c('Initial qualifying events', 'Initial qualifying events', 'Initial qualifying events',
        'Initial qualifying events', 'Initial qualifying events', 'Initial qualifying events',
        'Prior observation requirement: 3 days', 'Prior observation requirement: 3 days',
        'Prior observation requirement: 3 days', 'Prior observation requirement: 3 days',
        'Prior observation requirement: 3 days', 'Prior observation requirement: 3 days',
        'Sex requirement: Both', 'Sex requirement: Both', 'Sex requirement: Both',
        'Sex requirement: Male', 'Sex requirement: Male', 'Sex requirement: Male')
  ))
  expect_true(all(
    attrition(cdm$cohort1)$number_records |> sort() ==
      c(rep(0, 6), rep(3, 12))
  ))
  expect_true(all(settings(cdm$cohort1)$min_prior_observation |> unique() == 3))
  expect_true(all(colnames(settings(cdm$cohort1)) %in% c("cohort_definition_id", "cohort_name", "min_prior_observation", "sex")))
  expect_true(all(
    settings(cdm$cohort1)$cohort_name |> sort() ==
      c("cohort_1_1", "cohort_1_2", "cohort_2_1", "cohort_2_2", "cohort_3_1", "cohort_3_2")
  ))

  cdm$cohort2 <- cdm$cohort |>
    requireSex(sex = c("Both", "Male"), name = "cohort2", cohortId = c(1,3)) |>
    requirePriorObservation(minPriorObservation = c(3, 2), name = "cohort2", cohortId = 1)
  expect_true(all(settings(cdm$cohort2) |> dplyr::arrange(.data$cohort_definition_id) |> dplyr::pull("cohort_name") ==
                    c("cohort_1_1_1", "cohort_2", "cohort_3_1", "cohort_1_2", "cohort_3_2", "cohort_1_1_2")))
  expect_true(all(
    attrition(cdm$cohort2)$reason |> sort() ==
      c('Initial qualifying events', 'Initial qualifying events', 'Initial qualifying events',
        'Initial qualifying events', 'Initial qualifying events', 'Initial qualifying events',
        'Prior observation requirement: 2 days', 'Prior observation requirement: 3 days',
        'Sex requirement: Both', 'Sex requirement: Both', 'Sex requirement: Both',
        'Sex requirement: Male', 'Sex requirement: Male')
  ))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("codelist kept with >1 requirement", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod()
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1, 2),
    "concept_name" = c("my concept 1", "my concept 2"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:13,
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 4, 4),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, 430, 10),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, 400, 100),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )

  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1, b = 2), name = "cohort1")

  cdm$cohort2 <- cdm$cohort1 |> requireDemographics(name = "cohort2", minPriorObservation = c(0,3), cohortId = 1)
  expect_equal(
    attr(cdm$cohort2, "cohort_codelist") |> dplyr::collect() |> dplyr::arrange(.data$cohort_definition_id),
    dplyr::tibble(
      cohort_definition_id = 1:3,
      codelist_name = c("a", "b", "a"),
      concept_id = c(1, 2, 1),
      type = c("index event", "index event", "index event")
    )
  )

  PatientProfiles::mockDisconnect(cdm)
})

test_that("settings with extra columns", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3, seed = 4)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort <- cdm$cohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm$cohort) |>
        dplyr::mutate(sex = "Both", extra1 = 1, extra2 = "hi")
      )
  cdm$cohort <- cdm$cohort |> requireSex(sex = c("Both", "Female"))
  expect_equal(
    cdm$cohort |> settings() |> dplyr::arrange(.data$cohort_definition_id),
    dplyr::tibble(
      cohort_definition_id = 1:6,
      cohort_name = c("cohort_1_1", "cohort_2_1", "cohort_3_1", "cohort_1_2", "cohort_2_2", "cohort_3_2"),
      sex = c(rep("Both", 3), rep("Female", 3)),
      extra1 = 1,
      extra2 = "hi"
    )
  )
  expect_true(all(colnames(attrition(cdm$cohort)) ==
                    c("cohort_definition_id", "number_records", "number_subjects", "reason_id", "reason", "excluded_records", "excluded_subjects" )))
  PatientProfiles::mockDisconnect(cdm)
})

test_that("requireInteractions", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3, seed = 4)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- cdm$cohort |>
    requireDemographics(sex = c("Both", "Female"),
                        minPriorObservation = c(0, 1),
                        requirementInteractions = FALSE,
                        name = "cohort1")
  expect_equal(
    cdm$cohort1 |> settings() |> dplyr::arrange(.data$cohort_definition_id),
    dplyr::tibble(
      cohort_definition_id = 1:9,
      cohort_name = c("cohort_1_1", "cohort_2_1", "cohort_3_1", "cohort_1_2", "cohort_2_2", "cohort_3_2", "cohort_1_3", "cohort_2_3", "cohort_3_3"),
      age_range = "0_150",
      sex = c(rep("Both", 6), rep("Female", 3)),
      min_prior_observation = c(rep(0, 3), rep(1, 3), rep(0, 3)),
      min_future_observation = rep(0, 9)
    )
  )
  PatientProfiles::mockDisconnect(cdm)
})
