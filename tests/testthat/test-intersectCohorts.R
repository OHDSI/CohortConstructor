test_that("intersect example - two cohorts", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 2)
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2018-01-01")),
      observation_period_end_date = as.Date(c("2022-03-01")),
      period_type_concept_id = 1L
    )
  )
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L, 2L),
      subject_id = c(1L, 1L, 2L),
      cohort_start_date = as.Date(c("2018-01-03", "2020-06-01",
                                    "2020-01-03")),
      cohort_end_date = as.Date(c("2021-01-10", "2020-06-10",
                                  "2020-01-10"))
    ))
  cdm <- cdm_local |> copyCdm()

  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  # person 1 has an intersection
  # they are in both cohorts between 1st June and 10th June 2020
  cdm$my_cohort_2 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_2" ,
    keepOriginalCohorts = FALSE,
    gap = 0)
  expect_true(nrow(cdm$my_cohort_2 |>
                     dplyr::collect()) == 1L)
  expect_true(cdm$my_cohort_2 |>
                dplyr::pull("subject_id") == 1L)
  expect_identical(cdm$my_cohort_2 |>
                 dplyr::pull("cohort_start_date"), as.Date("2020-06-01"))
  expect_identical(cdm$my_cohort_2 |>
                 dplyr::pull("cohort_end_date"), as.Date("2020-06-10"))
})

test_that("intersect example - three cohorts", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 2)
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2018-01-01")),
      observation_period_end_date = as.Date(c("2022-03-01")),
      period_type_concept_id = 1L
    )
  )
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L, 3L, 2L),
      subject_id = c(1L, 1L, 1L, 2L),
      cohort_start_date = as.Date(c("2018-01-03", "2020-06-01", "2020-06-03",
                                    "2020-01-03")),
      cohort_end_date = as.Date(c("2021-01-10", "2020-06-10", "2020-06-09",
                                  "2020-01-10"))
    ))
  cdm <- cdm_local |> copyCdm()
  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  # person 1 has an intersection
  # they are in all three cohorts between 3rd June and 9th June 2020
  cdm$my_cohort_2 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_2" ,
    keepOriginalCohorts = FALSE,
    gap = 0)
  expect_true(nrow(cdm$my_cohort_2 |>
                     dplyr::collect()) == 1L)
  expect_true(cdm$my_cohort_2 |>
                dplyr::pull("subject_id") == 1L)
  expect_identical(cdm$my_cohort_2 |>
                 dplyr::pull("cohort_start_date"), as.Date("2020-06-03"))
  expect_identical(cdm$my_cohort_2 |>
                 dplyr::pull("cohort_end_date"), as.Date("2020-06-09"))

  PatientProfiles::mockDisconnect(cdm)

})

test_that("intersect example - nobody with intersection", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 2)
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2018-01-01")),
      observation_period_end_date = as.Date(c("2022-03-01")),
      period_type_concept_id = 1L
    )
  )
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L, 2L),
      subject_id = c(1L, 1L, 2L),
      cohort_start_date = as.Date(c("2018-01-03", "2020-06-01",
                                    "2020-01-03")),
      cohort_end_date = as.Date(c("2019-01-10", "2020-06-10",
                                  "2020-01-10"))
    ))
  cdm <- cdm_local |> copyCdm()
  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  # person 1 has an intersection
  # they are in both cohorts between 1st June and 10th June 2020
  cdm$my_cohort_2 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_2" ,
    keepOriginalCohorts = FALSE,
    gap = 0)
  expect_true(nrow(cdm$my_cohort_2 |>
                     dplyr::collect()) == 0)
  expect_true(nrow(settings(cdm$my_cohort_2) |>
                     dplyr::collect()) == 1)

  PatientProfiles::mockDisconnect(cdm)

})

test_that("intersect with gap", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1)
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L),
      person_id = c(1L),
      observation_period_start_date = as.Date(c("2018-01-01")),
      observation_period_end_date = as.Date(c("2020-03-01")),
      period_type_concept_id = 1L
    )
  )
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L),
      subject_id = c(1L, 1L),
      cohort_start_date = as.Date(c("2018-01-03", "2020-01-03")),
      cohort_end_date = as.Date(c("2018-01-10", "2020-01-10"))
    ))
  cdm <- cdm_local |> copyCdm()

  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  # with gap of zero there should be no intersect records
  cdm$my_cohort_2 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_2" ,
    keepOriginalCohorts = FALSE,
    gap = 0)
  expect_true(nrow(cdm$my_cohort_2 |>
                     dplyr::collect()) == 0L)

  # but if we do use a sufficient gap we will have a record
  cdm$my_cohort_3 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_3" ,
    keepOriginalCohorts = FALSE,
    gap = 5000)
  expect_true(nrow(cdm$my_cohort_3 |>
                     dplyr::collect()) == 1L)
  expect_true(cdm$my_cohort_3 |>
                dplyr::pull("subject_id") == 1L)
  expect_identical(cdm$my_cohort_3 |>
                 dplyr::pull("cohort_start_date"), as.Date("2018-01-03"))
  expect_identical(cdm$my_cohort_3 |>
                 dplyr::pull("cohort_end_date"), as.Date("2020-01-10"))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("keepOriginalCohorts", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 2)
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2018-01-01")),
      observation_period_end_date = as.Date(c("2022-03-01")),
      period_type_concept_id = 1L
    )
  )
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L, 2L),
      subject_id = c(1L, 1L, 2L),
      cohort_start_date = as.Date(c("2018-01-03", "2020-06-01",
                                    "2020-01-03")),
      cohort_end_date = as.Date(c("2021-01-10", "2020-06-10",
                                  "2020-01-10"))
    ))
  cdm <- cdm_local |> copyCdm()
  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  # person 1 has an intersection
  # they are in both cohorts between 1st June and 10th June 2020
  cdm$my_cohort_2 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_2" ,
    keepOriginalCohorts = TRUE,
    gap = 0)

  expect_true(nrow(settings(cdm$my_cohort) |>
    dplyr::inner_join(settings(cdm$my_cohort_2),
                      by = c("cohort_definition_id", "cohort_name"))) == 2)

  # with returnNonOverlappingCohorts
  cdm$my_cohort_3 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_3" ,
    keepOriginalCohorts = TRUE,
    returnNonOverlappingCohorts = TRUE,
    gap = 0)
  expect_true(nrow(settings(cdm$my_cohort) |>
                     dplyr::inner_join(settings(cdm$my_cohort_3),
                                       by = c("cohort_definition_id", "cohort_name"))) == 2)

  # nobody with intersection
  cdm$my_cohort <- cdm$my_cohort |>
    dplyr::filter(cohort_start_date != "2020-06-01") |>
    omopgenerics::recordCohortAttrition("filter")
  cdm$my_cohort_4 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_4" ,
    keepOriginalCohorts = TRUE,
    gap = 0)
  expect_true(nrow(settings(cdm$my_cohort) |>
                     dplyr::inner_join(settings(cdm$my_cohort_4),
                                       by = c("cohort_definition_id", "cohort_name"))) == 2)

  cdm$my_cohort_5 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_5" ,
    keepOriginalCohorts = TRUE,
    returnNonOverlappingCohorts = TRUE,
    gap = 0)
  expect_true(nrow(settings(cdm$my_cohort) |>
                     dplyr::inner_join(settings(cdm$my_cohort_5),
                                       by = c("cohort_definition_id", "cohort_name"))) == 2)

  PatientProfiles::mockDisconnect(cdm)

})

test_that("returnNonOverlappingCohorts - two cohorts", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 2)
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2018-01-01")),
      observation_period_end_date = as.Date(c("2022-03-01")),
      period_type_concept_id = 1L
    )
  )
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L, 2L),
      subject_id = c(1L, 1L, 2L),
      cohort_start_date = as.Date(c("2018-01-03", "2020-06-01",
                                    "2020-01-03")),
      cohort_end_date = as.Date(c("2021-01-10", "2020-06-10",
                                  "2020-01-10"))
    ))
  cdm <- cdm_local |> copyCdm()
  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  # person 1 has an intersection
  # they are in both cohorts between 1st June and 10th June 2020
  cdm$my_cohort_2 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_2" ,
    keepOriginalCohorts = FALSE,
    returnNonOverlappingCohorts = TRUE,
    gap = 0)
  # subject 1 now has two records for when they were in cohort 1 and not cohort 2
  onlyCohort1 <- settings(cdm$my_cohort_2) |>
    dplyr::filter(cohort_name == "only_in_cohort_1") |>
    dplyr::pull("cohort_definition_id")
  expect_true(all(sort(cdm$my_cohort_2 |>
    dplyr::filter(cohort_definition_id == !!onlyCohort1) |>
    dplyr::pull(cohort_start_date)) == as.Date(c("2018-01-03", "2020-06-11"))))

  PatientProfiles::mockDisconnect(cdm)

})

test_that("returnNonOverlappingCohorts - three cohorts", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 2)
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2018-01-01")),
      observation_period_end_date = as.Date(c("2022-03-01")),
      period_type_concept_id = 1L
    )
  )
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L, 3L, 2L),
      subject_id = c(1L, 1L, 1L, 2L),
      cohort_start_date = as.Date(c("2018-01-03", "2020-06-01", "2020-06-03",
                                    "2020-01-03")),
      cohort_end_date = as.Date(c("2021-01-10", "2020-06-10", "2020-06-09",
                                  "2020-01-10"))
    ))
  cdm <- cdm_local |> copyCdm()
  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  # person 1 has an intersection
  # they are in all three cohorts between 3rd June and 9th June 2020
  # were only in cohort 1 between 1st March 2018 and 31st May 2020, and
  # between 11th June 2020 and 10th January 2021
  cdm$my_cohort_2 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_2" ,
    keepOriginalCohorts = FALSE,
    returnNonOverlappingCohorts = TRUE,
    gap = 0)

  # subject 1 now has two records for when they were in cohort 1 and not cohort 2 or cohort 3
  onlyCohort1 <- settings(cdm$my_cohort_2) |>
    dplyr::filter(cohort_name == "only_in_cohort_1") |>
    dplyr::pull("cohort_definition_id")
  expect_true(all(sort(cdm$my_cohort_2 |>
                         dplyr::filter(cohort_definition_id == !!onlyCohort1) |>
                         dplyr::pull(cohort_start_date)) == as.Date(c("2018-01-03", "2020-06-11"))))
  expect_true(all(sort(cdm$my_cohort_2 |>
                         dplyr::filter(cohort_definition_id == !!onlyCohort1) |>
                         dplyr::pull(cohort_end_date)) == as.Date(c("2020-05-31", "2021-01-10"))))

  PatientProfiles::mockDisconnect(cdm)

})

test_that("attrition and cohortId", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 4, seed = 2)
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- cdm$cohort1 |>
    requireInDateRange(dateRange = as.Date(c("1990-01-01", "2025-01-01"))) |>
    requireSex(sex = "Female") |>
    requireAge(ageRange = list(c(0,40)))

  cdm$cohort1 <- intersectCohorts(
    cohort = cdm$cohort1, cohortId = c("cohort_1", "cohort_2"),
    name = "cohort1", returnNonOverlappingCohorts = TRUE,
    keepOriginalCohorts = FALSE
  )
  expect_true(nrow(settings(cdm$cohort1)) == 3)
  expect_identical(settings(cdm$cohort1)$non_overlapping, c(NA, TRUE, TRUE))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort1)$reason %in%
  c('Initial qualifying events', 'Initial qualifying events',
    'cohort_start_date after 1990-01-01', 'cohort_start_date before 2025-01-01',
    'Sex requirement: Female', 'Age requirement: 0 to 40',
    'Trim to non overlapping entries', 'Initial qualifying events',
    'cohort_start_date after 1990-01-01', 'cohort_start_date before 2025-01-01',
    'Sex requirement: Female', 'Age requirement: 0 to 40',
    'Trim to non overlapping entries')
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason_id ==  c(1, 1:6, 1:6)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$number_records |> sort() ==
                    c(0, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$number_subjects |> sort() ==
                    c(0, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$excluded_records |> sort() ==
                    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$excluded_subjects |> sort() ==
                    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2)))
  expect_true(all(omopgenerics::settings(cdm$cohort1)$cohort_name |> sort() ==
                    c("cohort_1_cohort_2", "only_in_cohort_1", "only_in_cohort_2")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("codelist", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(seed = 1)
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1, 2, 3),
    "concept_name" = c("my concept 1", "my concept 2", "my concept 3"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:17,
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 3, 3, 1, 2, 3, 3),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, 430, -10, 100, 123, -10, 1000),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, 400, -100, NA, 190, 123, 1500),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )
  cdm_local$observation_period <- cdm_local$observation_period |>
    dplyr::mutate(observation_period_start_date = as.Date("1980-01-01"), observation_period_end_date = as.Date("2020-01-01"))

  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- conceptCohort(cdm, conceptSet = list(c1 = c(1,3), c2 = c(2)), name = "cohort1")

  # intersect concept generated cohort
  cdm$cohort2 <- intersectCohorts(cdm$cohort1, name = "cohort2")
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("2012-01-21", "2014-02-09")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2012-03-11", "2014-03-31")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("subject_id") |> sort() == c(1, 1)
  ))
  codes <- attr(cdm$cohort2, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  # mutually esclusive
  cdm$cohort3 <- intersectCohorts(cdm$cohort1, returnNonOverlappingCohorts = TRUE, name = "cohort3")
  expect_identical(collectCohort(cdm$cohort3, 1), collectCohort(cdm$cohort2, 1))
  expect_true(all(
    cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort() ==
      c('2009-12-22', '2010-01-01', '2010-01-11', '2010-05-31', '2012-01-21',
        '2012-03-12', '2012-09-27', '2014-02-09', '2014-04-01', '2014-12-06')
  ))
  expect_true(all(
    cdm$cohort3 |> dplyr::pull("cohort_end_date") |> sort() ==
      c('2010-05-04', '2011-08-24', '2012-01-20', '2012-03-11', '2014-02-08',
        '2014-02-09', '2014-03-31', '2014-05-20', '2014-12-10', '2015-06-24')
  ))
  expect_true(all(
    cdm$cohort3 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 1, 1, 1, 1, 2, 3, 3, 3)
  ))
  codes <- attr(cdm$cohort3, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 4), rep("c2", 2))))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 1, 2, 2, 3, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort()== rep("index event", 6)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1, 2, 2, 3)))

  # only comb
  cdm$cohort4 <- intersectCohorts(cdm$cohort1, keepOriginalCohorts = FALSE, name = "cohort4")
  expect_true(all(
    cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("2012-01-21", "2014-02-09")
  ))
  expect_true(all(
    cdm$cohort4 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2012-03-11", "2014-03-31")
  ))
  expect_true(all(
    cdm$cohort4 |> dplyr::pull("subject_id") |> sort() == c(1, 1)
  ))
  codes <- attr(cdm$cohort4, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  # union concept + non concept cohorts
  cdm <- omopgenerics::bind(cdm$cohort, cdm$cohort1, name = "cohort5")
  cdm$cohort6 <- intersectCohorts(cdm$cohort5, name = "cohort6")
  codes <- attr(cdm$cohort6, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("records combined for gap must be in the same observation period", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 2)
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L, 3L),
      person_id = c(1L, 1L, 2L),
      observation_period_start_date = as.Date(c("2018-01-01",
                                                "2020-01-01",
                                                "2018-01-01")),
      observation_period_end_date = as.Date(c("2019-01-01",
                                              "2020-03-01",
                                              "2020-03-01")),
      period_type_concept_id = 1L
    )
  )
  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L, 1L, 2L),
      subject_id = c(1L, 1L, 2L, 2L),
      cohort_start_date = as.Date(c("2018-01-03", "2020-01-03",
                                    "2018-01-03", "2020-01-03")),
      cohort_end_date = as.Date(c("2018-01-10", "2020-01-10",
                                  "2018-01-10", "2020-01-10"))
    ))
  cdm <- cdm_local |> copyCdm()

  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  # cohort entries are identical
  # but person 1 has two observation periods, so their records should not be
  # combined
  cdm$my_cohort_2 <- intersectCohorts(
    cohort = cdm$my_cohort,
    name = "my_cohort_2" ,
    keepOriginalCohorts = FALSE,
    gap = 5000)
  expect_true(nrow(cdm$my_cohort_2 |>
                     dplyr::collect()) == 1L)
  expect_true(cdm$my_cohort_2 |>
                dplyr::pull("subject_id") == 2L)
  expect_identical(cdm$my_cohort_2 |>
                 dplyr::pull("cohort_start_date"), as.Date("2018-01-03"))
  expect_identical(cdm$my_cohort_2 |>
                 dplyr::pull("cohort_end_date"), as.Date("2020-01-10"))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("multiple observation periods", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1)
  cdm_local$observation_period <- dplyr::tibble(
    "observation_period_id" = as.integer(1:7),
    "person_id" = as.integer(c(1, 1, 1, 2, 2, 3, 4)),
    "observation_period_start_date" = as.Date(c(
      "2000-01-01", "2001-01-01", "2003-01-01", "2001-01-01", "2002-01-01",
      "2000-01-01", "2000-01-01"
    )),
    "observation_period_end_date" =as.Date(c(
      "2000-12-20", "2002-01-01", "2005-01-01", "2001-12-31", "2004-01-01",
      "2004-01-01", "2003-01-01"
    )),
    "period_type_concept_id" = NA_integer_
  )
  cdm_local$cohort1 <- dplyr::tibble(
    "cohort_definition_id" = as.integer(c(1, 2, 1, 2)),
    "subject_id" = as.integer(c(1, 1, 1, 1)),
    "cohort_start_date" = as.Date(c(
      "2000-01-01", "2000-12-01", "2001-01-01", "2001-01-01"
    )),
    "cohort_end_date" =as.Date(c(
      "2000-12-20", "2000-12-20", "2001-04-01", "2001-12-30"
    ))
  )
  cdm <- cdm_local |> copyCdm()
  cdm$cohort1 <- cdm$cohort1 |> omopgenerics::newCohortTable()
  cdm$cohort1 <- cdm$cohort1 |> intersectCohorts(gap = 9999)
  expect_identical(collectCohort(cdm$cohort1, 1), dplyr::tibble(
      "subject_id" = as.integer(c(1, 1)),
      "cohort_start_date" = as.Date(c("2000-01-01", "2001-01-01")),
      "cohort_end_date" = as.Date(c("2000-12-20", "2001-12-30"))
    ))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("test indexes - postgres", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")
  skip_if(!testIndexes)

  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                       host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                       password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    write_schema = c(schema =  Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
                     prefix = "cc_"),
    achilles_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  )

  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_cohort",
                                   table = data.frame(cohort_definition_id = 1:2L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-02"),
                                                      cohort_end_date = as.Date("2009-01-03"),
                                                      other_date = as.Date("2009-01-01")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- intersectCohorts(cdm$my_cohort)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdm_disconnect(cdm = cdm)
})
