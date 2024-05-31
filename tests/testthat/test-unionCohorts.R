test_that("unionCohorts works", {

    cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 4)
  cdm <- cdm_local |> copyCdm()
  # simple example
  cdm$cohort2 <- unionCohorts(cdm$cohort1, name = "cohort2")
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1994-02-19", "1999-08-24", "2000-06-23", "2015-01-19")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("2001-08-26", "2006-09-27", "2007-08-06", "2015-09-14")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("subject_id") %>% sort() == 1:4
  ))
  expect_true(all(attrition(cdm$cohort2) ==
                    dplyr::tibble(
                      cohort_definition_id = 1,
                      number_records = 4,
                      number_subjects = 4,
                      reason_id = 1,
                      reason = "Initial qualifying events",
                      excluded_records = 0,
                      excluded_subjects = 0
                    )))
  expect_true(settings(cdm$cohort2)$cohort_name == "cohort_1_cohort_2_cohort_3_cohort_4")

  # choose cohort Id
  cdm$cohort3 <- unionCohorts(cdm$cohort1, cohortId = 1:2, name = "cohort3")
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1997-10-22", "2000-06-23", "2015-03-05")
  ))
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1999-05-28", "2006-09-27", "2015-07-06")
  ))
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("subject_id") %>% sort() == c(1, 3, 4)
  ))
  expect_true(all(attrition(cdm$cohort3) ==
                    dplyr::tibble(
                      cohort_definition_id = 1,
                      number_records = 3,
                      number_subjects = 3,
                      reason_id = 1,
                      reason = "Initial qualifying events",
                      excluded_records = 0,
                      excluded_subjects = 0
                    )))
  expect_true(settings(cdm$cohort3)$cohort_name == "cohort_1_cohort_2")

  PatientProfiles::mockDisconnect(cdm)
})

test_that("gap and name works", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 4, seed = 11, recordPerson = 2)
  cdm_local$cohort <- cdm_local$cohort |>
    dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::filter(id %% 2 == 0) |>
    dplyr::select(-id)
  cdm_local$cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2),
    subject_id = c(1, 1, 1),
    cohort_start_date = as.Date(c("2000-07-01", "2000-07-10", "2000-07-22")),
    cohort_end_date = as.Date(c("2000-07-02", "2000-07-20", "2000-08-22"))
  )
  cdm <- cdm_local |> copyCdm()
  cdm$cohort1 <- cdm$cohort1 |> omopgenerics::newCohortTable()

  # gap
  cdm$cohort2 <- unionCohorts(cdm$cohort1, gap = 2,  name = "cohort2")
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("2000-07-01", "2000-07-10")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("2000-07-02", "2000-08-22")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("subject_id") %>% sort() == 1
  ))
  expect_true(all(attrition(cdm$cohort2) ==
                    dplyr::tibble(
                      cohort_definition_id = 1,
                      number_records = 2,
                      number_subjects = 1,
                      reason_id = 1,
                      reason = "Initial qualifying events",
                      excluded_records = 0,
                      excluded_subjects = 0
                    )))
  expect_true(settings(cdm$cohort2)$cohort_name == "cohort_1_cohort_2")


   # names
  cdm$cohort <- unionCohorts(cdm$cohort, gap = 2,  cohortName = "test")
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1991-10-17", "1991-12-11", "1993-02-17", "1999-04-12", "1999-08-23",
        "1999-09-19", "2001-12-27", "2015-03-02", "2015-03-17")
  ))
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1991-10-19", "1992-10-12", "1999-07-18", "1999-08-24", "2001-03-19",
        "2001-06-05", "2008-08-28", "2015-03-09", "2015-03-25")
  ))
  expect_true(all(
    cdm$cohort %>% dplyr::pull("subject_id") %>% sort() == c(1, rep(2, 3), rep(3, 2), rep(4, 3))
  ))
  expect_true(all(attrition(cdm$cohort) ==
                    dplyr::tibble(
                      cohort_definition_id = 1,
                      number_records = 9,
                      number_subjects = 4,
                      reason_id = 1,
                      reason = "Initial qualifying events",
                      excluded_records = 0,
                      excluded_subjects = 0
                    )))
  expect_true(settings(cdm$cohort)$cohort_name == "test")

  PatientProfiles::mockDisconnect(cdm)
})

test_that("Expected behaviour", {
  testthat::skip_on_cran()
   cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 4, seed = 8, recordPerson = 2)
  cdm <- cdm_local |> copyCdm()
  expect_warning(
    cohort <- unionCohorts(cdm$cohort,
                          cohortId = 1,
                          gap = 0,
                          cohortName = NULL,
                          name = "cohort1")
  )
  expect_true(cohort |> dplyr::anti_join(cdm$cohort, by = colnames(cohort)) |>
                dplyr::tally() |> dplyr::pull("n") == 0)
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                          cohortId = NULL,
                          gap = -1,
                          cohortName = NULL,
                          name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                          cohortId = NULL,
                          gap = NA,
                          cohortName = NULL,
                          name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                          cohortId = NULL,
                          gap = Inf,
                          cohortName = NULL,
                          name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                          cohortId = "1",
                          gap = 1,
                          cohortName = NULL,
                          name = "cohort1")
  )
  expect_warning(
    cohort <- unionCohorts(cdm$cohort,
                          cohortId = NULL,
                          gap = 1,
                          cohortName = "hOLA",
                          name = "cohort1")
  )

  PatientProfiles::mockDisconnect(cdm)
})

test_that("test codelist", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort()
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
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 4, 4, 1, 2, 3, 4),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, 430, -10, 100, 123, -10, 1000),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, 400, -100, NA, 190, 123, 1500),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )
  cdm_local$observation_period <- cdm_local$observation_period|>
    dplyr::mutate(observation_period_start_date = as.Date("1990-01-01"), observation_period_end_date = as.Date("2020-01-01"))

  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- conceptCohort(cdm, conceptSet = list(c1 = c(1,3), c2 = c(2)), name = "cohort1")

  # Union concept generated cohort
  cdm$cohort2 <- unionCohorts(cdm$cohort1, name = "cohort2")
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("2009-12-22", "2010-01-01", "2010-01-11", "2010-05-31", "2012-09-27", "2014-12-06")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("2010-05-04", "2011-08-24", "2014-02-09", "2014-05-20", "2014-12-10", "2015-06-24")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("subject_id") %>% sort() == c(1, 1, 2, 3, 3, 4)
  ))
  codes <- attr(cdm$cohort2, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  # union concept + non concept cohorts
  cdm <- omopgenerics::bind(cdm$cohort, cdm$cohort1, name = "cohort3")
  cdm$cohort4 <- unionCohorts(cdm$cohort3, name = "cohort4")
  expect_true(all(
    cdm$cohort4 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1997-10-22", "2001-03-30", "2003-06-15", "2009-12-22", "2010-01-01", "2010-01-11", "2010-05-31", "2012-09-27", "2014-12-06", "2015-03-25")
  ))
  expect_true(all(
    cdm$cohort4 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1999-05-28", "2003-06-14", "2005-11-23", "2010-05-04", "2011-08-24", "2014-02-09", "2014-05-20", "2014-12-10", "2015-04-14", "2015-06-24")
  ))
  expect_true(all(
    cdm$cohort4 %>% dplyr::pull("subject_id") %>% sort() == c(1, 1, 1, 1, 2, 3, 3, 3, 4, 4)
  ))
  codes <- attr(cdm$cohort4, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  PatientProfiles::mockDisconnect(cdm)
})
