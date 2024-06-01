test_that("yearCohorts - change name", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort"))
  cdm <- cdm_local |> copyCdm()

  # simple example
  cdm$cohort1 <- yearCohorts(cohort = cdm$cohort,
                             years = 1997:2002,
                             cohortId = NULL,
                             name = "cohort1")
  expect_equal(settings(cdm$cohort1) |> dplyr::arrange(.data$cohort_definition_id),
               dplyr::tibble(
                 cohort_definition_id = 1:6,
                 cohort_name = paste0("cohort_1_", 1997:2002),
                 target_cohort_definition_id = 1,
                 year = 1997:2002,
                 target_cohort_name = "cohort_1"
               ))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1997-10-22", "1998-01-01", "1999-01-01", "2001-03-30", "2002-01-01")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1997-12-31", "1998-12-31", "1999-05-28", "2001-12-31", "2002-12-31")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 4, 4, 4)))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_definition_id") |> sort() == c(1:3, 5:6)))
  expect_true(all(attrition(cdm$cohort1)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 1997-01-01 and 1997-12-31',
    'Initial qualifying events', 'Restrict to observations between: 1998-01-01 and 1998-12-31',
    'Initial qualifying events', 'Restrict to observations between: 1999-01-01 and 1999-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2000-01-01 and 2000-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2001-01-01 and 2001-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2002-01-01 and 2002-12-31'
  )))

 # more than 1 cohort
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 3)
  cdm <- cdm_local |> copyCdm()
  # all cohorts
  cdm$cohort1 <- yearCohorts(cohort = cdm$cohort,
                            years = 2005:2008,
                            cohortId = NULL,
                            name = "cohort1")
  expect_equal(settings(cdm$cohort1) |> dplyr::arrange(.data$cohort_definition_id),
               dplyr::tibble(
                 cohort_definition_id = 1:12,
                 cohort_name = c(
                   "cohort_1_2005", "cohort_2_2005", "cohort_3_2005", "cohort_1_2006",
                   "cohort_2_2006", "cohort_3_2006", "cohort_1_2007", "cohort_2_2007",
                   "cohort_3_2007", "cohort_1_2008", "cohort_2_2008", "cohort_3_2008"
                 ),
                 target_cohort_definition_id = c(1:3, 1:3, 1:3, 1:3),
                 year = c(rep(2005, 3), rep(2006, 3), rep(2007, 3), rep(2008, 3)),
                 target_cohort_name = rep(paste0("cohort_", 1:3), 4)
               ))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2005-01-01", "2005-01-01", "2005-01-01", "2006-01-01", "2006-01-01", "2007-01-01")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("2005-11-23", "2005-12-31", "2005-12-31", "2006-09-27", "2006-12-31", "2007-08-06")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 1, 4, 4, 4)))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_definition_id") |> sort() == c(1:3, 5:6, 9)))
  expect_true(all(attrition(cdm$cohort1)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31'
  )))
  expect_true(all(cohortCount(cdm$cohort1)$number_records == c(1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0)))

  # just 1 cohort
  cdm$cohort1 <- yearCohorts(cohort = cdm$cohort,
                             years = 2005:2008,
                             cohortId = 1,
                             name = "cohort1")
  expect_equal(settings(cdm$cohort1) |> dplyr::arrange(.data$cohort_definition_id),
               dplyr::tibble(
                 cohort_definition_id = 1:4,
                 cohort_name = c(
                   paste0("cohort_1_", 2005:2008)
                 ),
                 target_cohort_definition_id = c(1, 1, 1, 1),
                 year = c(2005:2008),
                 target_cohort_name = c(rep("cohort_1", 4))
               ))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2005-01-01")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("2005-11-23")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") |> sort() == 1))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_definition_id") |> sort() == 1))
  expect_true(all(attrition(cdm$cohort1)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31'
  )))
  expect_true(all(cohortCount(cdm$cohort1)$number_records == c(1, 0, 0, 0)))

  # no years in:
  cdm$cohort1 <- yearCohorts(cohort = cdm$cohort,
                             years = numeric(),
                             cohortId = 1,
                             name = "cohort1")
  expect_equal(cdm$cohort1 |> dplyr::collect(), cdm$cohort |> dplyr::collect())

  PatientProfiles::mockDisconnect(cdm)
})

test_that("yearCohorts - keep name", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort"))
  cdm <- cdm_local |> copyCdm()

  # simple example
  cdm$cohort <- yearCohorts(cohort = cdm$cohort,
                             years = 1997:2002,
                             cohortId = NULL)
  expect_equal(settings(cdm$cohort) |> dplyr::arrange(.data$cohort_definition_id),
               dplyr::tibble(
                 cohort_definition_id = 1:6,
                 cohort_name = paste0("cohort_1_", 1997:2002),
                 target_cohort_definition_id = 1,
                 year = 1997:2002,
                 target_cohort_name = "cohort_1"
               ))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1997-10-22", "1998-01-01", "1999-01-01", "2001-03-30", "2002-01-01")))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("1997-12-31", "1998-12-31", "1999-05-28", "2001-12-31", "2002-12-31")))
  expect_true(all(cdm$cohort |> dplyr::pull("subject_id") |> sort() == c(1, 1, 4, 4, 4)))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_definition_id") |> sort() == c(1:3, 5:6)))
  expect_true(all(attrition(cdm$cohort)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 1997-01-01 and 1997-12-31',
    'Initial qualifying events', 'Restrict to observations between: 1998-01-01 and 1998-12-31',
    'Initial qualifying events', 'Restrict to observations between: 1999-01-01 and 1999-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2000-01-01 and 2000-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2001-01-01 and 2001-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2002-01-01 and 2002-12-31'
  )))

  # more than 1 cohort
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 3)
  cdm <- cdm_local |> copyCdm()

  # just 1 cohort
  cdm$cohort <- yearCohorts(cohort = cdm$cohort,
                             years = 2005:2008,
                             cohortId = 1)
  expect_equal(settings(cdm$cohort) |> dplyr::arrange(.data$cohort_definition_id),
               dplyr::tibble(
                 cohort_definition_id = 1:4,
                 cohort_name = c(
                   paste0("cohort_1_", 2005:2008)
                 ),
                 target_cohort_definition_id = c(1, 1, 1, 1),
                 year = c(2005:2008),
                 target_cohort_name = c(rep("cohort_1", 4))
               ))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2005-01-01")))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("2005-11-23")))
  expect_true(all(cdm$cohort |> dplyr::pull("subject_id") |> sort() == c(1)))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_definition_id") |> sort() == c(1)))
  expect_true(all(attrition(cdm$cohort)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31'
  )))
  expect_true(all(cohortCount(cdm$cohort)$number_records == c(1, 0, 0, 0)))

  PatientProfiles::mockDisconnect(cdm)
})
