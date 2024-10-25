test_that("requireDateRange", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2,seed = 1) |>
    omock::mockCohort(name = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- cdm_local |> copyCdm()

  # empty result
  cdm$cohort1 <- cdm$cohort1 |>
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2011-01-01")))
  expect_true(all(cohortCount(cdm$cohort1)$number_records == c(0, 0)))
  expect_true(all(cohortCount(cdm$cohort1)$number_subjects == c(0, 0)))
  expect_true(cdm$cohort1 |> dplyr::tally() |> dplyr::pull("n") == 0)

  cdm$cohort1 <- cdm$cohort2 |>
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2020-01-01")),
                       name = "cohort1")
  expect_true(cdm$cohort1 |>
                dplyr::pull("subject_id") |> unique() == 3L)
  expect_true(all(cdm$cohort1 |>
                    dplyr::arrange(.data$cohort_start_date) |>
                    dplyr::pull("cohort_start_date") ==
                    c("2015-01-25", "2015-02-02")))

  # index date
  cdm$cohort3 <- cdm$cohort2 |>
    dplyr::mutate(new_index_date = as.Date("2000-03-30")) |>
    requireInDateRange(dateRange = as.Date(c("2000-01-01", "2001-01-01")),
                       name = "cohort3",
                       indexDate = "new_index_date")
  expect_identical(cdm$cohort3 |> dplyr::pull("cohort_start_date"), cdm$cohort2 |> dplyr::pull("cohort_start_date"))

  # 1 cohort id
  cdm$cohort4 <- cdm$cohort2 |>
    requireInDateRange(dateRange = as.Date(c("2000-01-01", "2001-01-01")),
                       cohortId = 1,
                       name = "cohort4")
  expect_true(all(attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events",
                      "cohort_start_date after 2000-01-01",
                      "cohort_start_date before 2001-01-01",
                      "Initial qualifying events")))
  expect_true(all(cohortCount(cdm$cohort4)$number_records == c(2,4)))
  expect_true(all(cohortCount(cdm$cohort4)$number_subjects == c(1,3)))
  expect_true(all(cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-07-11", "2000-01-11", "2000-05-28", "2000-06-17", "2004-12-12", "2015-02-02")))
  # NA
  expect_no_error(
    cdm$cohort5 <- cdm$cohort2 |>
      requireInDateRange(dateRange = as.Date(c(NA, "2010-01-01")), name = "cohort5")
  )
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-07-11", "2000-01-11", "2000-05-28", "2000-06-17", "2003-05-08", "2004-12-12")))
  expect_true(all(attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events",
                      "cohort_start_date before 2010-01-01",
                      "Initial qualifying events",
                      "cohort_start_date before 2010-01-01")))

  expect_no_error(
    cdm$cohort6 <- cdm$cohort2 |>
      requireInDateRange(dateRange = as.Date(c("2000-01-01", NA)), name = "cohort6")
  )
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2000-01-11", "2000-05-28", "2000-06-17", "2003-05-08", "2004-12-12", "2015-01-25", "2015-02-02")))
  expect_true(all(attrition(cdm$cohort6)$reason ==
                    c("Initial qualifying events",
                      "cohort_start_date after 2000-01-01",
                      "Initial qualifying events",
                      "cohort_start_date after 2000-01-01")))
  expect_no_error(
    cdm$cohort7 <- cdm$cohort2 |>
      requireInDateRange(dateRange = as.Date(c(NA, NA)), name = "cohort7")
  )
  expect_identical(cdm$cohort7 |> dplyr::collect(), cdm$cohort2 |> dplyr::collect())

  # expect error
  expect_error(requireInDateRange(cohort = "a"))
  expect_error(cdm$cohort1 |>
                 requireInDateRange(dateRange = as.Date(c("2010-01-01"))))
  expect_error(cdm$cohort1 |>
                 requireInDateRange(dateRange = as.Date(c("2010-01-01", "2010-01-01",
                                                          "2009-01-01"))))
  expect_error(cdm$cohort1 |>
                 requireInDateRange(dateRange = c("a", "b")))
  expect_error(
    cdm$cohort1 |>
      requireInDateRange(dateRange = as.Date(c("2010-01-01", "2010-01-01")), indexDate = "subject_id")
  )
  expect_error(
    cdm$cohort1 |>
      requireInDateRange(dateRange = as.Date(c("2011-01-01", "2010-01-01")))
  )

  PatientProfiles::mockDisconnect(cdm)
})

test_that("trim cohort dates", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 1) |>
    omock::mockCohort(name = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- cdm$cohort1 |>
    trimToDateRange(dateRange = as.Date(c("2001-01-01", "2005-01-01")))

  expect_identical(sort(cdm$cohort1 |>
                      dplyr::pull("subject_id")), as.integer(c(1, 1, 1, 1, 1, 1, 2)))
  expect_true(all(cdm$cohort1 |>
                    dplyr::pull("cohort_start_date") ==
                    c("2003-05-17", "2004-03-11", "2001-01-01", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13")))
  expect_true(all(cdm$cohort1 |>
                    dplyr::pull("cohort_end_date") ==
                    c("2004-03-10", "2005-01-01", "2001-06-15", "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-01")))

  # cohort id
  cdm$cohort3 <- cdm$cohort2 |>
    trimToDateRange(dateRange = as.Date(c("2001-01-01", "2005-01-01")),
                    cohortId = "cohort_1",
                    name = "cohort3")
  expect_true(omopgenerics::cohortCount(cdm$cohort3)$number_records[1] == 2)
  expect_identical(sort(cdm$cohort3 |>
                      dplyr::pull("subject_id")), as.integer(c(1, 1, 1, 2, 2, 3)))
  expect_identical(omopgenerics::attrition(cdm$cohort3)$reason[
    omopgenerics::attrition(cdm$cohort3)$cohort_definition_id == 1], c("Initial qualifying events", "cohort_start_date >= 2001-01-01", "cohort_end_date <= 2005-01-01"))
  expect_identical(omopgenerics::attrition(cdm$cohort3)$reason[
    omopgenerics::attrition(cdm$cohort3)$cohort_definition_id == 2], "Initial qualifying events")

  # NA
  cdm$cohort4 <- cdm$cohort2 |>
    trimToDateRange(dateRange = as.Date(c(NA, "2005-01-01")),
                    cohortId = 1,
                    name = "cohort4")
  expect_identical(sort(cdm$cohort4 |> dplyr::pull("cohort_end_date")), as.Date(c("2000-05-27", "2001-09-08", "2002-03-26", "2004-12-11", "2005-01-01", "2007-09-06", "2015-08-12")))
  expect_identical(omopgenerics::attrition(cdm$cohort4)$reason, c("Initial qualifying events", "cohort_end_date <= 2005-01-01", "Initial qualifying events"))

  cdm$cohort5 <- cdm$cohort2 |>
    trimToDateRange(dateRange = as.Date(c("2005-01-01", NA)),
                    cohortId = 1,
                    name = "cohort5")
  expect_identical(sort(cdm$cohort5 |> dplyr::pull("cohort_start_date")), as.Date(c("1999-07-11", "2000-06-17", "2004-12-12", "2005-01-01", "2015-01-25", "2015-02-02")))
  expect_identical(omopgenerics::attrition(cdm$cohort5)$reason, c("Initial qualifying events", "cohort_start_date >= 2005-01-01", "Initial qualifying events"))

  cdm$cohort6 <- cdm$cohort2 |>
    trimToDateRange(dateRange = as.Date(c(NA, NA)),
                    cohortId = 1,
                    name = "cohort6")
  expect_identical(cdm$cohort6 |> dplyr::collect(), cdm$cohort2 |> dplyr::collect())

  PatientProfiles::mockDisconnect(cdm)
})
