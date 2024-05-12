test_that("requireDateRange", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2) |>
    omock::mockCohort(tableName = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")
  cdm$cohort1 <- cdm$cohort1 %>%
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2011-01-01")))
  expect_true(all(cohortCount(cdm$cohort1)$number_records == c(0, 0)))
  expect_true(all(cohortCount(cdm$cohort1)$number_subjects == c(0, 0)))
  expect_true(cdm$cohort1 |> dplyr::tally() |> dplyr::pull("n") == 0)

  cdm$cohort1 <- cdm$cohort2 %>%
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2020-01-01")),
                       name = "cohort1")
  expect_true(cdm$cohort1 %>%
    dplyr::pull("subject_id") |> unique() == 3L)
  expect_true(all(cdm$cohort1 %>%
                dplyr::arrange(.data$cohort_start_date) %>%
                dplyr::pull("cohort_start_date") ==
                  c("2015-04-14", "2015-02-02", "2015-02-08", "2015-02-23")))

  # index date
  cdm$cohort3 <- cdm$cohort2 %>%
    dplyr::mutate(new_index_date = as.Date("2000-03-30")) %>%
    requireInDateRange(dateRange = as.Date(c("2000-01-01", "2001-01-01")),
                       name = "cohort3",
                       indexDate = "new_index_date")
  expect_equal(cdm$cohort3 |> dplyr::pull("cohort_start_date"),
               cdm$cohort2 |> dplyr::pull("cohort_start_date"))

  # 1 cohort id
  cdm$cohort4 <- cdm$cohort2 %>%
    requireInDateRange(dateRange = as.Date(c("2000-01-01", "2001-01-01")),
                       cohortId = 1,
                       name = "cohort4")
  expect_true(all(attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events",
                      "cohort_start_date between 2000-01-01 & 2001-01-01",
                      "Initial qualifying events")))
  expect_true(all(cohortCount(cdm$cohort4)$number_records == c(1,4)))
  expect_true(all(cohortCount(cdm$cohort4)$number_subjects == c(1,2)))
  expect_true(all(cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
                c("1993-01-06", "2000-03-06", "2015-02-02", "2015-02-08", "2015-02-23")))

  # expect error
  expect_error(requireInDateRange(cohort = "a"))
  expect_error(cdm$cohort1 %>%
    requireInDateRange(dateRange = as.Date(c("2010-01-01"))))
  expect_error(cdm$cohort1 %>%
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2010-01-01",
                                             "2009-01-01"))))
  expect_no_error(
    cdm$cohort1 %>%
      requireInDateRange(dateRange = as.Date(c(NA, "2010-01-01")))
  )
  expect_no_error(
    cdm$cohort1 %>%
      requireInDateRange(dateRange = as.Date(c("2010-01-01", NA)))
  )
  expect_error(cdm$cohort1 %>%
    requireInDateRange(dateRange = c("a", "b")))
  expect_error(
    cdm$cohort1 %>%
      requireInDateRange(dateRange = c(NA, NA))
  )
  expect_error(
    cdm$cohort1 %>%
      requireInDateRange(dateRange = as.Date(c("2010-01-01", "2010-01-01")), indexDate = "subject_id")
  )

  CDMConnector::cdm_disconnect(cdm)
})

test_that("trim cohort dates", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2) |>
    omock::mockCohort(tableName = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort1 <- cdm$cohort1 %>%
    trimToDateRange(dateRange = as.Date(c("2001-01-01", "2005-01-01")))

  expect_equal(sort(cdm$cohort1 %>%
    dplyr::pull("subject_id")), c(1, 1, 1, 1, 1))
  expect_true(all(cdm$cohort1 %>%
    dplyr::pull("cohort_start_date") ==
      c("2001-03-30", "2003-06-15", "2001-01-01", "2001-07-16", "2001-12-04")))
  expect_true(all(cdm$cohort1 %>%
                dplyr::pull("cohort_end_date") ==
                  c("2003-06-14", "2005-01-01", "2001-07-15", "2001-12-03", "2005-01-01")))

  # cohort id
  cdm$cohort3 <- cdm$cohort2 %>%
    trimToDateRange(dateRange = as.Date(c("2001-01-01", "2005-01-01")),
                    cohortId = 1,
                    name = "cohort3")
  expect_true(omopgenerics::cohortCount(cdm$cohort3)$number_records[1] == 1)
  expect_equal(sort(cdm$cohort3 %>%
                      dplyr::pull("subject_id")), c(1, 3, 3, 3, 4))
  expect_equal(omopgenerics::attrition(cdm$cohort3)$reason[
      omopgenerics::attrition(cdm$cohort3)$cohort_definition_id == 1],
      c("Initial qualifying events", "cohort_start_date >= 2001-01-01", "cohort_end_date <= 2005-01-01")
    )
  expect_equal(omopgenerics::attrition(cdm$cohort3)$reason[
    omopgenerics::attrition(cdm$cohort3)$cohort_definition_id == 2],
    "Initial qualifying events"
  )

CDMConnector::cdm_disconnect(cdm)
})
