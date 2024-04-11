test_that("requiring presence in another table", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2)
  cdm_local$table <- dplyr::tibble(
    person_id = c(1, 3, 4),
    date_start = as.Date(c("2002-01-01", "2015-10-01", "2000-01-01")),
    date_end = as.Date(c("2002-01-01", "2015-10-01", "2000-01-01"))
  )
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort2 <-  requireTableIntersectFlag(x = cdm$cohort1,
                                            targetTable = "table",
                                            targetStartDate = "date_start",
                                            targetEndDate = "date_end",
                                            window = c(-Inf, Inf),
                                            name = "cohort2")

  expect_equal(cdm$cohort2 |> dplyr::pull("subject_id") |> sort(),
               cdm$cohort1 |> dplyr::pull("subject_id") |> sort())
  expect_equal(omopgenerics::attrition(cdm$cohort2)$reason,
               c("Initial qualifying events",
                 "In table table between -Inf & Inf days relative to cohort_start_date",
                 "Initial qualifying events",
                 "In table table between -Inf & Inf days relative to cohort_start_date"))

  cdm$cohort3 <-  requireTableIntersectFlag(x = cdm$cohort1,
                                            targetTable = "table",
                                            targetStartDate = "date_start",
                                            targetEndDate = "date_end",
                                            window = c(-Inf, 0),
                                            name = "cohort3")
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") == c(1,3,3)))
  expect_equal(omopgenerics::attrition(cdm$cohort3)$reason,
               c("Initial qualifying events",
                 "In table table between -Inf & 0 days relative to cohort_start_date",
                 "Initial qualifying events",
                 "In table table between -Inf & 0 days relative to cohort_start_date"))

  cdm$cohort4 <-  requireTableIntersectFlag(x = cdm$cohort1,
                                            targetTable = "table",
                                            targetStartDate = "date_start",
                                            targetEndDate = "date_end",
                                            window = c(-Inf, 0),
                                            censorDate = "cohort_end_date",
                                            name = "cohort4")
  expect_true(cdm$cohort4 |> dplyr::pull("subject_id") == 1)
  expect_true(cdm$cohort4 |> dplyr::pull("cohort_start_date") == "2003-06-15")
  expect_equal(omopgenerics::attrition(cdm$cohort4)$reason,
               c("Initial qualifying events",
                 "In table table between -Inf & 0 days relative to cohort_start_date, censoring at cohort_end_date",
                 "Initial qualifying events",
                 "In table table between -Inf & 0 days relative to cohort_start_date, censoring at cohort_end_date"))

  CDMConnector::cdm_disconnect(cdm)
})
