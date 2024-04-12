test_that("requiring death", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2)
  cdm_local$death <- dplyr::tibble(
    person_id = c(1,3),
    death_date = as.Date(c("2013-06-29", "2015-10-11")),
    death_type_concept_id = NA
  )
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort3 <-  requireDeathFlag(x = cdm$cohort1,
                                   window = c(0, Inf),
                                   name = "cohort3")
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") %in% c(1,3)))
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(subject_id %in% c(1,3)) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date")))

# censor
  cdm$cohort4 <-  requireDeathFlag(x = cdm$cohort1,
                                   window = c(0, Inf),
                                   censorDate = "cohort_end_date",
                                   name = "cohort4")
  expect_true(cdm$cohort4 |> dplyr::tally() |> dplyr::pull() == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date",
                      "Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date")))


  # index date
  cdm$cohort5 <-  requireDeathFlag(x = cdm$cohort1,
                                   window = c(0, 365),
                                   indexDate = "cohort_end_date",
                                   name = "cohort5")
  expect_true(all(cdm$cohort5 |> dplyr::pull("subject_id") %in% 3))
  expect_true(all(
    cdm$cohort5 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("2015-03-05", "2015-03-25")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events", "Death between 0 & 365 days relative to cohort_end_date",
                      "Initial qualifying events", "Death between 0 & 365 days relative to cohort_end_date")))
  # name
  cdm$cohort1 <-  requireDeathFlag(x = cdm$cohort1)
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "Death between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Death between 0 & Inf days relative to cohort_start_date")))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("not death", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2, seed = 3)
  cdm_local$death <- dplyr::tibble(
    person_id = c(1,3),
    death_date = as.Date(c("2013-06-29", "2015-10-11")),
    death_type_concept_id = NA
  )
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort3 <-  requireDeathFlag(x = cdm$cohort1,
                                   window = c(0, Inf),
                                   name = "cohort3",
                                   negate = TRUE)

  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") %in% c(2,4)))
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(subject_id %in% c(2,4)) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events", "Alive between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events", "Alive between 0 & Inf days relative to cohort_start_date")))

  CDMConnector::cdm_disconnect(cdm)
})
