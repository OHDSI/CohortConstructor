test_that("sampleCohort subsetting one cohort", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- CDMConnector::copyCdmTo(con = DBI::dbConnect(duckdb::duckdb()),
                                 cdm = cdm_local,
                                 schema = "main",
                                 overwrite = TRUE)

  cdm$cohort1 <- sampleCohorts(cdm$cohort1, 1, 2)
  expect_true(cdm$cohort1 |>
                dplyr::filter(cohort_definition_id == 1) |>
                dplyr::pull("subject_id") |>
                unique() |> length() == 2)
  expect_true(attrition(cdm$cohort1) |>
                dplyr::filter(reason == "Sample 2 individuals") |>
                dplyr::pull("cohort_definition_id") == 1)

  # Subset it again should yield the same cohort
  test_cohort1 <- sampleCohorts(cdm$cohort1, 1, 2)
  expect_true(all.equal(test_cohort1, cdm$cohort1))
  expect_true(attrition(test_cohort1) |>
                dplyr::filter(reason == "Sample 2 individuals" & reason_id == 3) |>
                dplyr::pull("excluded_subjects") == 0)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("sampleCohort subsetting multiple cohorts", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 3, seed = 2)
  cdm <- CDMConnector::copyCdmTo(con = DBI::dbConnect(duckdb::duckdb()),
                                 cdm = cdm_local,
                                 schema = "main",
                                 overwrite = TRUE)

  cdm$cohort1 <- sampleCohorts(cdm$cohort1, n = 5)
  expect_true(all(attrition(cdm$cohort1) |>
                dplyr::filter(reason == "Sample 5 individuals") |>
                dplyr::arrange(cohort_definition_id) |>
                dplyr::pull("number_subjects") == c(5,5,4)))

  # Subset it again but only cohorts 1 and 3
  cdm$cohort2 <- sampleCohorts(cdm$cohort1, c(1,3), 4, name = "cohort2")
  expect_true(all.equal(cdm$cohort2 |> dplyr::filter(cohort_definition_id == 3) |>
                          dplyr::collect() |> dplyr::arrange(subject_id, cohort_start_date),
                        cdm$cohort1 |> dplyr::filter(cohort_definition_id == 3) |>
                          dplyr::collect() |> dplyr::arrange(subject_id, cohort_start_date)))

  expect_true(all(attrition(cdm$cohort2) |>
                dplyr::filter(reason == "Sample 4 individuals") |>
                dplyr::arrange(cohort_definition_id) |>
                dplyr::pull("excluded_subjects") == c(1,0)))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("sampleCohort subsetting all cohorts", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 3, seed = 2)
  cdm <- CDMConnector::copyCdmTo(con = DBI::dbConnect(duckdb::duckdb()),
                                 cdm = cdm_local,
                                 schema = "main",
                                 overwrite = TRUE)

  test1 <- sampleCohorts(cdm$cohort1, n = 2)
  test2 <- sampleCohorts(cdm$cohort1, cohortId = c(1,2,3), n = 2)
  expect_true(all.equal(test1, test2))
  test3 <- sampleCohorts(cdm$cohort1, cohortId = paste0("cohort_", c(1,2,3)), n = 2)
  expect_true(all.equal(test1, test3))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("expected errors", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 3, seed = 2)
  cdm <- CDMConnector::copyCdmTo(con = DBI::dbConnect(duckdb::duckdb()),
                                 cdm = cdm_local,
                                 schema = "main",
                                 overwrite = TRUE)

  expect_error(sampleCohorts(cdm$cohort2, n = 10))
  expect_error(sampleCohorts(cdm_local$cohort1, n = 10))
  expect_error(sampleCohorts(cdm$cohort1, cohortId = 4, n = 10))
  expect_error(sampleCohorts(cdm$cohort1, cohortId = "1", n = 10))
  expect_error(sampleCohorts(cdm$cohort1, n = -1))
  expect_error(sampleCohorts(cdm$cohort1))
  expect_error(sampleCohorts(cdm$cohort1, n = c(1,2)))
  expect_error(cdm$cohort2 <- sampleCohorts(cdm$cohort1, n = 10))

  PatientProfiles::mockDisconnect(cdm)
})
