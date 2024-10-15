test_that("sampleCohort subsetting one cohort", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
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
    omock::mockPerson(n = 10,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 3, seed = 2)
  cdm <- CDMConnector::copyCdmTo(con = DBI::dbConnect(duckdb::duckdb()),
                                 cdm = cdm_local,
                                 schema = "main",
                                 overwrite = TRUE)

  cdm$cohort1a <- sampleCohorts(cdm$cohort1, n = 4, name = "cohort1a")
  expect_true(all(attrition(cdm$cohort1a) |>
                dplyr::filter(reason == "Sample 4 individuals") |>
                dplyr::arrange(cohort_definition_id) |>
                dplyr::pull("number_subjects") == c(4,4,4)))

  # Subset it again but only cohorts 1 and 3
  cdm$cohort1b <- sampleCohorts(cdm$cohort1, cohortId = c(1,3), n = 4, name = "cohort1b")
  expect_true(all(attrition(cdm$cohort1b) |>
                    dplyr::filter(reason == "Sample 4 individuals") |>
                    dplyr::arrange(cohort_definition_id) |>
                    dplyr::pull("number_subjects") == c(4,4)))
  expect_true(omopgenerics::cohortCount(cdm$cohort1b) |>
    dplyr::filter(cohort_definition_id == 2) |>
    dplyr::pull("number_records") == 10)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("sampleCohort subsetting all cohorts", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
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


test_that("original cohort attributes unchanged", {

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 20) |>
    omock::mockObservationPeriod()

  db <- DBI::dbConnect(duckdb::duckdb())
  cdm <- CDMConnector::copyCdmTo(con = db, cdm = cdm_local,
                                 schema ="main", overwrite = TRUE)

  cdm$cohort <- demographicsCohort(cdm, name = "cohort")
  cohort_before <- attrition(cdm$cohort)

  cdm$new_cohort <- cdm$cohort |>
    CohortConstructor::sampleCohorts(name = "new_cohort", n = 10)
  cohort_after <- attrition(cdm$cohort)

  expect_identical(cohort_before, cohort_after)

})
