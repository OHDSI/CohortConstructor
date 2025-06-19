test_that("test renameCohort", {
  testthat::skip_on_cran()

  cdm <- omock::mockPerson(nPerson = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "cohort", numberCohorts = 3, cohortName = c("c_1", "c_2", "c_3")
    ) |>
    copyCdm()

  # one cohort
  expect_no_error(
    cdm$cohort <- cdm$cohort |>
      renameCohort(cohortId = 1, newCohortName = "my_cohort")
  )
  expect_identical(
    omopgenerics::settings(cdm$cohort),
    dplyr::tibble(
      cohort_definition_id = c(1L, 2L, 3L),
      cohort_name = c("my_cohort", "c_2", "c_3")
    )
  )

  # multiple cohorts
  expect_no_error(
    cdm$cohort <- cdm$cohort |>
      renameCohort(
        cohortId = c(1, 2),
        newCohortName = c("covid", "tb")
      )
  )
  expect_identical(
    omopgenerics::settings(cdm$cohort),
    dplyr::tibble(
      cohort_definition_id = c(1L, 2L, 3L),
      cohort_name = c("covid", "tb", "c_3")
    )
  )

  # multiple cohorts not sorted
  expect_no_error(
    cdm$cohort <- cdm$cohort |>
      renameCohort(
        cohortId = c(3, 2),
        newCohortName = c("xx", "st")
      )
  )
  expect_identical(
    omopgenerics::settings(cdm$cohort),
    dplyr::tibble(
      cohort_definition_id = c(1L, 2L, 3L),
      cohort_name = c("covid", "st", "xx")
    )
  )

  # expect error if present name
  expect_error(
    cdm$cohort <- cdm$cohort |>
      renameCohort(cohortId = 1, newCohortName = "xx")
  )

  # expect error if different lengths
  expect_error(
    cdm$cohort <- cdm$cohort |>
      renameCohort(cohortId = 1, newCohortName = c("name1", "name2"))
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  CDMConnector::cdmDisconnect(cdm = cdm)
})
