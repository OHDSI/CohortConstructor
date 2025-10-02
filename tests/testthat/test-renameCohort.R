test_that("test renameCohort", {
  skip_on_cran()

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

  # rename multiple in one go
  cdm <- omock::mockPerson(nPerson = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "cohort", numberCohorts = 3,
      cohortName = c("cohort_1",
                     "cohort_2",
                     "cohort_3")
    ) |>
    copyCdm()
  expect_identical(cdm$cohort |>
    renameCohort(newCohortName = "{cohort_name}_new")|>
    settings() |>
    dplyr::pull("cohort_name"),
  c("cohort_1_new",
    "cohort_2_new",
    "cohort_3_new"))

  cdm <- omock::mockPerson(nPerson = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "cohort", numberCohorts = 3,
      cohortName = c("cohort_1",
                     "cohort_2",
                     "cohort_3")
    ) |>
    copyCdm()
  expect_identical(cdm$cohort |>
                     renameCohort(newCohortName = "updated_{cohort_name}_new_{cohort_name}")|>
                     settings() |>
                     dplyr::pull("cohort_name"),
                   c("updated_cohort_1_new_cohort_1",
                     "updated_cohort_2_new_cohort_2",
                     "updated_cohort_3_new_cohort_3"))

# no need to specify cohort id if length equals number of cohorts
  cdm <- omock::mockPerson(nPerson = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "cohort", numberCohorts = 1,
      cohortName = c("cohort_1")
    ) |>
    copyCdm()
  expect_identical(cdm$cohort |>
                     renameCohort(newCohortName = "new_name")|>
                     settings() |>
                     dplyr::pull("cohort_name"),
                   c("new_name"))

  cdm <- omock::mockPerson(nPerson = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "cohort", numberCohorts = 3,
      cohortName = c("cohort_1",
                     "cohort_2",
                     "cohort_3")
    ) |>
    copyCdm()
  expect_identical(cdm$cohort |>
                     renameCohort(newCohortName = c("a",
                                                    "b",
                                                    "c"))|>
                     settings() |>
                     dplyr::pull("cohort_name"),
                   c(c("a",
                       "b",
                       "c")))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})
