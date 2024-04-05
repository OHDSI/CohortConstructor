test_that("test restrict to first entry works", {
  set.seed(123)
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = "cohort1", numberCohorts = 1, recordPerson = 2) |>
    omock::mockCohort(tableName = "cohort2", numberCohorts = 2, recordPerson = 2)

  expect_true(all(cdm$cohort1 |> CohortConstructor::restrictToFirstEntry() |>
    dplyr::pull(cohort_start_date) == c("2001-05-29", "1999-07-30", "2015-01-23")))

  expect_true(all(cdm$cohort1 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(subject_id) %in% 1:3))

  expect_true(all(cdm$cohort2 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(cohort_start_date) ==
                    c("2001-05-29", "1999-07-30", "2015-01-23", "2002-10-09", "1999-04-16")))

  expect_true(all(cdm$cohort2 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(subject_id) == c(1:3, 1:2)))

})

test_that("restrictToFirstEntry, cohortIds & name arguments", {
  set.seed(123)
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3, recordPerson = 2)

  expect_no_error(
    cdm$new_cohort <- CohortConstructor::restrictToFirstEntry(
      cohort = cdm$cohort,
      cohortId = 1,
      name = "new_cohort")
  )

  counts <- omopgenerics::cohortCount(cdm$cohort)
  counts_new <- omopgenerics::cohortCount(cdm$new_cohort)

  expect_equal(counts |> dplyr::filter(cohort_definition_id %in% 2:3),
               counts_new |> dplyr::filter(cohort_definition_id %in% 2:3))
  expect_false(counts |> dplyr::filter(cohort_definition_id == 1) %>% dplyr::pull(number_records) ==
               counts_new |> dplyr::filter(cohort_definition_id == 1) %>% dplyr::pull(number_records))
  expect_equal(counts_new |> dplyr::filter(cohort_definition_id == 1),
               dplyr::tibble(cohort_definition_id = 1, number_records = 3, number_subjects = 3))
  expect_true(all(cdm$new_cohort |>  dplyr::pull(cohort_start_date) ==
                    c("2001-05-29", "1999-07-30", "2015-01-23", "2002-10-09", "2003-09-12",
                      "1999-04-16", "2000-03-09", "2000-05-05", "2015-02-22", "2015-03-24",
                      "2002-09-28", "1999-08-25", "1999-12-14", "1999-12-24")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(cohort_end_date) ==
                    c("2002-10-23", "2001-10-02", "2015-02-16", "2003-09-11", "2009-03-19",
                      "2000-03-08", "2000-05-04", "2001-04-01", "2015-03-23", "2015-06-11",
                      "2008-10-13", "1999-12-13", "1999-12-23", "2000-04-01")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(subject_id) == c(1, 2, 3, 1, 1, 2, 2, 2, 3, 3, 1, 2, 2, 2)))

  expect_no_error(
    cdm$cohort <- CohortConstructor::restrictToFirstEntry(
      cohort = cdm$cohort,
      cohortId = NULL)
  )
  expect_true(all(cdm$cohort |> dplyr::pull(subject_id) == c(1:3, 1:3, 1:2)))
})


test_that("restrictToFirstEntry, index date", {
  set.seed(123)
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 1, recordPerson = 2)

  cdm$cohort <- cdm$cohort |>
    dplyr::mutate("index_date" = dplyr::if_else(
      cohort_start_date == max(cohort_start_date),
      min(cohort_start_date),
      cohort_start_date))

  expect_true(all(cdm$cohort |> restrictToFirstEntry(indexDate = "index_date") |>
                    dplyr::pull(index_date) == c("2001-05-29", "1999-07-30", "1999-07-30")))
})
