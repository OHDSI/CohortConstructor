test_that("test restrict to first entry works", {
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = "cohort1", numberCohorts = 1, recordPerson = 2) |>
    omock::mockCohort(name = "cohort2", numberCohorts = 2, recordPerson = 2)

  expect_true(all(cdm$cohort1 |> CohortConstructor::requireIsFirstEntry() |>
    dplyr::pull(cohort_start_date) == c("2001-05-29", "1999-07-30", "2015-01-23")))

  expect_true(all(cdm$cohort1 |> CohortConstructor::requireIsFirstEntry() |>
                    dplyr::pull(subject_id) %in% 1:3))

  expect_true(all(cdm$cohort2 |> CohortConstructor::requireIsFirstEntry() |>
                    dplyr::pull(cohort_start_date) ==
                    c("2001-05-29", "1999-07-30", "2015-01-23", "2002-10-09", "1999-04-16")))

  expect_true(all(cdm$cohort2 |> CohortConstructor::requireIsFirstEntry() |>
                    dplyr::pull(subject_id) == c(1:3, 1:2)))

})

test_that("requireIsFirstEntry, cohortIds & name arguments", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3, recordPerson = 2)

  expect_no_error(
    cdm$new_cohort <- CohortConstructor::requireIsFirstEntry(
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
  expect_true(all(
    omopgenerics::attrition(cdm$new_cohort)$reason  ==
      c("Initial qualifying events", "Restricted to first entry",
        "Initial qualifying events", "Initial qualifying events")
  ))

})

test_that("requireIsFirstEntry, index date + errors", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 1, recordPerson = 2)

  cdm$cohort <- cdm$cohort |>
    dplyr::mutate("index_date" = dplyr::if_else(
      cohort_start_date == max(cohort_start_date),
      min(cohort_start_date),
      cohort_start_date))

  expect_true(all(cdm$cohort |> requireIsFirstEntry(indexDate = "index_date") |>
                    dplyr::pull(index_date) == c("2001-05-29", "1999-07-30", "1999-07-30")))

  expect_error(cdm$cohort |> requireIsFirstEntry(indexDate = "a"))
  expect_error(cdm$cohort |> requireIsFirstEntry(name = 1))
  expect_error(cdm$cohort1 <- cdm$cohort |> requireIsFirstEntry(name = "cohort2"))
  expect_error(cdm$cohort |> requireIsFirstEntry(cohortId = Inf))
  expect_error(cdm$cohort |> dplyr::collect() |> requireIsFirstEntry())
  expect_warning(cdm$cohort |> requireIsFirstEntry(cohortId = c(1, 5)))
})

test_that("requireIsLastEntry", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3, recordPerson = 2)

  cdm$new_cohort <- CohortConstructor::requireIsLastEntry(
    cohort = cdm$cohort,
    cohortId = 1,
    name = "new_cohort")

  counts <- omopgenerics::cohortCount(cdm$cohort)
  counts_new <- omopgenerics::cohortCount(cdm$new_cohort)

  expect_equal(counts |> dplyr::filter(cohort_definition_id %in% 2:3),
               counts_new |> dplyr::filter(cohort_definition_id %in% 2:3))
  expect_false(counts |> dplyr::filter(cohort_definition_id == 1) %>% dplyr::pull(number_records) ==
                 counts_new |> dplyr::filter(cohort_definition_id == 1) %>% dplyr::pull(number_records))
  expect_equal(counts_new |> dplyr::filter(cohort_definition_id == 1),
               dplyr::tibble(cohort_definition_id = 1, number_records = 3, number_subjects = 3))
  expect_true(all(cdm$new_cohort |>  dplyr::pull(cohort_start_date) ==
                    c("2004-01-08", "1999-07-30", "2015-03-11", "2002-10-09", "2003-09-12",
                      "1999-04-16", "2000-03-09", "2000-05-05", "2015-02-22", "2015-03-24",
                      "2002-09-28", "1999-08-25", "1999-12-14", "1999-12-24")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(cohort_end_date) ==
                    c("2009-10-03", "2001-10-02", "2015-04-13", "2003-09-11", "2009-03-19",
                      "2000-03-08", "2000-05-04", "2001-04-01", "2015-03-23", "2015-06-11",
                      "2008-10-13", "1999-12-13", "1999-12-23", "2000-04-01")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(subject_id) ==
                    c(1, 2, 3, 1, 1, 2, 2, 2, 3, 3, 1, 2, 2, 2)))
  expect_true(all(omopgenerics::attrition(cdm$new_cohort)$reason == c(
    c("Initial qualifying events", "Restricted to last entry", "Initial qualifying events",
      "Initial qualifying events"))
  ))

  # index date and name
  cdm$cohort <- cdm$cohort |>
    dplyr::mutate(
      new_index = dplyr::if_else(cohort_start_date == as.Date("2004-01-08"),
                                 as.Date("1999-01-01"), cohort_start_date)
    )

  cdm$cohort <- CohortConstructor::requireIsLastEntry(cohort = cdm$cohort,
                                                       indexDate = "new_index")
  expect_true(all(cdm$cohort |> dplyr::pull(subject_id) == c(1:3, 1:3, 1:2)))
  expect_true(all(cdm$cohort |>  dplyr::pull(cohort_start_date) ==
                    c("2002-10-24", "1999-07-30", "2015-03-11", "2003-09-12", "2000-05-05",
                      "2015-03-24", "2002-09-28", "1999-12-24")))
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_end_date) ==
                    c("2004-01-07", "2001-10-02", "2015-04-13", "2009-03-19", "2001-04-01",
                      "2015-06-11", "2008-10-13", "2000-04-01")))
  expect_true(all(omopgenerics::attrition(cdm$cohort)$reason == c(
    c("Initial qualifying events", "Restricted to last entry",
      "Initial qualifying events", "Restricted to last entry",
      "Initial qualifying events", "Restricted to last entry")
  )))

  # errors
  expect_error(cdm$cohort |> requireIsLastEntry(indexDate = "a"))
  expect_error(cdm$cohort |> requireIsLastEntry(name = 1))
  expect_error(cdm$cohort1 <- cdm$cohort |> requireIsLastEntry(name = "cohort2"))
  expect_error(cdm$cohort |> requireIsLastEntry(cohortId = Inf))
  expect_error(cdm$cohort |> dplyr::collect() |> requireIsLastEntry())
  expect_warning(cdm$cohort |> requireIsLastEntry(cohortId = c(1, 5)))
})
