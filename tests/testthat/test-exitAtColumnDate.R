test_that("exit at first date", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 7) |>
    omock::mockObservationPeriod()
  cdm_local$cohort <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2, 3, 4, 4),
    cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
    cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
    other_date_1 = as.Date(c("2001-08-01", "2001-01-01", "2015-01-15", NA, "2002-12-09")),
    other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "1990-13-09", "2002-12-09"))
  )
  cdm_local$death <- dplyr::tibble(
    person_id = 1:3,
    death_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11")),
    death_type_concept_id = NA
  )
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))

  cdm$cohort <- cdm$cohort %>%
    omopgenerics::newCohortTable()

  # works
  cdm$cohort1 <- cdm$cohort |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      name = "cohort1"
    )
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1990-12-09", "2001-01-01", "2001-08-01", "2002-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("exit_reason") %>% sort() ==
      c('cohort_end_date', 'other_date_1', 'other_date_1',
        'other_date_2; cohort_end_date; other_date_1', 'other_date_2; other_date_1')
  ))
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "exit_reason")))

  # works with == name and no exit reason
  cdm$cohort <- cdm$cohort |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE
    )
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1990-12-09", "2001-01-01", "2001-08-01", "2002-12-09", "2015-01-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("exit at last date", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 7) |>
    omock::mockObservationPeriod()
  cdm_local$cohort <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2, 3, 4, 4),
    cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
    cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
    other_date_1 = as.Date(c("2001-08-01", "2001-01-01", "2015-01-15", NA, "2002-12-09")),
    other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "1990-13-09", "2002-12-09"))
  )
  cdm_local$death <- dplyr::tibble(
    person_id = 1:3,
    death_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11")),
    death_type_concept_id = NA
  )
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))

 cdm$cohort <- cdm$cohort %>%
    omopgenerics::newCohortTable()

 # test cohort id working

  CDMConnector::cdm_disconnect(cdm)
})

test_that("expected errors" {


})
