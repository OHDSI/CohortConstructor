test_that("exit at first date", {
  testthat::skip_on_cran()
  cdm <- mockCohortConstructor(
    tables = list(
      "cohort" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = c(1, 2, 3, 4, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-08-01", "2001-01-01", "2015-01-15", NA, "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "1990-13-09", "2002-12-09"))
      )
    ),
    con = connection(),
    writeSchema = writeSchema()
  )
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
  # activate test when arrange works for duckdb
  # expect_true(all(
  #   cdm$cohort1 %>% dplyr::pull("exit_reason") %>% sort() ==
  #     c('cohort_end_date', 'other_date_1', 'other_date_1',
  #       'other_date_2; cohort_end_date; other_date_1', 'other_date_2; other_date_1')
  # ))
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "exit_reason")))

  # works with == name and cohort ID
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
    cdm$cohort %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1990-12-09", "2001-01-01", "2001-08-01", "2002-12-09", "2015-01-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("exit at last date", {
  testthat::skip_on_cran()
  cdm <- mockCohortConstructor(
    tables = list(
      "cohort" = dplyr::tibble(
        cohort_definition_id = c(1, 1, 2, 2, 2),
        subject_id = c(1, 2, 3, 4, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-09-02", "2001-01-01", "2015-01-15", "2000-11-09", "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "1990-13-09", "2002-12-10"))
      )
    ),
    con = connection(),
    writeSchema = writeSchema()
  )

  # test cohort id working
  cdm$cohort1 <- cdm$cohort |>
    exitAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE,
      cohortId = 1,
      name = "cohort1"
    )
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1990-12-09", "2001-01-12", "2001-09-02", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  # test not cohort end as columns working
  cdm$cohort <- cdm$cohort |>
    exitAtLastDate(
      dateColumns = c("other_date_1", "other_date_2"),
      returnReason = TRUE
    )
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("2000-11-09", "2001-01-01", "2001-09-02", "2002-12-10", "2015-04-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "exit_reason")))
  expect_true(all(cdm$cohort %>% dplyr::pull("exit_reason") %in% c("other_date_1", "other_date_2")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("expected errors", {
  testthat::skip_on_cran()
  # NA
  cdm <- mockCohortConstructor(
    tables = list(
      "cohort" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = c(1, 2, 3, 4, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-09-02", NA, "2015-01-15", "2000-11-09", "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "2002-12-10", "2002-12-10"))
      )
    ),
    con = connection(),
    writeSchema = writeSchema()
  )
  expect_error(cdm$cohort <- cdm$cohort |>
                 exitAtLastDate(
                   dateColumns = c("other_date_1", "other_date_2"),
                   returnReason = TRUE
                 ))
  # overlap
  expect_error(cdm$cohort <- cdm$cohort |>
                 dplyr::filter(!is.na(.data$other_date_2)) |>
                 exitAtLastDate(
                   dateColumns = c("other_date_1", "other_date_2"),
                   returnReason = TRUE
                 ))

  PatientProfiles::mockDisconnect(cdm)

  # outside observation
  cdm <- mockCohortConstructor(tables = list(
    "cohort" = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 2, 3, 4),
      cohort_start_date = as.Date(c("2000-06-03", "2015-01-15", "1989-12-09", "2000-12-09")),
      cohort_end_date = as.Date(c("2001-09-01", "2015-02-15", "1990-12-09", "2002-12-09")),
      other_date_1 = as.Date(c("2001-09-02", "2015-01-15", "2000-11-09", "2002-12-09")),
      other_date_2 = as.Date(c("2001-08-01", "2016-04-15", "2002-12-10", "2002-12-10"))
    )
  ),
  con = connection(),
  writeSchema = writeSchema())
  expect_error(cdm$cohort <- cdm$cohort |>
                 exitAtLastDate(
                   dateColumns = c("other_date_1", "other_date_2"),
                   returnReason = TRUE
                 ))
  PatientProfiles::mockDisconnect(cdm)
  # start > end
  cdm <- mockCohortConstructor(tables = list(
    "cohort" = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 2, 3),
      cohort_start_date = as.Date(c("2000-06-03", "2015-01-15", "1989-12-09")),
      cohort_end_date = as.Date(c("2001-09-01", "2015-02-15", "1990-12-09")),
      other_date_1 = as.Date(c("2000-06-02", "2015-01-15", "2000-11-09")),
      other_date_2 = as.Date(c("2001-08-01", "2016-04-15", "2002-12-10"))
    )
  ),
  con = connection(),
  writeSchema = writeSchema())
  expect_error(cdm$cohort <- cdm$cohort |>
                 exitAtFirstDate(
                   dateColumns = c("other_date_1", "other_date_2"),
                   returnReason = TRUE
                 ))
  PatientProfiles::mockDisconnect(cdm)
})
