test_that("entry at first date", {
  testthat::skip_on_cran()
  cdm <- mockCohortConstructor(
    tables = list(
      "cohort" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = c(1, 2, 3, 4, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-08-01", "2001-01-01", "2015-01-15", NA, "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "1990-11-09", "2002-12-09"))
      )
    ),
    con = connection(),
    writeSchema = writeSchema()
  )
  # works
  cdm$cohort1 <- cdm$cohort |>
    entryAtFirstDate(
      dateColumns = c("cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      name = "cohort1"
    )
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1990-12-09", "2001-01-12", "2001-09-01", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(grepl("cohort_start_date", cdm$cohort1 %>% dplyr::pull("entry_reason"))))
  expect_true(sum(grepl("other_date_1", cdm$cohort1 %>% dplyr::pull("entry_reason"))) == 1)
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "entry_reason")))

  # works with == name and cohort ID
  cdm$cohort <- cdm$cohort |>
    entryAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE
    )
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1990-11-09", "2001-01-01", "2001-08-01", "2002-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1990-12-09", "2001-01-12", "2001-09-01", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("entry at last date", {
  testthat::skip_on_cran()
  cdm <- mockCohortConstructor(
    tables = list(
      "cohort" = dplyr::tibble(
        cohort_definition_id = c(1, 1, 2, 2, 2),
        subject_id = c(1, 2, 3, 4, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-10-01", "2001-04-15", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-09-02", "2001-01-01", "2015-01-15", "1989-11-09", "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-02-15", "1990-11-09", "2002-12-09"))
      )
    ),
    con = connection(),
    writeSchema = writeSchema()
  )

  # test cohort id working
  cdm$cohort1 <- cdm$cohort |>
    entryAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      cohortId = 1,
      name = "cohort1"
    )
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1989-12-09", "2000-12-09", "2001-04-15", "2001-10-01", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1990-12-09", "2001-04-15", "2001-10-01", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "entry_reason")))
  expect_true(all(
    cdm$cohort1 %>% dplyr::pull("entry_reason") %>% sort() ==
      c("cohort_end_date", "cohort_end_date", "cohort_start_date", "cohort_start_date", "cohort_start_date")
  ))

  # test not cohort end as columns working
  cdm$cohort <- cdm$cohort |>
    entryAtLastDate(
      dateColumns = c("other_date_1", "other_date_2"),
      returnReason = FALSE
    )
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("1990-11-09", "2001-01-01", "2001-09-02", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(
    cdm$cohort %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("1990-12-09", "2001-04-15", "2001-10-01", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  PatientProfiles::mockDisconnect(cdm)
})
