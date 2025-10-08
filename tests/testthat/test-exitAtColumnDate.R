test_that("exit at first date", {
  skip_on_cran()
  cdm <- omock::mockCdmFromTables(
    tables = list(
      "cohort_1" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = c(1, 2, 3, 4, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-08-01", "2001-01-01", "2015-01-15", NA, "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "1990-13-09", "2002-12-09"))
      ),
      "cohort_2" = dplyr::tibble(
        cohort_definition_id = c(1, 1, 2, 2, 2),
        subject_id = c(1, 2, 3, 4, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-09-02", "2001-01-01", "2015-01-15", "2000-11-09", "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "1990-13-09", "2002-12-10"))
      ),
      "cohort_3" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = c(1, 2, 3, 4, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-09-02", NA, "2015-01-15", "2000-11-09", "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "2002-12-10", "2002-12-10"))
      ),
      "cohort_4" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = c(1, 2, 3, 4),
        cohort_start_date = as.Date(c("2000-06-03", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-09-02", "2015-01-15", "2000-11-09", "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", "2016-04-15", "2002-12-10", "2002-12-10"))
      ),
      "cohort_5" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = c(1, 2, 3),
        cohort_start_date = as.Date(c("2000-06-03", "2015-01-15", "1989-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2015-02-15", "1990-12-09")),
        other_date_1 = as.Date(c("2000-06-02", "2015-01-15", "2000-11-09")),
        other_date_2 = as.Date(c("2001-08-01", "2016-04-15", "2002-12-10"))
      )
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = dplyr::tibble(
      "observation_period_id" = c(1L, 2L, 3L, 4L, 5L),
      "person_id" = observation_period_id,
      "observation_period_start_date" = as.Date(c("1978-01-01")),
      "observation_period_end_date" = as.Date(c("2025-01-01")),
      "period_type_concept_id" = NA_integer_
    ))
  cdm <- cdm |> copyCdm()

  # Exit at first date ----
  cdm$cohort1 <- cdm$cohort_1 |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      keepDateColumns = FALSE,
      name = "cohort1"
    )
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1990-12-09", "2001-01-01", "2001-08-01", "2002-12-09", "2015-01-15")
  ))
  # activate test when arrange works for duckdb
  # expect_true(all(
  #   cdm$cohort1 |> dplyr::pull("exit_reason") |> sort() ==
  #     c('cohort_end_date', 'other_date_1', 'other_date_1',
  #       'other_date_2; cohort_end_date; other_date_1', 'other_date_2; other_date_1')
  # ))
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "exit_reason")))

  # test keep dates
  cdm$cohort2 <- cdm$cohort_1 |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      name = "cohort2",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1), cdm$cohort2 |> dplyr::select(-c("other_date_1", "other_date_2")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2", "exit_reason") %in% colnames(cdm$cohort2)))
  expect_equal(collectCohort(cdm$cohort2, 1), collectCohort(cdm$cohort1, 1))

  cdm$cohort3 <- cdm$cohort_1 |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE,
      name = "cohort3",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1 |> dplyr::select(!"exit_reason")), cdm$cohort3 |> dplyr::select(-c("other_date_1", "other_date_2")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2") %in% colnames(cdm$cohort3)))
  expect_equal(collectCohort(cdm$cohort3, 1), collectCohort(cdm$cohort1, 1))

  # same name
  cdm$cohort_new <- cdm$cohort_1 |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2") |>
    dplyr::compute(name = "cohort_new", temporary = FALSE) |>
    omopgenerics::newCohortTable()

  cdm$cohort_new <- cdm$cohort_new |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      keepDateColumns = TRUE
    )
  expect_true(all(c("other_date_1", "other_date_2", "exit_reason") %in% colnames(cdm$cohort_new)))
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort_new, 1))

  # works with == name and cohort ID
  cdm$cohort_1 <- cdm$cohort_1 |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      keepDateColumns = FALSE,
      returnReason = FALSE
    )
  expect_true(all(
    cdm$cohort_1 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort_1 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1990-12-09", "2001-01-01", "2001-08-01", "2002-12-09", "2015-01-15")
  ))
  expect_true(all(colnames(cdm$cohort_1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  # Indexes ----
  if (dbToTest == "postgres CDMConnector" & testIndexes) {
    expect_true(
      DBI::dbGetQuery(CDMConnector::cdmCon(cdm), paste0("SELECT * FROM pg_indexes WHERE tablename = 'coco_test_cohort_1';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX coco_test_cohort_1_subject_id_cohort_start_date_idx ON public.coco_test_cohort_1 USING btree (subject_id, cohort_start_date)"
    )
  }

  # Exit at last date ----
  # test cohort id working
  cdm$cohort1 <- cdm$cohort_2 |>
    exitAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE,
      cohortId = 1,
      keepDateColumns = FALSE,
      name = "cohort1"
    )
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1990-12-09", "2001-01-12", "2001-09-02", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  # test cohort id working
  cdm$cohort11 <- cdm$cohort_2 |>
    exitAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE,
      cohortId = c("cohort_1"),
      keepDateColumns = FALSE,
      name = "cohort11"
    )
  expect_identical(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort11, 1))

  # test keep dates
  cdm$cohort2 <- cdm$cohort_2 |>
    exitAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      name = "cohort2",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1), cdm$cohort2 |> dplyr::select(-c("other_date_1", "other_date_2", "exit_reason")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2", "exit_reason") %in% colnames(cdm$cohort2)))
  expect_equal(collectCohort(cdm$cohort2, 1), collectCohort(cdm$cohort1, 1))

  cdm$cohort3 <- cdm$cohort_2 |>
    exitAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE,
      name = "cohort3",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1), cdm$cohort3 |> dplyr::select(-c("other_date_1", "other_date_2")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2") %in% colnames(cdm$cohort3)))
  expect_equal(collectCohort(cdm$cohort3, 1), collectCohort(cdm$cohort1, 1))

  # same name
  cdm$cohort_new <- cdm$cohort_2 |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2") |>
    dplyr::compute(name = "cohort_new", temporary = FALSE) |>
    omopgenerics::newCohortTable()

  cdm$cohort_new <- cdm$cohort_new |>
    exitAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      keepDateColumns = TRUE
    )
  expect_true(all(c("other_date_1", "other_date_2", "exit_reason") %in% colnames(cdm$cohort_new)))
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort_new, 1))

  # test not cohort end as columns working
  cdm$cohort_2 <- cdm$cohort_2 |>
    exitAtLastDate(
      dateColumns = c("other_date_1", "other_date_2"),
      keepDateColumns = FALSE,
      returnReason = TRUE
    )
  expect_true(all(
    cdm$cohort_2 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort_2 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2000-11-09", "2001-01-01", "2001-09-02", "2002-12-10", "2015-04-15")
  ))
  expect_true(all(colnames(cdm$cohort_2) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "exit_reason")))
  expect_true(all(cdm$cohort_2 |> dplyr::pull("exit_reason") %in% c("other_date_1", "other_date_2")))

  # Indexes ----
  if (dbToTest == "postgres CDMConnector" & testIndexes) {
    expect_true(
      DBI::dbGetQuery(CDMConnector::cdmCon(cdm), paste0("SELECT * FROM pg_indexes WHERE tablename = 'coco_test_cohort_2';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX coco_test_cohort_2_subject_id_cohort_start_date_idx ON public.coco_test_cohort_2 USING btree (subject_id, cohort_start_date)"
    )
  }

  # Expected errors ----
  expect_error(cdm$cohort_3 <- cdm$cohort_3 |>
                 exitAtLastDate(
                   dateColumns = c("other_date_1", "other_date_2"),
                   returnReason = TRUE
                 ))
  # overlap
  expect_error(cdm$cohort_3 <- cdm$cohort_3 |>
                 dplyr::filter(!is.na(.data$other_date_2)) |>
                 exitAtLastDate(
                   dateColumns = c("other_date_1", "other_date_2"),
                   returnReason = TRUE
                 ))

  expect_error(cdm$cohort_4 <- cdm$cohort_3 |>
                 exitAtLastDate(
                   dateColumns = c("other_date_1", "other_date_2"),
                   returnReason = TRUE
                 ))

  expect_error(cdm$cohort_5 <- cdm$cohort_5 |>
                 exitAtFirstDate(
                   dateColumns = c("other_date_1", "other_date_2"),
                   returnReason = TRUE
                 ))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})
