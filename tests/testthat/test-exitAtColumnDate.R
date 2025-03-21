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
  # remove when omock > 0.3.1
  cdm$cohort <- cdm$cohort |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2") |>
    dplyr::compute(name = "cohort", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  # works
  cdm$cohort1 <- cdm$cohort |>
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
  cdm$cohort2 <- cdm$cohort |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      name = "cohort2",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1), cdm$cohort2 |> dplyr::select(-c("other_date_1", "other_date_2")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2", "exit_reason") %in% colnames(cdm$cohort2)))
  expect_equal(collectCohort(cdm$cohort2, 1), collectCohort(cdm$cohort1, 1))

  cdm$cohort3 <- cdm$cohort |>
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
  cdm$cohort_new <- cdm$cohort |>
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
  cdm$cohort <- cdm$cohort |>
    exitAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      keepDateColumns = FALSE,
      returnReason = FALSE
    )
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1990-12-09", "2001-01-01", "2001-08-01", "2002-12-09", "2015-01-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
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
  # remove when omock > 0.3.1
  cdm$cohort <- cdm$cohort |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2") |>
    dplyr::compute(name = "cohort", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  # test cohort id working
  cdm$cohort1 <- cdm$cohort |>
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
  cdm$cohort11 <- cdm$cohort |>
    exitAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE,
      cohortId = c("cohort_1"),
      keepDateColumns = FALSE,
      name = "cohort11"
    )
  expect_identical(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort11, 1))

  # test keep dates
  cdm$cohort2 <- cdm$cohort |>
    exitAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      name = "cohort2",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1), cdm$cohort2 |> dplyr::select(-c("other_date_1", "other_date_2", "exit_reason")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2", "exit_reason") %in% colnames(cdm$cohort2)))
  expect_equal(collectCohort(cdm$cohort2, 1), collectCohort(cdm$cohort1, 1))

  cdm$cohort3 <- cdm$cohort |>
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
  cdm$cohort_new <- cdm$cohort |>
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
  cdm$cohort <- cdm$cohort |>
    exitAtLastDate(
      dateColumns = c("other_date_1", "other_date_2"),
      keepDateColumns = FALSE,
      returnReason = TRUE
    )
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1989-12-09", "2000-01-01", "2000-06-03", "2000-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2000-11-09", "2001-01-01", "2001-09-02", "2002-12-10", "2015-04-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "exit_reason")))
  expect_true(all(cdm$cohort |> dplyr::pull("exit_reason") %in% c("other_date_1", "other_date_2")))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
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
  # remove when omock > 0.3.1
  cdm$cohort <- cdm$cohort |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2") |>
    dplyr::compute(name = "cohort", temporary = FALSE) |>
    omopgenerics::newCohortTable()
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

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
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
  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("test indexes - postgres", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")
  skip_if(!testIndexes)

  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                       host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                       password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    writeSchema = Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
    writePrefix = "cc_",
    achillesSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  )

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::contains("og_"))

  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_cohort",
                                   table = data.frame(cohort_definition_id = 1L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-02"),
                                                      cohort_end_date = as.Date("2009-01-03"),
                                                      other_date = as.Date("2009-01-01")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- exitAtLastDate(cdm$my_cohort, dateColumns = c("cohort_end_date", "other_date"), returnReason = TRUE)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  cdm$my_cohort <- exitAtFirstDate(cdm$my_cohort, dateColumns = c("cohort_start_date", "cohort_end_date"), returnReason = TRUE)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og_", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
