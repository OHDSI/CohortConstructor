test_that("entry at first date", {
  testthat::skip_on_cran()
  cdm <- mockCohortConstructor(
    tables = list(
      "cohort" = dplyr::tibble(
        cohort_definition_id = 1L,
        subject_id = c(1, 2, 3, 4, 4)  |> as.integer(),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-09-01", "2001-01-12", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-08-01", "2001-01-01", "2015-01-15", NA, "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-04-15", "1990-11-09", "2002-12-09"))
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
    entryAtFirstDate(
      dateColumns = c("cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2"),
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
      c("1990-12-09", "2001-01-12", "2001-09-01", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(grepl("cohort_start_date", cdm$cohort1 |> dplyr::pull("entry_reason"))))
  expect_true(sum(grepl("other_date_1", cdm$cohort1 |> dplyr::pull("entry_reason"))) == 1)
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "entry_reason")))

  # test keep dates
  cdm$cohort2 <- cdm$cohort |>
    entryAtFirstDate(
      dateColumns = c("cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      name = "cohort2",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1), cdm$cohort2 |> dplyr::select(-c("other_date_1", "other_date_2")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2", "entry_reason") %in% colnames(cdm$cohort2)))
  expect_equal(collectCohort(cdm$cohort2, 1), collectCohort(cdm$cohort1, 1))

  cdm$cohort3 <- cdm$cohort |>
    entryAtFirstDate(
      dateColumns = c("cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE,
      name = "cohort3",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1 |> dplyr::select(!"entry_reason")), cdm$cohort3 |> dplyr::select(-c("other_date_1", "other_date_2")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2") %in% colnames(cdm$cohort3)))
  expect_equal(collectCohort(cdm$cohort3, 1), collectCohort(cdm$cohort1, 1))

  # same name
  cdm$cohort_new <- cdm$cohort |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2") |>
    dplyr::compute(name = "cohort_new", temporary = FALSE) |>
    omopgenerics::newCohortTable()

  cdm$cohort_new <- cdm$cohort_new |>
    entryAtFirstDate(
      dateColumns = c("cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      keepDateColumns = TRUE
    )
  expect_true(all(c("other_date_1", "other_date_2", "entry_reason") %in% colnames(cdm$cohort_new)))
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort_new, 1))

  # works with == name and cohort ID
  cdm$cohort <- cdm$cohort |>
    entryAtFirstDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      keepDateColumns = FALSE,
      returnReason = FALSE
    )
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1990-11-09", "2001-01-01", "2001-08-01", "2002-12-09", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1990-12-09", "2001-01-12", "2001-09-01", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("entry at last date", {
  testthat::skip_on_cran()
  cdm <- mockCohortConstructor(
    tables = list(
      "cohort" = dplyr::tibble(
        cohort_definition_id = c(1, 1, 2, 2, 2) |> as.integer(),
        subject_id = c(1, 2, 3, 4, 4) |> as.integer(),
        cohort_start_date = as.Date(c("2000-06-03", "2000-01-01", "2015-01-15", "1989-12-09", "2000-12-09")),
        cohort_end_date = as.Date(c("2001-10-01", "2001-04-15", "2015-02-15", "1990-12-09", "2002-12-09")),
        other_date_1 = as.Date(c("2001-09-02", "2001-01-01", "2015-01-15", "1989-11-09", "2002-12-09")),
        other_date_2 = as.Date(c("2001-08-01", NA, "2015-02-15", "1990-11-09", "2002-12-09"))
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

  cdm$cohort1 <- cdm$cohort |>
    entryAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      cohortId = 1,
      keepDateColumns = FALSE,
      name = "cohort1"
    )
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1989-12-09", "2000-12-09", "2001-04-15", "2001-10-01", "2015-01-15")
  ))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1990-12-09", "2001-04-15", "2001-10-01", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(colnames(cdm$cohort1) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "entry_reason")))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("entry_reason") |> sort() ==
      c("cohort_end_date", "cohort_end_date", "cohort_start_date", "cohort_start_date", "cohort_start_date")
  ))

  # test keep dates
  cdm$cohort2 <- cdm$cohort |>
    entryAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      name = "cohort2",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1), cdm$cohort2 |> dplyr::select(-c("other_date_1", "other_date_2")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2", "entry_reason") %in% colnames(cdm$cohort2)))
  expect_equal(collectCohort(cdm$cohort2, 1), collectCohort(cdm$cohort1, 1))

  cdm$cohort3 <- cdm$cohort |>
    entryAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = FALSE,
      name = "cohort3",
      keepDateColumns = TRUE
    )
  expect_equal(colnames(cdm$cohort1 |> dplyr::select(!"entry_reason")), cdm$cohort3 |> dplyr::select(-c("other_date_1", "other_date_2")) |> colnames())
  expect_true(all(c("other_date_1", "other_date_2") %in% colnames(cdm$cohort3)))
  expect_equal(collectCohort(cdm$cohort3, 1), collectCohort(cdm$cohort1, 1))

  # same name
  cdm$cohort_new <- cdm$cohort |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "other_date_1", "other_date_2") |>
    dplyr::compute(name = "cohort_new", temporary = FALSE) |>
    omopgenerics::newCohortTable()

  cdm$cohort_new <- cdm$cohort_new |>
    entryAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      keepDateColumns = TRUE
    )
  expect_true(all(c("other_date_1", "other_date_2", "entry_reason") %in% colnames(cdm$cohort_new)))
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort_new, 1))

  # test cohort id working
  # test character cohort id working
  cdm$cohort1 <- cdm$cohort |>
    entryAtLastDate(
      dateColumns = c("cohort_end_date", "other_date_1", "other_date_2"),
      returnReason = TRUE,
      cohortId = c("cohort_2"),
      keepDateColumns = FALSE,
      name = "cohort1"
    )
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1990-12-09", "2000-01-01", "2000-06-03", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(
    cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1990-12-09", "2001-04-15", "2001-10-01", "2002-12-09", "2015-02-15")
  ))

  # test not cohort end as columns working
  cdm$cohort <- cdm$cohort |>
    entryAtLastDate(
      dateColumns = c("other_date_1", "other_date_2"),
      keepDateColumns = FALSE,
      returnReason = FALSE
    )
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1990-11-09", "2001-01-01", "2001-09-02", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1990-12-09", "2001-04-15", "2001-10-01", "2002-12-09", "2015-02-15")
  ))
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

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

  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_cohort",
                                   table = data.frame(cohort_definition_id = 1L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-02"),
                                                      cohort_end_date = as.Date("2009-01-03"),
                                                      other_date = as.Date("2009-01-01")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- entryAtFirstDate(cdm$my_cohort, dateColumns = c("cohort_end_date", "other_date"), returnReason = TRUE)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  cdm$my_cohort <- entryAtLastDate(cdm$my_cohort, dateColumns = c("cohort_start_date", "cohort_end_date"), returnReason = FALSE)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
