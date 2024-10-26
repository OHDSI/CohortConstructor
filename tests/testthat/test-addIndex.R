
test_that("local tibble and duckdb test - will do nothing for these", {

  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1,
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  expect_no_error(cdm$cohort <- cdm$cohort |>
    addCohortTableIndex())

  cdm <- cdm |> copyCdm()
  expect_no_error(cdm$cohort |>
    addCohortTableIndex())

  # expected error
  expect_error(cdm$person |>
    addCohortTableIndex())

})

test_that("postgres test - adds indexes", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")

  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                       host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                       password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    write_schema = c(schema =  Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
                     prefix = "cc_"),
    achilles_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  )

  cdm <- omopgenerics::insertTable(cdm = cdm,
              name = "my_cohort",
              table = data.frame(cohort_definition_id = 1L,
                                 subject_id = 1L,
                                 cohort_start_date = as.Date("2009-01-01"),
                                 cohort_end_date = as.Date("2009-01-02")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)

  indexes_start <- DBI::dbGetQuery(db,
                        paste0("SELECT indexname FROM pg_indexes WHERE tablename = 'cc_my_cohort';"))
  expect_true(nrow(indexes_start) == 0)

  cdm$my_cohort <- cdm$my_cohort |> addCohortTableIndex()
  indexes_end <- DBI::dbGetQuery(db,
                            paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';"))
  expect_true(nrow(indexes_end) == 1)

  # no error if we add another index - it will just be skipped
  cdm$my_cohort <- cdm$my_cohort |> addCohortTableIndex()
  indexes_end_2 <- DBI::dbGetQuery(db,
                            paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';"))
  expect_true(nrow(indexes_end_2) == 1)

  # should still all work as before
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)

  omopgenerics::dropTable(cdm = cdm, name = "my_cohort")
  CDMConnector::cdm_disconnect(cdm = cdm)

})

test_that("sql server test - adds indexes", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_SQL_SERVER_SERVER") == "")

  db <- DBI::dbConnect(odbc::odbc(),
                       Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                       Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                       Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                       UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                       PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                       TrustServerCertificate="yes",
                       Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))

  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = strsplit(Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"), "\\.")[[1]],
    write_schema = c(schema =  strsplit(Sys.getenv("CDM5_SQL_SERVER_SCRATCH_SCHEMA"), "\\.")[[1]],
                     prefix = "cc_")
  )

  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_cohort",
                                   table = data.frame(cohort_definition_id = 1L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-01"),
                                                      cohort_end_date = as.Date("2009-01-02")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)

  indexes_start <- DBI::dbGetQuery(db,
                                 paste0("
  SELECT i.name AS index_name,
         o.name AS table_name,
         'tempdb' AS database_name
  FROM tempdb.sys.indexes i
  JOIN tempdb.sys.objects o ON i.object_id = o.object_id
  WHERE o.name = 'cc_my_cohort'
  AND i.name IS NOT NULL
"))
  expect_true(nrow(indexes_start) == 0)

  cdm$my_cohort <- cdm$my_cohort |> addCohortTableIndex()

  indexes_end <- DBI::dbGetQuery(db,
                                   paste0("
  SELECT i.name AS index_name,
         o.name AS table_name,
         'tempdb' AS database_name
  FROM tempdb.sys.indexes i
  JOIN tempdb.sys.objects o ON i.object_id = o.object_id
  WHERE o.name = 'cc_my_cohort'
  AND i.name IS NOT NULL
"))
  expect_true(nrow(indexes_end) == 1)

  # no error if we add another index - it will just be skipped
  cdm$my_cohort <- cdm$my_cohort |> addCohortTableIndex()
  indexes_end_2 <- DBI::dbGetQuery(db,
                                 paste0("
  SELECT i.name AS index_name,
         o.name AS table_name,
         'tempdb' AS database_name
  FROM tempdb.sys.indexes i
  JOIN tempdb.sys.objects o ON i.object_id = o.object_id
  WHERE o.name = 'cc_my_cohort'
  AND i.name IS NOT NULL
"))
  expect_true(nrow(indexes_end_2) == 1)

  # should still all work as before
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  omopgenerics::dropTable(cdm = cdm, name = "my_cohort")


  # no cdm prefix
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = strsplit(Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"), "\\.")[[1]],
    write_schema = c(schema =  strsplit(Sys.getenv("CDM5_SQL_SERVER_SCRATCH_SCHEMA"), "\\.")[[1]])
  )
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_new_cohort",
                                   table = data.frame(cohort_definition_id = 1L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-01"),
                                                      cohort_end_date = as.Date("2009-01-02")))
  cdm$my_new_cohort <- omopgenerics::newCohortTable(cdm$my_new_cohort)
  cdm$my_new_cohort <- cdm$my_new_cohort |> addCohortTableIndex()

  omopgenerics::dropTable(cdm = cdm, name = "my_new_cohort")

  CDMConnector::cdm_disconnect(cdm = cdm)

})

test_that("sql server no prefix test - adds indexes", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_SQL_SERVER_SERVER") == "")

  db <- DBI::dbConnect(odbc::odbc(),
                       Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                       Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                       Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                       UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                       PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                       TrustServerCertificate="yes",
                       Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))


  # no cdm prefix
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = strsplit(Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"), "\\.")[[1]],
    write_schema = c(schema =  strsplit(Sys.getenv("CDM5_SQL_SERVER_SCRATCH_SCHEMA"), "\\.")[[1]])
  )
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_new_cohort",
                                   table = data.frame(cohort_definition_id = 1L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-01"),
                                                      cohort_end_date = as.Date("2009-01-02")))
  cdm$my_new_cohort <- omopgenerics::newCohortTable(cdm$my_new_cohort)

  indexes_start <- DBI::dbGetQuery(db,
                                   paste0("
  SELECT i.name AS index_name,
         o.name AS table_name,
         'tempdb' AS database_name
  FROM tempdb.sys.indexes i
  JOIN tempdb.sys.objects o ON i.object_id = o.object_id
  WHERE o.name = 'my_new_cohort'
  AND i.name IS NOT NULL
"))
  expect_true(nrow(indexes_start) == 0)

  cdm$my_new_cohort <- cdm$my_new_cohort |> addCohortTableIndex()

  indexes_end <- DBI::dbGetQuery(db,
                                   paste0("
  SELECT i.name AS index_name,
         o.name AS table_name,
         'tempdb' AS database_name
  FROM tempdb.sys.indexes i
  JOIN tempdb.sys.objects o ON i.object_id = o.object_id
  WHERE o.name = 'my_new_cohort'
  AND i.name IS NOT NULL
"))
  expect_true(nrow(indexes_end) == 1)

  # skip creation now index exists
  cdm$my_new_cohort <- cdm$my_new_cohort |> addCohortTableIndex()

  indexes_end_2 <- DBI::dbGetQuery(db,
                                 paste0("
  SELECT i.name AS index_name,
         o.name AS table_name,
         'tempdb' AS database_name
  FROM tempdb.sys.indexes i
  JOIN tempdb.sys.objects o ON i.object_id = o.object_id
  WHERE o.name = 'my_new_cohort'
  AND i.name IS NOT NULL
"))
  expect_true(nrow(indexes_end_2) == 1)

  omopgenerics::dropTable(cdm = cdm, name = "my_new_cohort")

  CDMConnector::cdm_disconnect(cdm = cdm)

})
