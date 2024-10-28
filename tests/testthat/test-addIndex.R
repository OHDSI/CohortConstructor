
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

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdm_disconnect(cdm = cdm)

})
