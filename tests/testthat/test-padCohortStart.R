test_that("adding days to cohort start", {
  cdm <-  omock::mockCdmFromTables(tables = list(
    cohort =
      data.frame(
        cohort_definition_id = 1L,
        subject_id = 1L,
        cohort_start_date = as.Date("2020-01-03"),
        cohort_end_date = as.Date("2020-01-10")
      )
  ))
  cdm <- cdm |> copyCdm()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "observation_period",
    table = data.frame(
      observation_period_id = 1L,
      person_id = 1L,
      observation_period_start_date = as.Date("2020-01-01"),
      observation_period_end_date = as.Date("2020-03-01"),
      period_type_concept_id = 1L
    )
  )

  cdm$cohort_1 <- padCohortStart(cdm$cohort,
                                 days = 2,
                                 name = "cohort_1")
  expect_identical(cdm$cohort_1 |> dplyr::pull("cohort_start_date"), as.Date("2020-01-05"))

  # minus days
  cdm$cohort_2 <- padCohortStart(cdm$cohort,
                                 days = -2,
                                 name = "cohort_2")
  expect_identical(cdm$cohort_2 |> dplyr::pull("cohort_start_date"),
                   as.Date("2020-01-01"))

  # minus days goes outside of current observation period
  cdm$cohort_3 <- padCohortStart(
    cdm$cohort,
    days = -90,
    name = "cohort_3"
  )
  expect_identical(nrow(cdm$cohort_3 |>
                          dplyr::collect()), nrow(dplyr::tibble()))
  expect_true(cohortCount(cdm$cohort_3) |>
                dplyr::pull("number_subjects") == 0)

  # drop if cohort start would be after cohort end
  cdm$cohort_4 <- padCohortStart(cdm$cohort,
                                 days = 90,
                                 name = "cohort_4")
  expect_identical(nrow(cdm$cohort_4 |>
                          dplyr::collect()), nrow(dplyr::tibble()))
  expect_true(cohortCount(cdm$cohort_4) |>
                dplyr::pull("number_subjects") == 0)


  # update just one cohort, leaving the other unchanged
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date =
        as.Date("2020-01-01"),
      observation_period_end_date =
        as.Date("2020-03-01"),
      period_type_concept_id = 1L
    )
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "my_cohort",
    data.frame(
      cohort_definition_id = c(1L, 2L),
      subject_id = c(1L, 2L),
      cohort_start_date = as.Date("2020-01-03"),
      cohort_end_date = as.Date("2020-01-10")
    )
  )
  cdm$my_cohort <- cdm$my_cohort |>
    omopgenerics::newCohortTable()

  cdm$my_cohort_1 <- padCohortStart(
    cdm$my_cohort,
    days = 2,
    cohortId = 1,
    name = "my_cohort_1"
  )
  expect_identical(
    cdm$my_cohort_1 |>
      dplyr::filter(cohort_definition_id == 1) |>
      dplyr::pull("cohort_start_date"),
    as.Date("2020-01-05")
  )
  expect_identical(
    cdm$my_cohort_1 |>
      dplyr::filter(cohort_definition_id == 2) |>
      dplyr::pull("cohort_start_date"),
    as.Date("2020-01-03")
  )

  cdm$my_cohort_2 <- padCohortStart(
    cdm$my_cohort,
    days = 2,
    cohortId = "cohort_1",
    name = "my_cohort_2"
  )
  expect_identical(collectCohort(cdm$my_cohort_1, 1), collectCohort(cdm$my_cohort_2, 1))

  # input validation
  expect_error(padCohortStart(
    "my_cohort",
    days = 2,
    cohortId = 1,
    name = "my_cohort_1"
  ))
  expect_error(padCohortStart(
    cdm$my_cohort,
    days = "2",
    cohortId = 1,
    name = "my_cohort_1"
  ))
  expect_error(padCohortStart(
    cdm$my_cohort,
    days = 2,
    cohortId = "a",
    name = "my_cohort_1"
  ))
  expect_error(padCohortStart(
    cdm$my_cohort,
    days = 2,
    cohortId = 99,
    name = "my_cohort_1"
  ))
  expect_warning(padCohortStart(
    cdm$my_cohort,
    days = 2,
    cohortId = 1,
    name = "my_cohort 1"
  ))
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
                                                      cohort_start_date = as.Date("2009-01-02"),
                                                      cohort_end_date = as.Date("2009-01-03"),
                                                      other_date = as.Date("2009-01-01")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- padCohortStart(cdm$my_cohort, days = 1)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdm_disconnect(cdm = cdm)
})
