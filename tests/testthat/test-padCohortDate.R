test_that("simple example", {
  skip_on_cran()

  cdm <-  omock::mockCdmFromTables(tables = list(
    cohort =
      data.frame(
        cohort_definition_id = 1L,
        subject_id = c(1L, 2L),
        cohort_start_date = as.Date(c("2020-01-03","2020-01-03")),
        cohort_end_date = as.Date(c("2020-01-04", "2020-01-04"))
      )
  ))
  cdm <- cdm |> copyCdm()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2020-01-01", "2020-01-01")),
      observation_period_end_date = as.Date(c("2020-01-10", "2020-01-30")),
      period_type_concept_id = 1L
    )
  )

  cdm$cohort_1 <- padCohortEnd(cdm$cohort,
                                 days = 10,
                                 name = "cohort_1")

  expect_true(nrow(cdm$cohort_1 |>
    dplyr::collect()) == 2)
  # same cohort start
  expect_true(all(cdm$cohort_1 |>
                     dplyr::pull("cohort_start_date") ==
                   as.Date("2020-01-03")))
  # new cohort end
  expect_true(all(cdm$cohort_1 |>
                    dplyr::filter(subject_id == 1) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2020-01-10"))) # end of obs
  expect_true(all(cdm$cohort_1 |>
                    dplyr::filter(subject_id == 2) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2020-01-14"))) # cohort end plus 10 days

  # before cohort start - should be dropped if before cohort start
  cdm$cohort_2 <- padCohortEnd(cdm$cohort,
                               days = -2,
                               name = "cohort_2")
  expect_true(nrow(cdm$cohort_2 |>
                     dplyr::collect()) == 0)

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("overlapping entries", {
  skip_on_cran()

  cdm <-  omock::mockCdmFromTables(tables = list(
    cohort =
      data.frame(
        cohort_definition_id = c(1L, 1L, 2L, 2L),
        subject_id = c(1L, 1L, 1L, 2L),
        cohort_start_date = as.Date(c("2020-01-03",
                                      "2020-01-08",
                                      "2020-01-08",
                                      "2020-01-03")),
        cohort_end_date = as.Date(c("2020-01-04",
                                    "2020-01-10",
                                    "2020-01-10",
                                    "2020-01-04"))
      )
  ))
  cdm <- cdm |> copyCdm()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2020-01-01", "2020-01-01")),
      observation_period_end_date = as.Date(c("2020-01-10", "2020-01-30")),
      period_type_concept_id = 1L
    )
  )

  # by adding 10 days, person one will have overlapping entries
  # which should be collapsed
  # there entry in the other cohort should be unchanged
  cdm$cohort_1 <- padCohortEnd(cdm$cohort,
                               days = 10,
                               name = "cohort_1")
  expect_true(nrow(cdm$cohort_1 |>
                     dplyr::collect()) == 3)

  # new cohort end
  expect_true(all(cdm$cohort_1 |>
                    dplyr::filter(subject_id == 1,
                                  cohort_definition_id == 1) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2020-01-10"))) # end of obs
  expect_true(all(cdm$cohort_1 |>
                    dplyr::filter(subject_id == 2) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2020-01-14"))) # cohort end plus 10 days

  # leave one cohort unchanged
  cdm$cohort_2 <- padCohortEnd(cdm$cohort,
                               days = 10,
                               name = "cohort_2",
                               cohortId = 2)
  expect_true(nrow(cdm$cohort_2 |>
                     dplyr::collect()) == 4)

  expect_identical(cdm$cohort_2 |>
    dplyr::filter(subject_id == 2) |>
    dplyr::filter(cohort_definition_id == 2) |>
    dplyr::pull("cohort_end_date"), as.Date("2020-01-14"))

  expect_identical(sort(cdm$cohort |>
    dplyr::filter(cohort_definition_id == 1) |>
    dplyr::pull("cohort_end_date")), sort(cdm$cohort_2 |>
        dplyr::filter(cohort_definition_id == 1)  |>
    dplyr::pull("cohort_end_date")))

  # extra columns
 expect_no_error(
   cdm$cohort_3 <- cdm$cohort |>
    dplyr::mutate(extra_col = 1) |>
    padCohortEnd(days = 10, name = "cohort_3", cohortId = 2)
 )

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
  cdm$my_cohort <- padCohortEnd(cdm$my_cohort, days = 1)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("adding days to cohort start", {
  skip_on_cran()

  cdm <- omock::mockCdmFromTables(tables = list(
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
    name = "cohort_3",
    padObservation = FALSE
  )
  expect_true(nrow(dplyr::collect(cdm$cohort_3)) == 0)
  cdm$cohort_3 <- padCohortStart(
    cdm$cohort,
    days = -90,
    name = "cohort_3",
    padObservation = TRUE
  )
  expect_true(nrow(dplyr::collect(cdm$cohort_3)) == 1)
  expect_identical(
    cdm$cohort_3 |> dplyr::pull("cohort_start_date"), as.Date("2020-01-01")
  )

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
      observation_period_start_date = as.Date("2020-01-01"),
      observation_period_end_date = as.Date("2020-03-01"),
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
  expect_warning(padCohortStart(
    cdm$my_cohort,
    days = 2,
    cohortId = "a",
    name = "my_cohort_1"
  ))
  expect_warning(padCohortStart(
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
  cdm$my_cohort <- padCohortStart(cdm$my_cohort, days = 1)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("test padCohortDate", {
  skip_on_cran()

  cdm <- omock::mockCdmFromTables(tables = list(
    cohort = dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = c(1L, 2L),
      cohort_start_date = as.Date("2020-01-03"),
      cohort_end_date = as.Date("2020-01-20"),
      days = c(5, 8)
    )
  ))

  cdm <- copyCdm(cdm)

  expect_no_error(
    cdm$my_cohort <- cdm$cohort |>
      padCohortDate(
        days = "days",
        cohortDate = "cohort_end_date",
        indexDate = "cohort_start_date",
        collapse = TRUE,
        name = "my_cohort"
      )
  )

  expect_identical(
    cdm$my_cohort |>
      dplyr::filter(subject_id == 1) |>
      dplyr::pull("cohort_end_date"),
    as.Date("2020-01-08")
  )
  expect_identical(
    cdm$my_cohort |>
      dplyr::filter(subject_id == 2) |>
      dplyr::pull("cohort_end_date"),
    as.Date("2020-01-11")
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})
