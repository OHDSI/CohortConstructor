test_that("test restrict to first entry works", {
  skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = "cohort1", numberCohorts = 1, recordPerson = 2,seed = 1) |>
    omock::mockCohort(name = "cohort2", numberCohorts = 2, recordPerson = 2,seed = 1)

  expect_true(all(cdm$cohort1 |> CohortConstructor::requireIsFirstEntry() |>
                    dplyr::pull(cohort_start_date) == c("2001-05-29", "1999-07-30", "2015-01-23")))

  expect_true(all(cdm$cohort1 |> CohortConstructor::requireIsFirstEntry() |>
                    dplyr::pull(subject_id) %in% 1:3))

  expect_true(all(cdm$cohort2 |> CohortConstructor::requireIsFirstEntry() |>
                    dplyr::pull(cohort_start_date) ==
                    c("2001-05-29", "1999-07-30", "2015-01-23", "2002-10-09", "1999-04-16", "2015-02-22")))

  expect_true(all(cdm$cohort2 |> CohortConstructor::requireIsFirstEntry() |>
                    dplyr::pull(subject_id) == c(1:3, 1:3)))

})

test_that("requireIsFirstEntry, cohortIds & name arguments", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(numberCohorts = 3, recordPerson = 2,seed = 1)

  expect_no_error(
    cdm$new_cohort <- CohortConstructor::requireIsFirstEntry(
      cohort = cdm$cohort,
      cohortId = 1,
      name = "new_cohort")
  )

  counts <- omopgenerics::cohortCount(cdm$cohort)
  counts_new <- omopgenerics::cohortCount(cdm$new_cohort)

  expect_identical(counts |> dplyr::filter(cohort_definition_id %in% 2:3), counts_new |> dplyr::filter(cohort_definition_id %in% 2:3))
  expect_false(counts |> dplyr::filter(cohort_definition_id == 1) |> dplyr::pull(number_records) ==
                 counts_new |> dplyr::filter(cohort_definition_id == 1) |> dplyr::pull(number_records))
  expect_identical(counts_new |> dplyr::filter(cohort_definition_id == 1), dplyr::tibble(cohort_definition_id = 1L, number_records = 3L, number_subjects = 3L))
  expect_true(all(cdm$new_cohort |>  dplyr::pull(cohort_start_date) ==
                    c("2001-05-29", "1999-07-30", "2015-01-23", "2002-10-09", "2003-09-12",
                      "1999-04-16", "2000-03-09", "2000-05-05", "2015-02-22", "2002-09-28",
                      "1999-08-25", "1999-12-14", "1999-12-24", "2000-04-02", "2000-08-11")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(cohort_end_date) ==
                    c("2002-10-23", "2001-10-02", "2015-02-16", "2003-09-11", "2009-03-19",
                      "2000-03-08", "2000-05-04", "2001-04-01", "2015-03-23", "2008-10-13",
                      "1999-12-13", "1999-12-23", "2000-04-01", "2000-08-10", "2000-09-13")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(subject_id) == c(1, 2, 3, 1, 1, 2, 2, 2, 3, 1, 2, 2, 2, 2,2)))
  expect_true(all(
    omopgenerics::attrition(cdm$new_cohort)$reason  ==
      c("Initial qualifying events", "Restricted to first entry",
        "Initial qualifying events", "Initial qualifying events")
  ))

})

test_that("errors", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(numberCohorts = 1, recordPerson = 2,seed = 1)

  expect_error(cdm$cohort |> requireIsFirstEntry(name = 1))
  expect_error(cdm$cohort1 <- cdm$cohort |> requireIsFirstEntry(name = "cohort2"))
  expect_warning(cdm$cohort |> requireIsFirstEntry(cohortId = Inf))
  expect_error(cdm$cohort |> dplyr::collect() |> requireIsFirstEntry())
  expect_warning(cdm$cohort |> requireIsFirstEntry(cohortId = c(1, 5)))
})

test_that("requireIsLastEntry", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(numberCohorts = 3, recordPerson = 2,seed = 1)

  cdm$new_cohort <- CohortConstructor::requireIsLastEntry(
    cohort = cdm$cohort,
    cohortId = 1,
    name = "new_cohort")

  counts <- omopgenerics::cohortCount(cdm$cohort)
  counts_new <- omopgenerics::cohortCount(cdm$new_cohort)

  expect_identical(counts |> dplyr::filter(cohort_definition_id %in% 2:3), counts_new |> dplyr::filter(cohort_definition_id %in% 2:3))
  expect_false(counts |> dplyr::filter(cohort_definition_id == 1) |> dplyr::pull(number_records) ==
                 counts_new |> dplyr::filter(cohort_definition_id == 1) |> dplyr::pull(number_records))
  expect_identical(counts_new |> dplyr::filter(cohort_definition_id == 1), dplyr::tibble(cohort_definition_id = 1L, number_records = 3L, number_subjects = 3L))
  expect_true(all(cdm$new_cohort |>  dplyr::pull(cohort_start_date) ==
                    c("2004-01-08", "1999-07-30", "2015-02-17", "2002-10-09", "2003-09-12",
                      "1999-04-16", "2000-03-09", "2000-05-05", "2015-02-22", "2002-09-28",
                      "1999-08-25", "1999-12-14", "1999-12-24", "2000-04-02", "2000-08-11")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(cohort_end_date) ==
                    c("2009-10-03", "2001-10-02", "2015-03-10", "2003-09-11", "2009-03-19",
                      "2000-03-08", "2000-05-04", "2001-04-01", "2015-03-23", "2008-10-13",
                      "1999-12-13", "1999-12-23", "2000-04-01", "2000-08-10", "2000-09-13")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(subject_id) ==
                    c(1, 2, 3, 1, 1, 2, 2, 2, 3, 1, 2, 2, 2, 2, 2)))
  expect_true(all(omopgenerics::attrition(cdm$new_cohort)$reason == c(
    c("Initial qualifying events", "Restricted to last entry", "Initial qualifying events",
      "Initial qualifying events"))
  ))


  # errors
  expect_error(cdm$cohort |> requireIsLastEntry(name = 1))
  expect_error(cdm$cohort1 <- cdm$cohort |> requireIsLastEntry(name = "cohort2"))
  expect_warning(cdm$cohort |> requireIsLastEntry(cohortId = Inf))
  expect_error(cdm$cohort |> dplyr::collect() |> requireIsLastEntry())
  expect_warning(cdm$cohort |> requireIsLastEntry(cohortId = c(1, 5)))
})

test_that("requireEntry", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 10,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = "cohort1",
                      numberCohorts = 1,
                      recordPerson = 4,seed = 1)

  # 1 to inf will leave the cohort table unchanged
  cdm$cohort1_a <- requireIsEntry(
    cohort = cdm$cohort1,
    entryRange = c(1, Inf),
    cohortId = 1,
    name = "cohort1_a")

  expect_equal(
    cdm$cohort1 |> dplyr::collect(),
    cdm$cohort1_a |> dplyr::collect(),
    ignore_attr = TRUE
  )

  cdm$cohort1_b <- requireIsEntry(
    cohort = cdm$cohort1,
    entryRange = c(1, 1),
    cohortId = 1,
    name = "cohort1_b")
  cdm$cohort1_c <- requireIsFirstEntry(
    cohort = cdm$cohort1,
    cohortId = 1,
    name = "cohort1_c")
  expect_equal(
    cdm$cohort1 |> dplyr::collect() |> dplyr::arrange(),
    cdm$cohort1_a |> dplyr::collect() |> dplyr::arrange(),
    ignore_attr = TRUE
  )

 # won't have any records
  cdm$cohort1_d <- requireIsEntry(
    cohort = cdm$cohort1,
    entryRange = c(50, Inf),
    cohortId = 1,
    name = "cohort1_d")
 expect_true(nrow(cdm$cohort1_d |> dplyr::collect()) == 0)

 # errors
 expect_error(cdm$cohort1 |> requireIsEntry(entryRange = c(1,2,3)))
 expect_error(cdm$cohort1 |> requireIsEntry(entryRange = c(1,NA)))
 expect_error(cdm$cohort1 |> requireIsEntry(entryRange = "a"))
 expect_error(cdm$cohort1 |> requireIsEntry(entryRange = c(1, 1), name = 1))
 expect_error(cdm$cohort1 <- cdm$cohort1 |> requireIsEntry(entryRange = c(1, 1), name = "cohort2"))
 expect_warning(cdm$cohort1 |> requireIsEntry(entryRange = c(1, 1), cohortId = Inf))
 expect_error(cdm$cohort1 |> dplyr::collect() |> requireIsEntry(entryRange = c(1, 1)))
 expect_warning(cdm$cohort1 |> requireIsEntry(entryRange = c(1, 1), cohortId = c(1, 5)))


 # mock cohort
 cdm_local <- omock::mockCdmReference() |>
   omock::mockPerson(n = 10,seed = 1) |>
   omock::mockObservationPeriod(seed = 1) |>
   omock::mockCohort(name = "cohort1",
                     numberCohorts = 1,
                     recordPerson = 4,
                     seed = 1)
 cdm <- cdm_local |> copyCdm()
 cdm <- omopgenerics::insertTable(cdm, "observation_period",
                           data.frame(observation_period_id = c(1,2),
                                      person_id = c(1,2),
                                      observation_period_start_date  = as.Date("2010-01-01"),
                                      observation_period_end_date  = as.Date("2011-01-01"),
                                      period_type_concept_id = NA_integer_)
                           )
 cdm <- omopgenerics::insertTable(cdm, "my_cohort",
                                  data.frame(cohort_definition_id = 1L,
                                             subject_id = c(1,2, 2, 2, 2),
                                             cohort_start_date  = c(as.Date("2010-01-01"),
                                                                                as.Date("2010-10-01"),
                                                                                as.Date("2010-05-01"),
                                                                                as.Date("2010-06-01"),
                                                                                as.Date("2010-07-01")),
                                             cohort_end_date  = c(as.Date("2010-01-01"),
                                                                              as.Date("2010-10-01"),
                                                                              as.Date("2010-05-01"),
                                                                              as.Date("2010-06-01"),
                                                                              as.Date("2010-07-01")))
 )
 cdm$my_cohort <- cdm$my_cohort |> omopgenerics::newCohortTable(cohortSetRef = data.frame(cohort_definition_id = 1L,
                                                                                          cohort_name = "cohort"))
 cdm$my_cohort_1 <- requireIsEntry(
   cohort = cdm$my_cohort,
   entryRange = c(2, 3),
   name = "my_cohort_1")

 expect_equal(sort(cdm$my_cohort_1 |>
   dplyr::pull("cohort_start_date")),
   as.Date(c("2010-06-01","2010-07-01")),
   ignore_attr = TRUE)
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
    writeSchema = c(schema =  Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
                     prefix = "cc_"),
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
  cdm$my_cohort <- requireIsEntry(cdm$my_cohort, entryRange = c(0,2))
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  cdm$my_cohort <- requireIsEntry(cdm$my_cohort, entryRange = c(1,Inf))
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  cdm$my_cohort <- requireIsFirstEntry(cdm$my_cohort)
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  cdm$my_cohort <- requireIsLastEntry(cdm$my_cohort)
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
