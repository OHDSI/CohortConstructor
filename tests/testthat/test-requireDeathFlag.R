test_that("requiring death", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2)
  cdm_local$death <- dplyr::tibble(
    person_id = c(1,3),
    death_date = as.Date(c("2013-06-29", "2015-04-14")),
    death_type_concept_id = NA
  )
  cdm <- cdm_local |> copyCdm()

  cdm$cohort3 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   window = c(0, Inf),
                                   name = "cohort3")
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") %in% c(1,3)))
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(subject_id %in% c(1,3)) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date")))

# censor
  cdm$cohort4 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   window = c(0, Inf),
                                   censorDate = "cohort_end_date",
                                   name = "cohort4")
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") == c(3, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date",
                      "Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date")))


  # index date
  cdm$cohort5 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   window = c(0, 365),
                                   indexDate = "cohort_end_date",
                                   name = "cohort5")
  expect_true(all(cdm$cohort5 |> dplyr::pull("subject_id") %in% 3))
  expect_true(all(
    cdm$cohort5 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("2015-03-25")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events", "Death between 0 & 365 days relative to cohort_end_date",
                      "Initial qualifying events", "Death between 0 & 365 days relative to cohort_end_date")))

  # cohort Id
  cdm$cohort6 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   cohortId = 1,
                                   window = c(0, 365),
                                   name = "cohort6")
  expect_true(all(cdm$cohort6 |> dplyr::pull("subject_id") |> sort() %in% c(2,2,2,2)))
  expect_true(all(
    cdm$cohort6 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1999-04-19", "2000-02-04", "2000-03-12", "2000-08-05")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort6)$reason ==
                    c("Initial qualifying events",
                      "Death between 0 & 365 days relative to cohort_start_date",
                      "Initial qualifying events")))

  # name
  cdm$cohort1 <-  requireDeathFlag(cohort = cdm$cohort1, window = list(c(0, Inf)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "Death between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Death between 0 & Inf days relative to cohort_start_date")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("not death", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 3)
  cdm_local$death <- dplyr::tibble(
    person_id = c(1,3),
    death_date = as.Date(c("2013-06-29", "2015-10-11")),
    death_type_concept_id = NA
  )
  cdm <- cdm_local |> copyCdm()

  cdm$cohort3 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   window = c(0, Inf),
                                   name = "cohort3",
                                   negate = TRUE)

  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") %in% c(2,4)))
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(subject_id %in% c(2,4)) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events", "Alive between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events", "Alive between 0 & Inf days relative to cohort_start_date")))

  # censor Id
  cdm$cohort4 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   cohortId = "cohort_1",
                                   window = c(0, Inf),
                                   name = "cohort4",
                                   negate = TRUE)
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 1, 2, 4, 4)))
  expect_true(all(
    cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1992-02-15", "1992-04-20", "1999-07-17", "2001-09-10", "2002-08-02", "2004-06-18")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events", "Alive between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events")))

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
  cdm$my_cohort <- requireDeathFlag(cdm$my_cohort, window = list(c(0, Inf)))
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
