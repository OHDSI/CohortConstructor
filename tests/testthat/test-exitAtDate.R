test_that("exit at observation end", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 2,seed = 1)
  cdm <- cdm_local |> copyCdm()
  # simple example - test it works
  cdm$cohort1 <- cdm$cohort |> exitAtObservationEnd(name = "cohort1")
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2003-05-17", "2015-02-25")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2003-06-15", "2013-06-29", "2013-06-29", "2015-10-11")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 2, 3)))

  # test cohort id and name
  cdm$cohort <- cdm$cohort |> exitAtObservationEnd(cohortId = 1)
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17", "2015-02-25")))
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2001-11-27", "2002-01-29", "2002-06-12", "2003-06-15", "2005-01-15", "2013-06-29", "2015-10-11")))
  expect_true(all(cdm$cohort |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 1, 1, 1, 2, 3)))
  expect_true(all(attrition(cdm$cohort)$reason == c("Initial qualifying events", "Exit at observation period end date, limited to current observation period", "Initial qualifying events")))

  # additional columns warning
  expect_warning(cdm$cohort <- cdm$cohort |> dplyr::mutate(extra_col = 1) |> exitAtObservationEnd())
  expect_true(all(colnames(cdm$cohort) == c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  # expected errors
  expect_error(cdm$cohort |> exitAtObservationEnd(name = 1))
  expect_warning(cdm$cohort |> exitAtObservationEnd(cohortId = "HI"))
  expect_error(cdm$person |> exitAtObservationEnd())
  PatientProfiles::mockDisconnect(cdm)


  # multiple observation periods
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1)
  cdm_local$observation_period <- dplyr::tibble(
    "observation_period_id" = as.integer(1:8),
    "person_id" = as.integer(c(1, 1, 1, 2, 2, 3, 4, 1)),
    "observation_period_start_date" = as.Date(c(
      "2000-01-01", "2001-01-01", "2003-01-01", "2001-01-01", "2002-01-01",
      "2000-01-01", "2000-01-01", "1997-01-01"
    )),
    "observation_period_end_date" =as.Date(c(
      "2000-12-20", "2002-01-01", "2005-01-01", "2001-12-31", "2004-01-01",
      "2004-01-01", "2003-01-01", "1998-01-01"
    )),
    "period_type_concept_id" = NA_integer_
  )
  cdm_local$cohort <- dplyr::tibble(
    "cohort_definition_id" = as.integer(c(1, 1, 1, 1, 2, 2)),
    "subject_id" = as.integer(c(1, 1, 1, 2, 2, 1)),
    "cohort_start_date" = as.Date(c(
      "2000-01-12", "2000-12-12", "2001-01-12", "2001-01-12", "2002-01-12", "2003-01-12"
    )),
    "cohort_end_date" =as.Date(c(
      "2000-05-20", "2000-12-20", "2001-04-01", "2001-12-30", "2003-01-01", "2004-01-01"
    ))
  )
  cdm <- cdm_local |> copyCdm()
  cdm$cohort <- cdm$cohort |> omopgenerics::newCohortTable()
  # limit to first
  cdm$cohort1 <- cdm$cohort |> exitAtObservationEnd(name = "cohort1")
  expect_equal(
    collectCohort(cdm$cohort1, 1),
    dplyr::tibble(
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2000-01-12", "2001-01-12", "2001-01-12")),
      cohort_end_date = as.Date(c("2000-12-20", "2002-01-01", "2001-12-31"))
    )
  )
  expect_equal(
    collectCohort(cdm$cohort1, 2),
    dplyr::tibble(
      subject_id = c(1, 2),
      cohort_start_date = as.Date(c("2003-01-12", "2002-01-12")),
      cohort_end_date = as.Date(c("2005-01-01", "2004-01-01"))
    )
  )
  expect_true(attrition(cdm$cohort1)$reason[2] ==
                "Exit at observation period end date, limited to current observation period")
  # not limit to first
  cdm$cohort2 <- cdm$cohort |> exitAtObservationEnd(name = "cohort2", limitToCurrentPeriod = FALSE)
  expect_equal(
    collectCohort(cdm$cohort2, 1),
    dplyr::tibble(
      subject_id = c(1, 1, 1, 2, 2),
      cohort_start_date = as.Date(c("2000-01-12", "2001-01-01", "2003-01-01", "2001-01-12", "2002-01-01")),
      cohort_end_date = as.Date(c("2000-12-20", "2002-01-01", "2005-01-01", "2001-12-31", "2004-01-01"))
    )
  )
  expect_equal(
    collectCohort(cdm$cohort2, 2),
    dplyr::tibble(
      subject_id = c(1, 2),
      cohort_start_date = as.Date(c("2003-01-12", "2002-01-12")),
      cohort_end_date = as.Date(c("2005-01-01", "2004-01-01"))
    )
  )
  expect_true(attrition(cdm$cohort2)$reason[2] ==
                "Exit at observation period end date")
  # not limit to first + only first cohort
  cdm$cohort3 <- cdm$cohort |> exitAtObservationEnd(name = "cohort3", limitToCurrentPeriod = FALSE, cohortId = 1)
  expect_equal(
    collectCohort(cdm$cohort3, 1),
    collectCohort(cdm$cohort2, 1)
  )
  expect_equal(
    collectCohort(cdm$cohort3, 2),
    collectCohort(cdm$cohort, 2)
  )

  PatientProfiles::mockDisconnect(cdm)
})

test_that("exit at death date", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 2,seed = 1)
  cdm_local$death <- dplyr::tibble(
    person_id = 1:2,
    death_date = as.Date(c("2013-06-29", "2003-06-15")),
    death_type_concept_id = NA
  )
  cdm <- cdm_local |> copyCdm()

  # simple example - require death TRUE works
  cdm$cohort1 <- cdm$cohort |> exitAtDeath(requireDeath = TRUE, name = "cohort1")
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2003-05-17")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2003-06-15", "2013-06-29", "2013-06-29")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 2)))
  expect_true(all(attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events", "No death recorded", "Exit at death", "Initial qualifying events", "No death recorded", "Exit at death")))

  # simple example - require death FALSE works
  cdm$cohort2 <- cdm$cohort |> exitAtDeath(requireDeath = FALSE, name = "cohort2")
  expect_true(all(cdm$cohort2 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2003-05-17", "2015-02-25")))
  expect_true(all(cdm$cohort2 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2003-06-15", "2013-06-29", "2013-06-29", "2015-04-30")))
  expect_true(all(cdm$cohort2 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 2, 3)))
  expect_true(all(attrition(cdm$cohort2)$reason ==
                    c("Initial qualifying events", "Exit at death", "Initial qualifying events", "Exit at death")))

  # cohort ID and name
  cdm$cohort <- cdm$cohort |> exitAtDeath(cohortId = 1, requireDeath = TRUE)
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17")))
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2001-11-27", "2002-01-29", "2002-06-12", "2003-06-15", "2005-01-15", "2013-06-29")))
  expect_true(all(cdm$cohort |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 1, 1, 1, 2)))
  expect_true(all(attrition(cdm$cohort)$reason ==
                    c("Initial qualifying events", "No death recorded", "Exit at death", "Initial qualifying events")))

  # columns warning
  expect_warning(cdm$cohort <- cdm$cohort |> dplyr::mutate(extra_col = 1) |> exitAtDeath())
  expect_true(all(colnames(cdm$cohort) == c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  # expected errors
  expect_error(cdm$cohort |> exitAtDeath(name = 1))
  expect_warning(cdm$cohort |> exitAtDeath(cohortId = "HI"))
  expect_error(cdm$person |> exitAtDeath())
  expect_error(cdm$person |> exitAtDeath(requireDeath = 1))

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
  cdm$my_cohort <- exitAtObservationEnd(cdm$my_cohort)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  cdm$my_cohort <- exitAtDeath(cdm$my_cohort)

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
