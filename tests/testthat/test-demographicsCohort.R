test_that("input validation", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(18,40), sex = "Male")
  )

  expect_error(
    cdm$cohort4 <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(18,40), sex = "Male")
  )

  expect_error(
    cohort <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(40,18), sex = "Male")
  )

  expect_no_error(
    cohort <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(18,40), sex = "Male", minPriorObservation = 15)
  )

  expect_error(
    cohort <- cdm |>
      demographicsCohort(name = "cohort3", minPriorObservation = -15)
  )

  expect_error(
    cohort <- cdm |>
      demographicsCohort(name = "cohort3", minPriorObservation = "15")
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("Example: sex", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3", sex = "Male")
  )

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addSex()

  expect_true(all(cdm$cohort3 |> dplyr::pull("sex")== "Male"))

  expect_true(
    setequal((cdm$cohort3 |>
                dplyr::pull("subject_id")),
             (cdm$person |> dplyr::filter(gender_concept_id==8507) |> dplyr::pull("person_id"))
    )
  )

  cdm$cohort3 <- cdm$cohort3 |>
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) |>
    dplyr::mutate(check1 = (cohort_start_date == observation_period_start_date),
                  check2 = (cohort_end_date == observation_period_end_date)) |>
    dplyr::compute()

  expect_true(all(cdm$cohort3 |> dplyr::pull("check1")== TRUE))
  expect_true(all(cdm$cohort3 |> dplyr::pull("check2")== TRUE))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("Example: ageRange", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3", ageRange = c(18, 40))
  )

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addAge()

  expect_true(all(cdm$cohort3 |> dplyr::pull("age") >= 18))
  expect_true(all(cdm$cohort3 |> dplyr::pull("age") <= 40))

  cdm$cohort3 <- cdm$cohort3 |>
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) |>
    dplyr::mutate(check1 = (cohort_start_date >= observation_period_start_date),
                  check2 = (cohort_end_date <= observation_period_end_date)) |>
    dplyr::compute()

  expect_true(all(cdm$cohort3 |> dplyr::pull("check1")== TRUE))
  expect_true(all(cdm$cohort3 |> dplyr::pull("check2")== TRUE))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("Example: priorObs", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()
  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3", minPriorObservation = 15)
  )

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addPriorObservation()

  expect_true(all(cdm$cohort3 |> dplyr::pull("prior_observation") == 15))

  cdm$cohort3 <- cdm$cohort3 |>
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) |>
    dplyr::mutate(check1 = (cohort_start_date > observation_period_start_date),
                  check2 = (cohort_end_date == observation_period_end_date)) |>
    dplyr::compute()

  expect_true(all(cdm$cohort3 |> dplyr::pull("check1")== TRUE))
  expect_true(all(cdm$cohort3 |> dplyr::pull("check2")== TRUE))

  loc_cohort3 <- cdm$cohort3 |>
    dplyr::collect() |>
    dplyr::mutate(check3 = observation_period_start_date + 15) |>
    dplyr::mutate(check3 = (check3 == cohort_start_date))

  expect_true(all(loc_cohort3 |> dplyr::pull("check3")== TRUE))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("Example: mixture of parameters", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()

  isDuckdb <- attr(omopgenerics::cdmSource(cdm), "source_type") == "duckdb"
  if(isDuckdb){
    startTempTables <- countDuckdbTempTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
    startPermanentTables <- countDuckdbPermanentTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
  }

  expect_no_error(
    cdm$cohort3 <- cdm |>
      demographicsCohort(name = "cohort3",
                         ageRange = c(18,90),
                         sex = "Male",
                         minPriorObservation = 25)
  )

  if(isDuckdb){
    endTempTables <- countDuckdbTempTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
    endPermanentTables <- countDuckdbPermanentTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
    # we should have only added 4 permanent tables (the new cohort table and
    # three tables with settings, attrition, and codelist)
    # no temp tables will have been created
    expect_true(startTempTables == endTempTables)
    expect_true(
      startPermanentTables + 4 == endPermanentTables
    )
  }

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addPriorObservation()

  expect_true(all(cdm$cohort3 |> dplyr::pull("prior_observation") >= 25))

  cdm$cohort3 <- cdm$cohort3 |>
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) |>
    dplyr::mutate(check1 = (cohort_start_date > observation_period_start_date),
                  check2 = (cohort_end_date == observation_period_end_date)) |>
    dplyr::compute()

  expect_true(all(cdm$cohort3 |> dplyr::pull("check1")== TRUE))
  expect_true(all(cdm$cohort3 |> dplyr::pull("check2")== TRUE))

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addAge()

  expect_true(all(cdm$cohort3 |> dplyr::pull("age") >= 18))
  expect_true(all(cdm$cohort3 |> dplyr::pull("age") <= 50))

  cdm$cohort3 <- cdm$cohort3 |>
    PatientProfiles::addSex()

  expect_true(all(cdm$cohort3 |> dplyr::pull("sex")== "Male"))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
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

  cdm$my_cohort <- demographicsCohort(cdm, name = "my_cohort", ageRange = list(c(0, 50)))

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
