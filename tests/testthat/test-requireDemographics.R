test_that("test it works and expected errors", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 10, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(seed = 1)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- cdm$cohort |>
    requireDemographics(
      ageRange = c(0, 35),
      indexDate = "cohort_start_date",
      sex = "Both",
      minPriorObservation = 10,
      minFutureObservation = 40,
      name = "cohort1"
    )
  expect_true(inherits(cdm$cohort1, "cohort_table"))
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") == c(1, 1, 3, 4)))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") ==
                    c("2001-05-30", "2003-05-02", "2015-01-27", "1996-06-30")))
  expect_identical(settings(cdm$cohort1), dplyr::tibble(
      cohort_definition_id = 1L,
      cohort_name = "cohort_1",
      age_range = "0_35",
      sex = "Both",
      min_prior_observation = 10,
      min_future_observation = 40
    ))
  expect_true(all(
    attrition(cdm$cohort1)$reason ==
      c('Initial qualifying events', 'Age requirement: 0 to 35',
        'Sex requirement: Both', 'Prior observation requirement: 10 days',
        'Future observation requirement: 40 days')
  ))

  cdm$cohort <- cdm$cohort |>
    requireAge(ageRange = list(c(0, 35))) |>
    requireSex(sex = "Both") |>
    requirePriorObservation(minPriorObservation = 10) |>
    requireFutureObservation(minFutureObservation = 40)

  expect_true(inherits(cdm$cohort, "cohort_table"))
  expect_identical(omopgenerics::attrition(cdm$cohort), omopgenerics::attrition(cdm$cohort1))
  expect_true(all(cdm$cohort |> dplyr::pull("subject_id") |> sort() == c(1, 1, 3, 4)))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1996-06-30", "2001-05-30", "2003-05-02", "2015-01-27")))
  expect_identical(settings(cdm$cohort), dplyr::tibble(
      cohort_definition_id = 1L,
      cohort_name = "cohort_1",
      age_range = "0_35",
      sex = "Both",
      min_prior_observation = 10,
      min_future_observation = 40
    ))
  expect_true(all(
    attrition(cdm$cohort)$reason ==
      c('Initial qualifying events', 'Age requirement: 0 to 35',
        'Sex requirement: Both', 'Prior observation requirement: 10 days',
        'Future observation requirement: 40 days')
  ))

  # expect errors
  expect_error(requireDemographics(cohort = "cohort"))
  expect_error(requireDemographics(cohort = cdm$person))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    indexDate = "aaa"
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    ageRange = c(0, 50)
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    ageRange = list(c(0, 50, 100))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    ageRange = list(c(50, 0))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    sex = "all"
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    ageRange = list(c(-10, 40))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    ageRange = list(c(0, "a"))
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    sex = "a"
  ))

  expect_error(requireDemographics(
    cohort = cdm$cohort,
    minPriorObservation = -10
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort2,
    minPriorObservation = "a"
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    minFutureObservation = -10
  ))
  expect_error(requireDemographics(
    cohort = cdm$cohort,
    minFutureObservation = "a"
  ))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("restrictions applied to single cohort", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(recordPerson = 3,seed = 1)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()
  cdm$cohort1 <- cdm$cohort |>
    requireDemographics(ageRange = list(c(0, 5)), name = "cohort1")
  expect_true(all(c("2001-03-30", "2003-06-15") == cdm$cohort1 |> dplyr::pull("cohort_start_date")))
  expect_true(all(
    c("Initial qualifying events", "Age requirement: 0 to 5", "Sex requirement: Both",
      "Prior observation requirement: 0 days", "Future observation requirement: 0 days") ==
      omopgenerics::attrition(cdm$cohort1)$reason))
  expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
                  == colnames(cdm$cohort1)))
  expect_true(settings(cdm$cohort1)$cohort_definition_id == 1)
  expect_true(settings(cdm$cohort1)$cohort_name == "cohort_1")
  expect_true(settings(cdm$cohort1)$age_range == "0_5")
  expect_true(settings(cdm$cohort1)$sex == "Both")
  expect_true(settings(cdm$cohort1)$min_prior_observation == 0)
  expect_true(settings(cdm$cohort1)$min_future_observation == 0)

  cdm$cohort2 <- cdm$cohort |>
    requireDemographics(sex = "Male", name = "cohort2")
  expect_identical(dplyr::collect(cdm$cohort)$cohort_start_date |> sort(),
                   dplyr::collect(cdm$cohort2)$cohort_start_date |> sort())
  expect_true(all(
    c("Initial qualifying events", "Age requirement: 0 to 150", "Sex requirement: Male",
      "Prior observation requirement: 0 days", "Future observation requirement: 0 days") ==
      omopgenerics::attrition(cdm$cohort2)$reason))
  expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
                  == colnames(cdm$cohort2)))
  expect_true(settings(cdm$cohort2)$cohort_definition_id == 1)
  expect_true(settings(cdm$cohort2)$cohort_name == "cohort_1")
  expect_true(settings(cdm$cohort2)$age_range == "0_150")
  expect_true(settings(cdm$cohort2)$sex == "Male")
  expect_true(settings(cdm$cohort2)$min_prior_observation == 0)
  expect_true(settings(cdm$cohort2)$min_future_observation == 0)

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("ignore existing cohort extra variables", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(recordPerson = 3,seed = 1)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort <- cdm$cohort |>
    PatientProfiles::addDemographics() |>
    dplyr::compute(name = "cohort", temporary = FALSE)

  cdm$cohort <- cdm$cohort |>
    requirePriorObservation(minPriorObservation = 450)
  expect_true(all(colnames(cdm$cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date",
                      "age", "sex", "prior_observation", "future_observation")))
  expect_true(cdm$cohort |> dplyr::tally() |> dplyr::pull() == 2)
  expect_true(all(c("Initial qualifying events", "Prior observation requirement: 450 days") ==
                    omopgenerics::attrition(cdm$cohort)$reason))
  expect_true(all(colnames(settings(cdm$cohort)) == c("cohort_definition_id", "cohort_name", "min_prior_observation")))

  cdm$new_cohort <- cdm$cohort |>
    requirePriorObservation(minPriorObservation = 450, name = "new_cohort")
  expect_true(all(colnames(cdm$new_cohort) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date",
                      "age", "sex", "prior_observation", "future_observation")))
  expect_true(cdm$new_cohort |> dplyr::tally() |> dplyr::pull() == 2)
  expect_true(all(c("Initial qualifying events", "Prior observation requirement: 450 days",
                    "Prior observation requirement: 450 days") ==
                    omopgenerics::attrition(cdm$new_cohort)$reason))
  expect_true(all(colnames(settings(cdm$cohort)) == c("cohort_definition_id", "cohort_name", "min_prior_observation")))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("external columns kept after requireDemographics", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(recordPerson = 3,seed = 1)
  cdm_local$cohort <- cdm_local$cohort |>
    dplyr::mutate(
      col_extra1 = as.numeric(subject_id) + 1,
      col_extra2 = as.numeric(subject_id) + 2,
      new_index_date = cohort_start_date + 1
    )
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort <- cdm$cohort |>
    requireDemographics(indexDate = "new_index_date", ageRange = list(c(0,5)))

  expect_true(all(c("col_extra1", "col_extra2", "new_index_date") %in% colnames(cdm$cohort)))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("cohortIds", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(numberCohorts = 3,seed = 1)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$new_cohort <- requireSex(cohort = cdm$cohort, cohortId = 1, sex = "Male", name = "new_cohort") |>
    requirePriorObservation(cohortId = "cohort_3", minPriorObservation = 1000, name = "new_cohort")
  expect_true(all(
    omopgenerics::attrition(cdm$new_cohort)$reason ==
      c("Initial qualifying events", "Sex requirement: Male", "Initial qualifying events" ,
        "Initial qualifying events", "Prior observation requirement: 1000 days")
  ))
  expect_true(all(cdm$new_cohort |> dplyr::pull("cohort_definition_id") == c(2,2,2)))
  expect_true(all(cdm$new_cohort |> dplyr::pull("subject_id") == c(1,1,1)))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("settings with extra columns", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(numberCohorts = 3, seed = 4)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort <- cdm$cohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm$cohort) |>
        dplyr::mutate(sex = "Both", extra1 = 1, extra2 = "hi")
      )
  cdm$cohort <- cdm$cohort |> requireSex(sex = c("Both"))
  expect_identical(cdm$cohort |> settings() |> dplyr::arrange(.data$cohort_definition_id), dplyr::tibble(
      cohort_definition_id = as.integer(1:3),
      cohort_name = c("cohort_1", "cohort_2", "cohort_3"),
      extra1 = 1,
      extra2 = "hi",
      sex = c(rep("Both", 3))
    ))
  expect_true(all(colnames(attrition(cdm$cohort)) ==
                    c("cohort_definition_id", "number_records", "number_subjects", "reason_id", "reason", "excluded_records", "excluded_subjects" )))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("Inf age", {

  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(numberCohorts = 3, seed = 4)
  # to remove in new omock
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  expect_no_error(cdm$cohort1 <- cdm$cohort |>
    requireDemographics(ageRange = c(0, Inf),
                        name = "cohort1"))
  expect_error(cdm$cohort2 <-cdm$cohort |>
                    requireDemographics(ageRange = list(c(0, 17),
                                                        c(18,Inf)),
                                        name = "cohort2"))

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

  omopgenerics::dropSourceTable(cdm = cdm, dplyr::contains("og_"))
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_cohort",
                                   table = data.frame(cohort_definition_id = 1L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-02"),
                                                      cohort_end_date = as.Date("2009-01-03"),
                                                      other_date = as.Date("2009-01-01")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- requireDemographics(cdm$my_cohort)
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og_", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})

