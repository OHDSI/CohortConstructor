test_that("subsetCohort works", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()

  # Subset 1 cohort
  cdm$cohort2 <- subsetCohorts(cdm$cohort1, cohortId = 1, name = "cohort2")
  expect_true(unique(cdm$cohort2 |> dplyr::pull("cohort_definition_id")) == 1)
  expect_true(settings(cdm$cohort2) |> dplyr::pull("cohort_definition_id") == 1)
  expect_true(unique(attrition(cdm$cohort2) |> dplyr::pull("cohort_definition_id")) == 1)
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(.data$cohort_definition_id == 1) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_identical(attrition(cdm$cohort1) |> dplyr::filter(.data$cohort_definition_id == 1), attrition(cdm$cohort2))

  # subset more than 1 cohort
  cdm$cohort3 <- subsetCohorts(cdm$cohort1, c(3,4,5), name = "cohort3")
  expect_true(all(unique(cdm$cohort3|> dplyr::pull("cohort_definition_id")) == 3:5))
  expect_true(all(settings(cdm$cohort3) |> dplyr::pull("cohort_definition_id") == 3:5))
  expect_true(all(unique(attrition(cdm$cohort3) |> dplyr::pull("cohort_definition_id")) == 3:5))
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(.data$cohort_definition_id %in% 3:5) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_identical(attrition(cdm$cohort1) |> dplyr::filter(.data$cohort_definition_id %in% 3:5), attrition(cdm$cohort3))

  # same name
  cohort <- cdm$cohort1
  cdm$cohort1 <- subsetCohorts(cdm$cohort1, c(3,4,5))
  expect_true(all(unique(cdm$cohort1|> dplyr::pull("cohort_definition_id")) == 3:5))
  expect_true(all(settings(cdm$cohort1) |> dplyr::pull("cohort_definition_id") == 3:5))
  expect_true(all(unique(attrition(cdm$cohort1) |> dplyr::pull("cohort_definition_id")) == 3:5))
  expect_true(all(
    cohort |> dplyr::filter(.data$cohort_definition_id %in% 3:5) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_identical(attrition(cohort) |> dplyr::filter(.data$cohort_definition_id %in% 3:5), attrition(cdm$cohort1))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("codelist works", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(seed = 1)
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1, 2, 3),
    "concept_name" = c("my concept 1", "my concept 2", "my concept 3"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:17,
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 4, 4, 1, 2, 3, 4),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, 430, -10, 100, 123, -10, 1000),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, 400, -100, NA, 190, 123, 1500),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )
  cdm_local$observation_period <- cdm_local$observation_period|>
    dplyr::mutate(observation_period_start_date = as.Date("1990-01-01"), observation_period_end_date = as.Date("2020-01-01"))

  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- conceptCohort(cdm, conceptSet = list(c1 = c(1,3), c2 = c(2)), name = "cohort1")

  # Subset 1 cohort
  cdm$cohort2 <- subsetCohorts(cdm$cohort1, 1, name = "cohort2")
  expect_identical(attr(cdm$cohort2, "cohort_codelist") |> dplyr::collect(), attr(cdm$cohort1, "cohort_codelist") |> dplyr::filter(cohort_definition_id == 1) |> dplyr::collect())

  # same name
  cohort <- cdm$cohort1
  cdm$cohort1 <- subsetCohorts(cdm$cohort1, 2)
  expect_identical(attr(cdm$cohort1, "cohort_codelist") |> dplyr::collect(), attr(cohort, "cohort_codelist") |> dplyr::filter(cohort_definition_id == 2) |> dplyr::collect())

  PatientProfiles::mockDisconnect(cdm)
})

test_that("Expected behaviour", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2)
  cdm <- cdm_local |> copyCdm()

  # Subset 1 cohort
  expect_error(cdm$cohort2 <- subsetCohorts("cohort1", 1, name = "cohort2"))
  expect_error(cdm$cohort2 <- subsetCohorts(cdm$cohort1, "1", name = "cohort2"))
  expect_error(cdm$cohort2 <- subsetCohorts(cdm$cohort1, 2, name = "cohort3"))
  expect_error(cdm$cohort2 <- subsetCohorts(cdm$cohort1, 10, name = "cohort2"))
  expect_no_error(cohort <- subsetCohorts(cdm$cohort1, NULL))
  expect_identical(cohort, cdm$cohort1)

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
                                                      sex = "Female"))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- subsetCohorts(cdm$my_cohort, cohortId = 1)
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdm_disconnect(cdm = cdm)
})
