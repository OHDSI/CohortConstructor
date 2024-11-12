test_that("mearurementCohorts works", {
  skip_on_cran()

  cdm <- mockCohortConstructor(con = NULL, seed = 1)
  cdm$concept <- cdm$concept |>
    dplyr::union_all(
      dplyr::tibble(
        concept_id = c(4326744, 4298393, 45770407, 8876, 4124457, 999999, 123456) |> as.integer(),
        concept_name = c("Blood pressure", "Systemic blood pressure",
                         "Baseline blood pressure", "millimeter mercury column",
                         "Normal range", "Normal", "outObs"),
        domain_id = "Measurement",
        vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "UCUM", "SNOMED", "SNOMED", "hi"),
        standard_concept = "S",
        concept_class_id = c("Observable Entity", "Observable Entity",
                             "Observable Entity", "Unit", "Qualifier Value",
                             "Qualifier Value", "hi"),
        concept_code = NA,
        valid_start_date = NA,
        valid_end_date = NA,
        invalid_reason = NA
      )
    )
  cdm$measurement <- dplyr::tibble(
    measurement_id = 1:7L,
    person_id = as.integer(c(1, 1, 2, 3, 3, 1, 1)),
    measurement_concept_id = c(4326744, 4298393, 4298393, 45770407, 45770407, 123456, 123456) |> as.integer(),
    measurement_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20", "1900-01-01", "2050-01-01")),
    measurement_type_concept_id = NA_integer_,
    value_as_number = c(100, 125, NA, NA, NA, NA, NA) |> as.integer(),
    value_as_concept_id = c(0, 0, 0, 4124457, 999999, 0, 0) |> as.integer(),
    unit_concept_id = c(8876, 8876, 0, 0, 0, 0, 0) |> as.integer()
  )
  cdm <- cdm |> copyCdm()

  isDuckdb <- attr(omopgenerics::cdmSource(cdm), "source_type") == "duckdb"
  if(isDuckdb){
    startTempTables <- countDuckdbTempTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
    startPermanentTables <- countDuckdbPermanentTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
  }

  # simple example
  cdm$cohort <- measurementCohort(
    cdm = cdm,
    name = "cohort",
    conceptSet = list("normal_blood_pressure" = c(4326744L, 4298393L, 45770407L)),
    valueAsConcept = c(4124457),
    valueAsNumber = list("8876" = c(70L, 120L))
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

  expect_identical(collectCohort(cdm$cohort, 1), dplyr::tibble(
      subject_id = c(1L, 3L),
      cohort_start_date = as.Date(c("2000-07-01", "2015-02-19")),
      cohort_end_date = as.Date(c("2000-07-01", "2015-02-19"))
    ))
  expect_identical(
    cdm$cohort |> attrition() |> dplyr::as_tibble(),
    dplyr::tibble(
      "cohort_definition_id" = 1L,
      "number_records" = c(rep(2L, 4)),
      "number_subjects" = c(rep(2L, 4)),
      "reason_id" = 1:4L,
      "reason" = c(
        "Initial qualifying events",
        "Not missing record date", "Record in observation",
        "Distinct measurement records"
      ),
      "excluded_records" = c(0L, 0L, 0L, 0L),
      "excluded_subjects" = c(0L, 0L, 0L, 0L),
    )
  )
  expect_true(settings(cdm$cohort)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id |> sort() == c(4298393, 4326744, 45770407)))
  expect_identical(settings(cdm$cohort), dplyr::tibble(
      "cohort_definition_id" = 1L, "cohort_name" = "normal_blood_pressure",
      "cdm_version" = attr(cdm, "cdm_version"), "vocabulary_version" = "mock"
    ))

  # non valid concept ----
  cdm$cohort3 <- measurementCohort(
    cdm = cdm,
    name = "cohort3",
    conceptSet = list("normal_blood_pressure" = c(4326744L, 4298393L, 45770407L, 12345L)),
    valueAsConcept = c(4124457),
    valueAsNumber = list("8876" = c(70, 120))
  )
  expect_identical(collectCohort(cdm$cohort3, 1), dplyr::tibble(
      subject_id = c(1L, 3L),
      cohort_start_date = as.Date(c("2000-07-01", "2015-02-19")),
      cohort_end_date = as.Date(c("2000-07-01", "2015-02-19"))
    ))
  expect_true(settings(cdm$cohort3)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort3, "cohort_codelist") |> dplyr::collect()
  expect_true(all(c(4298393, 4326744, 45770407) %in% codes$concept_id))

  # no values ----
  cdm$cohort4 <- measurementCohort(
    cdm = cdm,
    name = "cohort4",
    conceptSet = list("normal_blood_pressure" = c(4326744L, 4298393L, 45770407L))
  )
  expect_identical(collectCohort(cdm$cohort4, 1), dplyr::tibble(
      subject_id = as.integer(c(1, 1, 2, 3, 3)),
      cohort_start_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20")),
      cohort_end_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20"))
    ))
  expect_true(settings(cdm$cohort4)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort4, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(4298393, 4326744, 45770407)))

  # number values ----
  cdm$cohort5 <- measurementCohort(
    cdm = cdm,
    name = "cohort5",
    conceptSet = list("normal_blood_pressure" = c(4326744L, 4298393L, 45770407L)),
    valueAsNumber = list("8876" = c(70, 120), "908" = c(800, 900))
  )
  expect_identical(collectCohort(cdm$cohort5, 1), dplyr::tibble(
      subject_id = 1L,
      cohort_start_date = as.Date(c("2000-07-01")),
      cohort_end_date = as.Date(c("2000-07-01"))
    ))
  expect_true(settings(cdm$cohort5)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort5, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(4298393, 4326744, 45770407)))

  # concept values ----
  cdm$cohort6 <- measurementCohort(
    cdm = cdm,
    name = "cohort6",
    conceptSet = list("normal_blood_pressure" = c(4326744L, 4298393L, 45770407L)),
    valueAsConcept = c(4124457, 999999, 12345)
  )
  expect_identical(collectCohort(cdm$cohort6, 1), dplyr::tibble(
      subject_id = c(3L, 3L),
      cohort_start_date = as.Date(c("2015-02-19", "2015-02-20")),
      cohort_end_date = as.Date(c("2015-02-19", "2015-02-20"))
    ))
  expect_true(settings(cdm$cohort6)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort6, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(4298393, 4326744, 45770407)))

  # more than 1 cohort ----
  cdm$cohort7 <- measurementCohort(
    cdm = cdm,
    name = "cohort7",
    conceptSet = list("c1" = c(4326744L), "c2" = c(4298393L, 45770407L))
  )
  expect_identical(collectCohort(cdm$cohort7, 1), dplyr::tibble(
      subject_id = c(1L),
      cohort_start_date = as.Date(c("2000-07-01")),
      cohort_end_date = as.Date(c("2000-07-01"))
    ))
  expect_identical(collectCohort(cdm$cohort7, 2), dplyr::tibble(
      subject_id = as.integer(c(1, 2, 3, 3)),
      cohort_start_date = as.Date(c("2000-12-11", "2002-09-08", "2015-02-19","2015-02-20")),
      cohort_end_date = as.Date(c("2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20"))
    ))
  expect_identical(
    attrition(cdm$cohort7) |> dplyr::as_tibble(),
    dplyr::tibble(
      "cohort_definition_id" = c(rep(1L, 4), rep(2L, 4)),
      "number_records" = c(rep(1L, 4), rep(4L, 4)),
      "number_subjects" = c(rep(1L, 4), rep(3L, 4)),
      "reason_id" = rep(1:4L, 2),
      "reason" = c(
        "Initial qualifying events",
        "Not missing record date",
        "Record in observation",
        "Distinct measurement records",
        "Initial qualifying events",
        "Not missing record date",
        "Record in observation",
        "Distinct measurement records"
      ),
      "excluded_records" = 0L,
      "excluded_subjects" = 0L
    )
  )
  expect_true(all(all(settings(cdm$cohort7)$cohort_name == c("c1", "c2"))))
  codes <- attr(cdm$cohort7, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id[codes$codelist_name == "c1"] == c(4326744)))
  expect_true(all(codes$concept_id[codes$codelist_name == "c2"] == c(4298393, 45770407)))

  # out of obs ----
  cdm$cohort8 <- measurementCohort(
    cdm = cdm,
    name = "cohort8",
    conceptSet = list("c1" = c(123456L))
  )
  expect_true(all(colnames(cdm$cohort8) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))
  expect_true(cdm$cohort8 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(settings(cdm$cohort8)$cohort_name == "c1")
  codes <- attr(cdm$cohort8, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(123456)))

  # empty cohort but non empty codelist ----
  cdm$cohort9 <- measurementCohort(
    cdm = cdm,
    name = "cohort9",
    conceptSet = list("c1" = c(1234567L))
  )
  expect_true(cdm$cohort9 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(settings(cdm$cohort9)$cohort_name == "c1")
  expect_identical(colnames(settings(cdm$cohort9)) |> sort(), c("cdm_version", "cohort_definition_id", "cohort_name", "vocabulary_version"))

  # empty cohort and empty codelist ----
  cdm$cohort10 <- measurementCohort(
    cdm = cdm,
    name = "cohort10",
    conceptSet = list()
  )
  expect_true(cdm$cohort10 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(cdm$cohort10 |> attrition() |> nrow() == 0)
  expect_true(cdm$cohort10 |> settings() |> nrow() == 0)
  expect_identical(colnames(settings(cdm$cohort10)) |> sort(), c("cdm_version", "cohort_definition_id", "cohort_name", "vocabulary_version"))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("expected errors", {
  testthat::skip_on_cran()
  cdm <- mockCohortConstructor(con = NULL, seed = 1)
  cdm$concept <- cdm$concept |>
    dplyr::union_all(
      dplyr::tibble(
        concept_id = c(4326744L, 4298393L, 45770407L, 8876L, 4124457L),
        concept_name = c("Blood pressure", "Systemic blood pressure",
                         "Baseline blood pressure", "millimeter mercury column",
                         "Normal range"),
        domain_id = "Measurement",
        vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "UCUM", "SNOMED"),
        standard_concept = "S",
        concept_class_id = c("Observable Entity", "Observable Entity",
                             "Observable Entity", "Unit", "Qualifier Value"),
        concept_code = NA,
        valid_start_date = NA,
        valid_end_date = NA,
        invalid_reason = NA
      )
    )
  cdm$measurement <- dplyr::tibble(
    measurement_id = 1:4L,
    person_id = c(1L, 1L, 2L, 3L),
    measurement_concept_id = c(4326744L, 4298393L, 4298393L, 45770407L),
    measurement_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19")),
    measurement_type_concept_id = NA_integer_,
    value_as_number = c(100L, 125L, NA_integer_, NA_integer_),
    value_as_concept_id = c(0L, 0L, 0L, 4124457L),
    unit_concept_id = c(8876L, 8876L, 0L, 0L)
  )
  cdm <- cdm |> copyCdm()

  # simple example
  expect_error(
    measurementCohort(
      cdm = cdm,
      name = "cohort",
      conceptSet = list(c(4326744L, 4298393L, 45770407L)),
      valueAsConcept = c(4124457L),
      valueAsNumber = list("8876" = c(70L, 120L))
    )
  )
  expect_error(
    measurementCohort(
      cdm = cdm,
      name = "cohort",
      conceptSet = list("name " = c(4326744L, 4298393L, 45770407L)),
      valueAsConcept = c(4124457),
      valueAsNumber = list("8876" = c(700, 120))
    )
  )

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
                                   table = data.frame(cohort_definition_id = 1:2L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-02"),
                                                      cohort_end_date = as.Date("2009-01-03"),
                                                      other_date = as.Date("2009-01-01")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- measurementCohort(cdm, name = "my_cohort", conceptSet = list(a = 40660437L))

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdm_disconnect(cdm = cdm)
})
