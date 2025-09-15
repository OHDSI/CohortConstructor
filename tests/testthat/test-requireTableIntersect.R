test_that("requiring presence in another table", {
  testthat::skip_on_cran()
  obs <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4),
    person_id = c(1, 2, 3, 4),
    observation_period_start_date = as.Date(c(
      "2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09"
    )),
    observation_period_end_date = as.Date(c(
      "2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31"
    )),
    "period_type_concept_id" = NA_integer_
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4),
    gender_concept_id = c(8532, 8507, 8507, 8507),
    year_of_birth = c(1997, 1963, 1986, 1978),
    month_of_birth = c(8, 1, 3, 11),
    day_of_birth = c(22, 27, 10, 8),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    subject_id = c(1, 1, 2, 3, 1, 1, 1, 1),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15"
    ))
  )

  cdm_local <- omock::mockCdmFromTables(
    tables = list(
      "cohort1" = cohort_1
    ),
    seed = 1
  )

  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local, name = "observation_period", table = obs)

  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local, name = "person", table = person)

  cdm_local$table <- dplyr::tibble(
    person_id = c(1, 3, 2, 2),
    date_start = as.Date(c("2002-01-01", "2015-10-01", "2000-01-01", "1999-01-01")),
    date_end = as.Date(c("2002-01-01", "2015-10-01", "2000-01-01", "1999-01-01"))
  )
  cdm <- cdm_local |> copyCdm()

  start_cols <- colnames(cdm$cohort1)
  cdm$cohort2 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = list(c(-Inf, Inf)),
                                        name = "cohort2")
  expect_identical(colnames(cdm$cohort2), colnames(cdm$cohort1))


  expect_identical(cdm$cohort2 |> dplyr::pull("subject_id") |> sort(), cdm$cohort1 |> dplyr::pull("subject_id") |> sort())
  expect_identical(omopgenerics::attrition(cdm$cohort2)$reason, c("Initial qualifying events",
                                                                  "In table table between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                                                                  "Initial qualifying events",
                                                                  "In table table between -Inf & Inf days relative to cohort_start_date between 1 and Inf"))

  cdm$cohort3 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = c(0, Inf),
                                        name = "cohort3")
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") == c(2,3,1,1)))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_start_date") ==
                    c("1999-05-03", "2015-02-25", "2001-03-24", "2001-11-28")))
  expect_identical(omopgenerics::attrition(cdm$cohort3)$reason, c("Initial qualifying events",
                                                                  "In table table between 0 & Inf days relative to cohort_start_date between 1 and Inf",
                                                                  "Initial qualifying events",
                                                                  "In table table between 0 & Inf days relative to cohort_start_date between 1 and Inf"))

  # censor date
  cdm$cohort4 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = c(-Inf, 0),
                                        censorDate = "cohort_end_date",
                                        name = "cohort4")
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") == c(1,1,1,1)))
  expect_true(all(cdm$cohort4 |> dplyr::pull("cohort_start_date") == c("2003-05-17", "2004-03-11", "2002-01-30", "2002-06-13")))
  expect_identical(omopgenerics::attrition(cdm$cohort4)$reason, c("Initial qualifying events",
                                                                  "In table table between -Inf & 0 days relative to cohort_start_date between 1 and Inf, censoring at cohort_end_date",
                                                                  "Initial qualifying events",
                                                                  "In table table between -Inf & 0 days relative to cohort_start_date between 1 and Inf, censoring at cohort_end_date"))

  # cohort Id
  cdm$cohort5 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        cohortId = 1,
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = c(-Inf, 0),
                                        censorDate = "cohort_end_date",
                                        name = "cohort5")
  expect_true(all(cdm$cohort5 |> dplyr::pull("subject_id") == c(1, 1, 1, 1, 1,1)))
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17", "2004-03-11")))
  expect_identical(omopgenerics::attrition(cdm$cohort5)$reason, c("Initial qualifying events",
                                                                  "In table table between -Inf & 0 days relative to cohort_start_date between 1 and Inf, censoring at cohort_end_date",
                                                                  "Initial qualifying events"))

  # out of observation
  cdm$cohort6 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        cohortId = 1,
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = c(-Inf, -1),
                                        inObservation = FALSE,
                                        censorDate = "cohort_end_date",
                                        name = "cohort6")
  expect_equal(
    cdm$cohort6 |>
      dplyr::filter(subject_id == 2) |>
      dplyr::pull("cohort_start_date"),
    as.Date("1999-05-03")
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  # expected errors
  # currently just 1 table suported´
  expect_error(
    requireTableIntersect(cohort = cdm$cohort1,
                          tableName = c("table", "observation_period"),
                          window = c(-Inf, Inf))
  )
  expect_error(
    requireTableIntersect(cohort = cdm$cohort1,
                          tableName = cdm$table,
                          window = c(-Inf, Inf))
  )
  expect_error(
    requireTableIntersect(cohort = cdm$cohort1,
                          tableName = "not_a_table",
                          window = c(-Inf, Inf))
  )

  PatientProfiles::mockDisconnect(cdm)
})

test_that("requiring absence in another table", {
  testthat::skip_on_cran
  obs <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4),
    person_id = c(1, 2, 3, 4),
    observation_period_start_date = as.Date(c(
      "2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09"
    )),
    observation_period_end_date = as.Date(c(
      "2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31"
    )),
    "period_type_concept_id" = NA_integer_
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4),
    gender_concept_id = c(8532, 8507, 8507, 8507),
    year_of_birth = c(1997, 1963, 1986, 1978),
    month_of_birth = c(8, 1, 3, 11),
    day_of_birth = c(22, 27, 10, 8),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    subject_id = c(1, 1, 2, 3, 1, 1, 1, 1),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15"
    ))
  )

  cdm_local <- omock::mockCdmFromTables(
    tables = list(
      "cohort1" = cohort_1
    ),
    seed = 1
  )

  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local, name = "observation_period", table = obs)

  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local, name = "person", table = person)

  cdm_local$table <- dplyr::tibble(
    person_id = c(1, 3, 4),
    date_start = as.Date(c("2002-01-01", "2015-10-01", "2000-01-01")),
    date_end = as.Date(c("2002-01-01", "2015-10-01", "2000-01-01"))
  )
  cdm <- cdm_local |> copyCdm()

  cdm$cohort2 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        intersections = 0,
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = c(-Inf, Inf),
                                        name = "cohort2")

  expect_true(cdm$cohort2 |> dplyr::pull("subject_id") |> length() == 1)
  expect_identical(omopgenerics::attrition(cdm$cohort2)$reason, c("Initial qualifying events",
                                                                  "Not in table table between -Inf & Inf days relative to cohort_start_date",
                                                                  "Initial qualifying events",
                                                                  "Not in table table between -Inf & Inf days relative to cohort_start_date"))

  cdm$cohort3 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        intersections = 0,
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = c(0, Inf),
                                        name = "cohort3")
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") == c(1,1,2,1,1)))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-05-03", "2002-01-30", "2002-06-13", "2003-05-17", "2004-03-11")))
  expect_identical(omopgenerics::attrition(cdm$cohort3)$reason, c("Initial qualifying events",
                                                                  "Not in table table between 0 & Inf days relative to cohort_start_date",
                                                                  "Initial qualifying events",
                                                                  "Not in table table between 0 & Inf days relative to cohort_start_date"))

  # censor date
  cdm$cohort4 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        intersections = 0,
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = c(-Inf, 0),
                                        censorDate = "cohort_end_date",
                                        name = "cohort4")
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") == c(2,3,1,1)))
  expect_true(all((cdm$cohort4 |> dplyr::pull("cohort_start_date") ==
                     c("1999-05-03", "2015-02-25", "2001-03-24", "2001-11-28"))))
  expect_identical(omopgenerics::attrition(cdm$cohort4)$reason, c("Initial qualifying events",
                                                                  "Not in table table between -Inf & 0 days relative to cohort_start_date, censoring at cohort_end_date",
                                                                  "Initial qualifying events",
                                                                  "Not in table table between -Inf & 0 days relative to cohort_start_date, censoring at cohort_end_date"))

  # cohort Id and name
  cdm$cohort1 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        intersections = 0,
                                        cohortId = "cohort_1",
                                        tableName = "table",
                                        targetStartDate = "date_start",
                                        targetEndDate = "date_end",
                                        window = c(0, Inf),
                                        censorDate = NULL)
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 1, 1, 1, 1, 2)))
  expect_true(all((cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
                     c("1999-05-03", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13",
                       "2003-05-17", "2004-03-11"))))
  expect_identical(omopgenerics::attrition(cdm$cohort1)$reason, c("Initial qualifying events",
                                                                  "Not in table table between 0 & Inf days relative to cohort_start_date",
                                                                  "Initial qualifying events"))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("different intersection count requirements", {
  testthat::skip_on_cran()

  cohort1 <- dplyr::tibble(
    subject_id = 1:10,
    cohort_definition_id = 1L,
    cohort_start_date = as.Date('2020-01-01'),
    cohort_end_date = as.Date('2020-01-01'))

  cdm_local <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort1" = cohort1))

  cdm_local$observation_period  <- cdm_local$observation_period |>
    dplyr::mutate(observation_period_start_date = as.Date("2010-01-01"),
                  observation_period_end_date = as.Date("2020-01-01"))

  cdm_local$concept <- dplyr::tibble(
    "concept_id" = 1,
    "concept_name" = "my concept",
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:6,
    "person_id" = c(1,2,2,3,3,3),
    "drug_concept_id" = 1,
    "drug_exposure_start_date" = as.Date('2019-01-01'),
    "drug_exposure_end_date" = as.Date('2019-01-01'),
    "drug_type_concept_id" = 1
  )
  cdm <- cdm_local |> copyCdm()

  # no intersections - people not in cohort2
  expect_identical(sort(cdm$cohort1 |>
                          requireTableIntersect(intersections = c(0, 0),
                                                tableName = "drug_exposure",
                                                window = c(-Inf, Inf),
                                                name = "cohort1_test") |>
                          dplyr::pull("subject_id")), as.integer(c(4,5,6,7,8,9,10)))


  # only one intersection
  expect_identical(sort(cdm$cohort1 |>
                          requireTableIntersect(intersections = c(1, 1),
                                                tableName = "drug_exposure",
                                                window = c(-Inf, Inf),
                                                name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(1L))

  expect_identical(sort(cdm$cohort1 |>
                          requireTableIntersect(intersections = c(1),
                                                tableName = "drug_exposure",
                                                window = c(-Inf, Inf),
                                                name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(1L))

  # 2 intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireTableIntersect(intersections = c(2, 2),
                                                tableName = "drug_exposure",
                                                window = c(-Inf, Inf),
                                                name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L))

  expect_identical(sort(cdm$cohort1 |>
                          requireTableIntersect(intersections = c(2),
                                                tableName = "drug_exposure",
                                                window = c(-Inf, Inf),
                                                name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L))


  # 2 or more intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireTableIntersect(intersections = c(2, Inf),
                                                tableName = "drug_exposure",
                                                window = c(-Inf, Inf),
                                                name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L, 3L))

  # 2 or 3 intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireTableIntersect(intersections = c(2, 3),
                                                tableName = "drug_exposure",
                                                window = c(-Inf, Inf),
                                                name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L, 3L))



  # expected errors
  expect_error(requireTableIntersect(cohort = cdm$cohort1,
                                     intersections = c(-10, 10),
                                     tableName = "drug_exposure",
                                     window = c(-Inf, Inf)))
  expect_error(requireTableIntersect(cohort = cdm$cohort1,
                                     intersections = c(11, 10),
                                     tableName = "drug_exposure",
                                     window = c(-Inf, Inf)))
  expect_error(requireTableIntersect(cohort = cdm$cohort1,
                                     intersections = c(Inf, Inf),
                                     tableName = "drug_exposure",
                                     window = c(-Inf, Inf)))
  expect_error(requireTableIntersect(cohort = cdm$cohort1,
                                     intersections = c(1, 2, 3),
                                     tableName = "drug_exposure",
                                     window = c(-Inf, Inf)))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("test indexes - postgres, and atFirst", {
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
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::contains("my_cohort"))

  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_cohort",
                                   table = data.frame(cohort_definition_id = 1L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-02"),
                                                      cohort_end_date = as.Date("2009-01-03"),
                                                      other_date = as.Date("2009-01-01")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- requireTableIntersect(cdm$my_cohort, tableName = "visit_occurrence", window = list(c(-Inf, 0)))
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  # atFirst
  cohort <- dplyr::tibble(
    cohort_definition_id = c(rep(1L, 4), rep(2L, 4)),
    subject_id = c(1L, 1L, 2L, 3L, rep(1L, 4)),
    cohort_start_date = as.Date(c(
      "2008-01-01", "2009-03-11", "2010-05-03", "2010-02-25",
      "2008-03-24", "2008-11-28", "2010-01-30", "2009-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2009-03-10", "2009-07-19", "2010-06-15", "2010-04-30",
      "2008-11-27", "2008-01-29", "2010-06-12", "2010-01-15"
    ))
  )
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_cohort",
                                   table = cohort)
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort, .softValidation = TRUE)
  cdm$my_cohort_1 <- requireTableIntersect(cohort = cdm$my_cohort,
                                           tableName = "measurement",
                                           window = list(c(-Inf, 0)),
                                           atFirst = TRUE,
                                           name = "my_cohort_1")
  expect_equal(
    collectCohort(cdm$my_cohort_1, 2),
    dplyr::tibble(
      subject_id = 1L,
      cohort_start_date = as.Date(NULL),
      cohort_end_date = as.Date(NULL)
    )
  )
  expect_equal(
    attrition(cdm$my_cohort_1)$reason,
    c('Initial qualifying events',
      'In table measurement between -Inf & 0 days relative to cohort_start_date between 1 and Inf. Requirement applied to the first entry',
      'Initial qualifying events',
      'In table measurement between -Inf & 0 days relative to cohort_start_date between 1 and Inf. Requirement applied to the first entry'
    ))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
