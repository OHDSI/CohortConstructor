test_that("require flag in concept", {#need
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    subject_id = c(1L, 1L, 2L, 3L, 1L, 1L, 1L, 1L),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15"
    ))
  )

  person <- dplyr::tibble(
    person_id = 1:4,
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L),
    year_of_birth = c(1997L, 1963L, 1986L, 1978L),
    month_of_birth = c(8L, 1L, 3L, 11L),
    day_of_birth = c(22L, 27L, 10L, 8L),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date(c("2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09")),
    observation_period_end_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31")),
    period_type_concept_id = NA_integer_
  )

  cdm_local <- omock::mockCdmFromTables(
    tables = list(
      "cohort1" = cohort_1
    ),
    seed = 1
  )

  cdm_local <- omopgenerics::insertTable(cdm = cdm_local, name = "observation_period", table = obs)
  cdm_local <- omopgenerics::insertTable(cdm = cdm_local, name = "person", table = person)

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
    "drug_exposure_id" = 1:11,
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )
  cdm <- cdm_local |> copyCdm()

  start_cols <- colnames(cdm$cohort1)
  cdm$cohort3 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                          conceptSet = list(a = 1L),
                                          window = c(-Inf, Inf),
                                          name = "cohort3")
  expect_identical(colnames(cdm$cohort3), colnames(cdm$cohort1))
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") == 1L))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17", "2004-03-11")))

  cdm$in_obs <- requireConceptIntersect(cohort = cdm$cohort1,
                                        conceptSet = list(a = 1L),
                                        intersections = 2,
                                        window = c(0, Inf),
                                        inObservation = FALSE,
                                        name = "in_obs")
  expect_identical(
    collectCohort(cdm$in_obs, 1),
    dplyr::tibble(subject_id = 2L, cohort_start_date = as.Date("1999-05-03"), cohort_end_date = as.Date("2001-06-15"))
  )

  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf")))
  # cohort Id
  cdm$cohort4 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                          cohortId = 1,
                                          conceptSet = list(a = 1L),
                                          window = c(-Inf, Inf),
                                          name = "cohort4")
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") ==
                    c(rep(1, 6))))
  expect_true(all(cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17", "2004-03-11")))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                      "Initial qualifying events")))
  # censor date
  cdm$cohort5 <- requireConceptIntersect(cohort = cdm$cohort1,
                                         conceptSet = list(a = 1L),
                                         window = c(-Inf, Inf),
                                         censorDate = "cohort_end_date",
                                         name = "cohort5")
  expect_true(cdm$cohort5 |> dplyr::pull("subject_id") |> length() == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf, censoring at cohort_end_date",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf, censoring at cohort_end_date")))
 # name
  cdm$cohort1 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                          conceptSet = list(a = 1L),
                                          window = c(-Inf, Inf))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf")))

  # empty concept
  expect_message(
    cdm$cohort1_equal <-  requireConceptIntersect(cohort = cdm$cohort1,
                                            conceptSet = list(),
                                            window = list(c(-Inf, Inf)),
                                            name = "cohort1_equal")
  )
  expect_true(all(omopgenerics::attrition(cdm$cohort1_equal)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf")))
  expect_equal(collectCohort(cdm$cohort1_equal,1), collectCohort(cdm$cohort1, 1))

  # expected errors
  # only support one concept at the moment
  expect_error(
    requireConceptIntersect(cohort = cdm$cohort1,
                            conceptSet = list(a = 1L, b = 2L),
                            window = c(-Inf, Inf))
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("requiring absence in another cohort", {
  testthat::skip_on_cran()
  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    subject_id = c(1L, 1L, 2L, 3L, 1L, 1L, 1L, 1L),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15"
    ))
  )

  person <- dplyr::tibble(
    person_id = 1:4,
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L),
    year_of_birth = c(1997L, 1963L, 1986L, 1978L),
    month_of_birth = c(8L, 1L, 3L, 11L),
    day_of_birth = c(22L, 27L, 10L, 8L),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date(c("2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09")),
    observation_period_end_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31")),
    period_type_concept_id = NA_integer_
  )

  cdm_local <- omock::mockCdmFromTables(
    tables = list(
      "cohort1" = cohort_1
    ),
    seed = 1
  )

  cdm_local <- omopgenerics::insertTable(cdm = cdm_local, name = "observation_period", table = obs)
  cdm_local <- omopgenerics::insertTable(cdm = cdm_local, name = "person", table = person)
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
    "drug_exposure_id" = 1:11,
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )
  cdm <- cdm_local |> copyCdm()

  cdm$cohort3_inclusion <-  requireConceptIntersect(cohort = cdm$cohort1,
                                                    conceptSet = list(a = 1L),
                                                    window = c(-Inf, Inf),
                                                    name = "cohort3_inclusion")
  cdm$cohort3_exclusion <-  requireConceptIntersect(cohort = cdm$cohort1,
                                                    conceptSet = list(a = 1L),
                                                    window = c(-Inf, Inf),
                                                    intersections = 0,
                                                    name = "cohort3_exclusion")
  in_both <- intersect(cdm$cohort3_inclusion |>
                         dplyr::pull("subject_id") |>
                         unique(),
                       cdm$cohort3_exclusion |>
                         dplyr::pull("subject_id") |>
                         unique())
  expect_true(length(in_both) == 0)
  in_both <- intersect(cdm$cohort3_inclusion |>
                         dplyr::pull("cohort_start_date") |>
                         sort(),
                       cdm$cohort3_exclusion |>
                         dplyr::pull("cohort_start_date") |>
                         sort())
  expect_true(length(in_both) == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort3_exclusion)$reason ==
                    c("Initial qualifying events",
                      "Not in concept a between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Not in concept a between -Inf & Inf days relative to cohort_start_date")))

  # cohort Id
  cdm$cohort3_exclusion_partial <-  requireConceptIntersect(
    cohort = cdm$cohort1,
    cohortId = "cohort_1",
    conceptSet = list(a = 1L),
    window = c(-Inf, Inf),
    intersections = 0,
    name = "cohort3_exclusion_partial"
  )
  expect_true(all(cdm$cohort3_exclusion_partial |> dplyr::pull("subject_id") |> sort() ==
                    c(1, 1, 1, 1, 2, 3)))
  expect_true(all(cdm$cohort3_exclusion_partial |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-05-03", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2015-02-25")))
  expect_true(all(omopgenerics::attrition(cdm$cohort3_exclusion_partial)$reason ==
                    c("Initial qualifying events",
                      "Not in concept a between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events"
                    )))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("different intersection count requirements", {#need
  testthat::skip_on_cran()

  cohort1 <- dplyr::tibble(
    subject_id = 1:10,
    cohort_definition_id = 1L,
    cohort_start_date = as.Date('2020-01-01'),
    cohort_end_date = as.Date('2020-01-01'))

  cdm_local <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort1" = cohort1), seed = 1)

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
                          requireConceptIntersect(intersections = c(0, 0),
                                                  conceptSet = list("a" = 1L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort1_test") |>
                          dplyr::pull("subject_id")), as.integer(c(4,5,6,7,8,9,10)))

  # only one intersection
  expect_identical(sort(cdm$cohort1 |>
                          requireConceptIntersect(intersections = c(1, 1),
                                                  conceptSet = list("a" = 1L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(1L))

  expect_identical(sort(cdm$cohort1 |>
                          requireConceptIntersect(intersections = c(1),
                                                  conceptSet = list("a" = 1L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(1L))

  # 2 intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireConceptIntersect(intersections = c(2, 2),
                                                  conceptSet = list("a" = 1L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L))

  expect_identical(sort(cdm$cohort1 |>
                          requireConceptIntersect(intersections = c(2),
                                                  conceptSet = list("a" = 1L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L))

  # 2 or more intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireConceptIntersect(intersections = c(2, Inf),
                                                  conceptSet = list("a" = 1L),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L, 3L))

  # 2 or 3 intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireConceptIntersect(intersections = c(2L, 3L),
                                                  conceptSet = list("a" = 1),
                                                  window = c(-Inf, Inf),
                                                  name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L, 3L))

  # expected errors
  expect_error(requireConceptIntersect(cohort = cdm$cohort1,
                                       intersections = c(-10, 10),
                                       conceptSet = list("a" = 1),
                                       window = c(-Inf, Inf)))
  expect_error(requireConceptIntersect(cohort = cdm$cohort1,
                                       intersections = c(11, 10),
                                       conceptSet = list("a" = 1),
                                       window = c(-Inf, Inf)))
  expect_error(requireConceptIntersect(cohort = cdm$cohort1,
                                       intersections = c(Inf, Inf),
                                       conceptSet = list("a" = 1),
                                       window = c(-Inf, Inf)))
  expect_error(requireConceptIntersect(cohort = cdm$cohort1,
                                       intersections = c(1, 2, 3),
                                       conceptSet = list("a" = 1),
                                       window = c(-Inf, Inf)))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("test indexes - postgres, atFirst", {
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
    writePrefix = "cc_test_",
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
  expect_no_error(cdm$my_cohort |> head(1))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  expect_no_error(omopgenerics::settings(cdm$my_cohort))
  cdm$my_cohort <- requireConceptIntersect(cdm$my_cohort,
                                           conceptSet = list(a = 0),
                                           window = list(c(0, Inf)))
  expect_no_error(cdm$my_cohort |> head(1))
  expect_no_error(omopgenerics::settings(cdm$my_cohort))
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_test_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_test_my_cohort_subject_id_cohort_start_date_idx ON public.cc_test_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  # atFirst
  cohort <- dplyr::tibble(
    cohort_definition_id = c(rep(1L, 4), rep(2L, 4)),
    subject_id = c(1L, 1L, 2L, 3L, rep(1L, 4)),
    cohort_start_date = as.Date(c(
      "2008-05-17", "2009-03-11", "2010-05-03", "2010-02-25",
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
  cdm$my_cohort_1 <- requireConceptIntersect(cohort = cdm$my_cohort,
                                            conceptSet = list(a = 22340),
                                            window = list(c(0, 365)),
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
      'Concept a between 0 & 365 days relative to cohort_start_date between 1 and Inf. Requirement applied to the first entry',
      'Initial qualifying events',
      'Concept a between 0 & 365 days relative to cohort_start_date between 1 and Inf. Requirement applied to the first entry'
    ))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("codelists", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 1)  |>
    omock::mockConditionOccurrence()
  cdm <- cdm_local |> copyCdm()

  cdm$cohort2 <- conceptCohort(cdm, list("a" = 194152L, "b" = 4151660L), name = "cohort2")
  # Only inclusion codes
  cdm$cohort3 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                          conceptSet = list("a" = 194152L),
                                          intersections = 0,
                                          window = c(-Inf, 0),
                                          name = "cohort3")
  expect_identical(
    attr(cdm$cohort3, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = 1:2,
      codelist_name = "a",
      concept_id = 194152L,
      codelist_type = "inclusion criteria"
    )
  )

  # no inlcusion codes
  cdm$cohort4 <-  requireConceptIntersect(cohort = cdm$cohort2,
                                          conceptSet = list("a" = 194152L),
                                          window = c(-Inf, Inf),
                                          name = "cohort4")
  expect_identical(
    attr(cdm$cohort4, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1L, 1L, 2L, 2L),
      codelist_name = c("a", "a", "b", "a"),
      concept_id = c(194152L, 194152L, 4151660L, 194152L),
      codelist_type = c("index event", "inclusion criteria", "index event", "inclusion criteria")
    )
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  CDMConnector::cdmDisconnect(cdm = cdm)
})
