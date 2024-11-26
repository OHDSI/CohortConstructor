test_that("require flag in concept", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 1)
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
                                              conceptSet = list(a = 1),
                                              window = c(-Inf, Inf),
                                              name = "cohort3")
  expect_identical(colnames(cdm$cohort3), colnames(cdm$cohort1))
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") == 1L))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17", "2004-03-11")))

  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf")))
  # cohort Id
  cdm$cohort4 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                              cohortId = 1,
                                              conceptSet = list(a = 1),
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
                                             conceptSet = list(a = 1),
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
                                              conceptSet = list(a = 1),
                                              window = c(-Inf, Inf))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf")))

  # empty concept
  expect_message(
    cdm$cohort1 <-  requireConceptIntersect(cohort = cdm$cohort1,
                                                conceptSet = list(),
                                                window = list(c(-Inf, Inf)))
  )
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date between 1 and Inf")))

  # expected errors
  # only support one concept at the moment
 expect_error(
   requireConceptIntersect(cohort = cdm$cohort1,
                               conceptSet = list(a = 1, b = 2),
                               window = c(-Inf, Inf))
 )
 expect_error(
   requireConceptIntersect(cohort = cdm$cohort1,
                               conceptSet = NULL,
                               window = c(-Inf, Inf))
 )

  PatientProfiles::mockDisconnect(cdm)
})

test_that("requiring absence in another cohort", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 1)
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
                                                       conceptSet = list(a = 1),
                                                       window = c(-Inf, Inf),
                                                       name = "cohort3_inclusion")
  cdm$cohort3_exclusion <-  requireConceptIntersect(cohort = cdm$cohort1,
                                                       conceptSet = list(a = 1),
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
    conceptSet = list(a = 1),
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
                                              conceptSet = list("a" = 1),
                                             window = c(-Inf, Inf),
                                             name = "cohort1_test") |>
                      dplyr::pull("subject_id")), as.integer(c(4,5,6,7,8,9,10)))


  # only one intersection
  expect_identical(sort(cdm$cohort1 |>
                      requireConceptIntersect(intersections = c(1, 1),
                                             conceptSet = list("a" = 1),
                                             window = c(-Inf, Inf),
                                             name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(1L))

  expect_identical(sort(cdm$cohort1 |>
                      requireConceptIntersect(intersections = c(1),
                                              conceptSet = list("a" = 1),
                                             window = c(-Inf, Inf),
                                             name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(1L))

  # 2 intersections
  expect_identical(sort(cdm$cohort1 |>
                      requireConceptIntersect(intersections = c(2, 2),
                                              conceptSet = list("a" = 1),
                                             window = c(-Inf, Inf),
                                             name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(2L))

  expect_identical(sort(cdm$cohort1 |>
                      requireConceptIntersect(intersections = c(2),
                                              conceptSet = list("a" = 1),
                                             window = c(-Inf, Inf),
                                             name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(2L))


  # 2 or more intersections
  expect_identical(sort(cdm$cohort1 |>
                      requireConceptIntersect(intersections = c(2, Inf),
                                              conceptSet = list("a" = 1),
                                             window = c(-Inf, Inf),
                                             name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(2L, 3L))

  # 2 or 3 intersections
  expect_identical(sort(cdm$cohort1 |>
                      requireConceptIntersect(intersections = c(2, 3),
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
                                                      other_date = as.Date("2009-01-01")))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- requireConceptIntersect(cdm$my_cohort, conceptSet = list(a = 0), window = list(c(0, Inf)))

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdm_disconnect(cdm = cdm)
})
