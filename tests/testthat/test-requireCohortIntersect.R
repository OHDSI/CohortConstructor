test_that("requiring presence in another cohort", {
  skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 1) |>
    omock::mockCohort(name = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- cdm_local |> copyCdm()

  start_cols <- colnames(cdm$cohort1)
  cdm$cohort3 <-  requireCohortIntersect(cohort = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 1,
                                         window = c(-Inf, Inf),
                                         name = "cohort3")
  expect_identical(colnames(cdm$cohort3), colnames(cdm$cohort1))

  expect_true(all(cdm$cohort3  |>
                    dplyr::distinct(subject_id) |>
                    dplyr::pull() %in%
                    intersect(cdm$cohort1 |>
                                dplyr::distinct(subject_id) |>
                                dplyr::pull(),
                              cdm$cohort2 |>
                                dplyr::filter(cohort_definition_id == 1) |>
                                dplyr::distinct(subject_id) |>
                                dplyr::pull())))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_1 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times",
                      "Initial qualifying events",
                      "In cohort cohort_1 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times")))

  cdm$cohort4 <-  requireCohortIntersect(cohort = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 2,
                                         window = list(c(-Inf, Inf)),
                                         name = "cohort4")
  expect_true(all(cdm$cohort4 |>
                    dplyr::distinct(subject_id) |>
                    dplyr::pull() %in%
                    intersect(cdm$cohort1 |>
                                dplyr::distinct(subject_id) |>
                                dplyr::pull(),
                              cdm$cohort2 |>
                                dplyr::filter(cohort_definition_id == 2) |>
                                dplyr::distinct(subject_id) |>
                                dplyr::pull())))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times",
                      "Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times")))

  # name
  cdm$cohort1 <-  requireCohortIntersect(cohort = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 2,
                                         window = c(-Inf, Inf))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times",
                      "Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date between 1 and Inf times")))

  # censor date
  cdm$cohort5 <- requireCohortIntersect(cohort = cdm$cohort2,
                                        targetCohortTable = "cohort1",
                                        targetCohortId = 2,
                                        window = c(0, Inf),
                                        censorDate = "cohort_end_date",
                                        name = "cohort5")
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_start_date") == c("2003-05-08", "2000-06-17", "2004-12-12")))
  expect_true(all(cdm$cohort5 |> dplyr::pull("subject_id") == c("1", "1", "1")))
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_definition_id") == c("1", "2", "2")))
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between 0 & Inf days relative to cohort_start_date between 1 and Inf times, censoring at cohort_end_date",
                      "Initial qualifying events",
                      "In cohort cohort_2 between 0 & Inf days relative to cohort_start_date between 1 and Inf times, censoring at cohort_end_date")))

  # cohort Id
  cdm$cohort6 <- requireCohortIntersect(cohort = cdm$cohort2,
                                        cohortId = "cohort_2",
                                        targetCohortTable = "cohort1",
                                        targetCohortId = 1,
                                        window = c(0, Inf),
                                        censorDate = "cohort_end_date",
                                        name = "cohort6")
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-07-11", "2000-01-11", "2000-05-28", "2000-06-17",
                      "2003-05-08", "2004-12-12", "2015-01-25", "2015-02-02")))
  expect_true(all(cdm$cohort6 |> dplyr::pull("subject_id") |> sort() == c("1","1","1", "2", "2", "2", "3", "3")))
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_definition_id") |> sort() == c(rep("1", 4), rep("2", 4))))
  expect_true(all(omopgenerics::attrition(cdm$cohort6)$reason ==
                    c("Initial qualifying events",
                      "Initial qualifying events",
                      "In cohort cohort_1 between 0 & Inf days relative to cohort_start_date between 1 and Inf times, censoring at cohort_end_date")))

  cdm$cohort7 <- requireCohortIntersect(cohort = cdm$cohort2,
                                        intersections = c(0,0),
                                        cohortId = 2,
                                        targetCohortTable = "cohort1",
                                        targetCohortId = 1,
                                        window = c(0, Inf),
                                        censorDate = "cohort_end_date",
                                        name = "cohort7")
  expect_true(all(cdm$cohort7 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2000-01-11", "2000-05-28", "2003-05-08", "2015-01-25")))
  expect_true(all(cdm$cohort7 |> dplyr::pull("subject_id") |> sort() == c("1", "2", "2", "3")))
  expect_true(all(cdm$cohort7 |> dplyr::pull("cohort_definition_id") |> sort() == c(rep("1", 4))))
  expect_true(all(omopgenerics::attrition(cdm$cohort7)$reason ==
                    c("Initial qualifying events",
                      "Initial qualifying events",
                      "Not in cohort cohort_1 between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date")))

  # expected errors
  # only support one target id
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort2",
                                      targetCohortId = c(1,2),
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort2",
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort22", # does not exist
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort2",
                                      targetCohortId = 10, # does not exist
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = "cohort2",
                                      targetCohortId = NULL, # only one id supported
                                      window = c(-Inf, Inf)))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      targetCohortTable = c("not_a_cohort", "lala"),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf)))


  PatientProfiles::mockDisconnect(cdm)
})

test_that("requiring absence in another cohort", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 1) |>
    omock::mockCohort(name = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- cdm_local |> copyCdm()

  cdm$cohort3_inclusion <-  requireCohortIntersect(cohort = cdm$cohort1,
                                                   targetCohortTable = "cohort2",
                                                   targetCohortId = 1,
                                                   window = c(-Inf, Inf),
                                                   name = "cohort3_inclusion")
  cdm$cohort3_exclusion <-  requireCohortIntersect(cohort = cdm$cohort1,
                                                   intersections = c(0, 0),
                                                   targetCohortTable = "cohort2",
                                                   targetCohortId = 1,
                                                   window = c(-Inf, Inf),
                                                   name = "cohort3_exclusion")
  in_both <- intersect(cdm$cohort3_inclusion |>
                         dplyr::pull("subject_id") |>
                         unique(),
                       cdm$cohort3_exclusion |>
                         dplyr::pull("subject_id") |>
                         unique())
  expect_true(length(in_both) == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort3_exclusion)$reason ==
                    c("Initial qualifying events",
                      "Not in cohort cohort_1 between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Not in cohort cohort_1 between -Inf & Inf days relative to cohort_start_date")))

  # empty target cohort
  expect_message(
    cdm$cohort1_equal <-  requireCohortIntersect(cohort = cdm$cohort1,
                                                 targetCohortTable = "cohort3_exclusion",
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 name = "cohort1_equal")
  )
  expect_equal(collectCohort(cdm$cohort1_equal, 1), collectCohort(cdm$cohort1, 1))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("different intersection count requirements", {
  testthat::skip_on_cran()

  cohort1 <- dplyr::tibble(
    subject_id = 1:10,
    cohort_definition_id = 1L,
    cohort_start_date = as.Date('2020-01-01'),
    cohort_end_date = as.Date('2020-01-01'))
  cohort2 <- dplyr::tibble(
    subject_id = c(1,2,2,3,3,3),
    cohort_definition_id = 1L,
    cohort_start_date = c(as.Date('2019-01-01'),
                          as.Date('2019-01-02'),
                          as.Date('2019-01-03'),
                          as.Date('2019-01-04'),
                          as.Date('2019-01-05'),
                          as.Date('2019-01-06')),
    cohort_end_date =  c(as.Date('2019-01-01'),
                         as.Date('2019-01-02'),
                         as.Date('2019-01-03'),
                         as.Date('2019-01-04'),
                         as.Date('2019-01-05'),
                         as.Date('2019-01-06'))
  )
  cdm_local <- omock::mockCdmReference() |> omock::mockCdmFromTables(tables = list("cohort1" = cohort1,
                                                                                   "cohort2" = cohort2), seed = 1)
  cdm <- cdm_local |> copyCdm()

  # no intersections - people not in cohort2
  expect_identical(sort(cdm$cohort1 |>
                          requireCohortIntersect(intersections = c(0, 0),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort2",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), as.integer(c(4,5,6,7,8,9,10)))


  # only one intersection
  expect_identical(sort(cdm$cohort1 |>
                          requireCohortIntersect(intersections = c(1, 1),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort2",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(1L))

  expect_identical(sort(cdm$cohort1 |>
                          requireCohortIntersect(intersections = c(1),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort2",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(1L))

  # 2 intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireCohortIntersect(intersections = c(2, 2),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort2",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L))

  expect_identical(sort(cdm$cohort1 |>
                          requireCohortIntersect(intersections = c(2),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort2",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L))


  # 2 or more intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireCohortIntersect(intersections = c(2, Inf),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort2",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L, 3L))

  # 2 or 3 intersections
  expect_identical(sort(cdm$cohort1 |>
                          requireCohortIntersect(intersections = c(2, 3),
                                                 targetCohortId = 1,
                                                 window = c(-Inf, Inf),
                                                 targetCohortTable = "cohort2",
                                                 name = "cohort1_test") |>
                          dplyr::pull("subject_id")), c(2L, 3L))



  # expected errors
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      intersections = c(-10, 10),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf),
                                      targetCohortTable = "cohort2"))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      intersections = c(11, 10),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf),
                                      targetCohortTable = "cohort2"))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      intersections = c(Inf, Inf),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf),
                                      targetCohortTable = "cohort2"))
  expect_error(requireCohortIntersect(cohort = cdm$cohort1,
                                      intersections = c(1, 2, 3),
                                      targetCohortId = 1,
                                      window = c(-Inf, Inf),
                                      targetCohortTable = "cohort2"))

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
  cdm$my_cohort <- requireCohortIntersect(cdm$my_cohort, targetCohortTable = "my_cohort", window = list(c(-Inf, Inf)))

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
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
  cdm$cohort3 <-  requireCohortIntersect(cohort = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 1,
                                         window = c(-Inf, Inf),
                                         name = "cohort3")
  expect_identical(
    attr(cdm$cohort3, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = 1:2,
      codelist_name = "a",
      concept_id = 194152L,
      type = "inclusion criteria"
    )
  )

  # no inlcusion codes
  cdm$cohort4 <-  requireCohortIntersect(cohort = cdm$cohort2,
                                         targetCohortTable = "cohort1",
                                         targetCohortId = 1,
                                         window = c(-Inf, Inf),
                                         name = "cohort4")
  expect_identical(
    attr(cdm$cohort4, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = 1:2L,
      codelist_name = c("a", "b"),
      concept_id = c(194152L, 4151660L),
      type = "index event"
    )
  )
  CDMConnector::cdmDisconnect(cdm = cdm)
})
