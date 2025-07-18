test_that("testing requireMinCohortCount", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = c("cohort1"),
      numberCohorts = 5,
      recordPerson = 2
    )
  cdm <- cdm_local |> copyCdm()

  startSettings <- settings(cdm$cohort1)
  startAttrition <- attrition(cdm$cohort1)

  # restrict all cohorts - all will be dropped
  cdm$cohort1_a <- requireMinCohortCount(cdm$cohort1,
                                         minCohortCount = 5,
                                         name = "cohort1_a"
  )
  expect_true(nrow(cdm$cohort1_a |>
                     dplyr::collect()) == 0)
  expect_true(nrow(cohortCount(cdm$cohort1_a) |>
                     dplyr::collect()) == 5)
  expect_true(all(cohortCount(cdm$cohort1_a) |>
                    dplyr::pull("number_records") == 0))
  expect_true(all(cohortCount(cdm$cohort1_a) |>
                    dplyr::pull("number_subjects") == 0))
  expect_true(nrow(settings(cdm$cohort1_a)) == 5)

  # restrict specific cohort
  cdm$cohort1_b <- requireMinCohortCount(
    cdm$cohort1,
    minCohortCount = 5,
    cohortId = c(1, 2),
    name = "cohort1_b"
  )
  expect_gt(nrow(cdm$cohort1_b |>
                   dplyr::collect()), 0)
  expect_true(all(cohortCount(cdm$cohort1_b) |>
                    dplyr::filter(cohort_definition_id == 1) |>
                    dplyr::pull("number_records") == 0))
  expect_true(all(cohortCount(cdm$cohort1_b) |>
                    dplyr::filter(cohort_definition_id == 1) |>
                    dplyr::pull("number_subjects") == 0))

  expect_true(all(cohortCount(cdm$cohort1_b) |>
                    dplyr::filter(cohort_definition_id == 3) |>
                    dplyr::pull("number_records") > 0))
  expect_true(all(cohortCount(cdm$cohort1_b) |>
                    dplyr::filter(cohort_definition_id == 3) |>
                    dplyr::pull("number_subjects") > 0))

  # restrict specific cohort
  expect_warning(
    cdm$cohort1_c <- requireMinCohortCount(
      cdm$cohort1,
      minCohortCount = 5,
      cohortId = c("cohort_1", "cohort_6"),
      name = "cohort1_c"
    ))
  expect_true(cdm$cohort1_c |> dplyr::tally() |> dplyr::pull() == 8)


  # nobody to drop
  expect_warning(
    cdm$cohort1_c <- requireMinCohortCount(cdm$cohort1,
                                           minCohortCount = 0,
                                           name = "cohort1_c"
    ))
  expect_identical(settings(cdm$cohort1_c), startSettings)
  expect_identical(
    attrition(cdm$cohort1_c) |>
      dplyr::filter(reason != "Fewer than minimum cohort count of 0"),
    startAttrition
  )

  # drop attributes
  cdm$cohort1_d <- requireMinCohortCount(cdm$cohort1,
                                         minCohortCount = 5,
                                         updateSettings = TRUE,
                                         name = "cohort1_d"
  )
  expect_true(nrow(cdm$cohort1_d |>
                     dplyr::collect()) == 0)
  expect_true(all(cohortCount(cdm$cohort1_d) |>
                    nrow() == 0))
  expect_true(all(attrition(cdm$cohort1_d) |>
                    nrow() == 0))

  # settings of original cohort unchanged
  expect_identical(settings(cdm$cohort1), startSettings)
  expect_identical(attrition(cdm$cohort1), startAttrition)

  # expect errors
  expect_error(cdm$cohort2 <- requireMinCohortCount("cohort1",
                                                    cohortId = 1,
                                                    name = "cohort2"
  ))
  expect_error(cdm$cohort2 <- requireMinCohortCount("cohort1",
                                                    minCohortCount = -10,
                                                    name = "cohort2"
  ))
  expect_error(cdm$cohort2 <- requireMinCohortCount(cdm$cohort1,
                                                    minCohortCount = "1",
                                                    name = "cohort2"
  ))
  expect_error(cdm$cohort2 <- requireMinCohortCount(cdm$cohort1,
                                                    minCohortCount = Inf,
                                                    name = "cohort2"
  ))
  expect_error(cdm$cohort2 <- requireMinCohortCount(cdm$cohort1,
                                                    minCohortCount = c(1, 2),
                                                    name = "cohort2"
  ))
  expect_error(cdm$cohort2 <- requireMinCohortCount(cdm$cohort1,
                                                    name = "cohort2"
  ))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
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
  cdm$my_cohort <- requireMinCohortCount(cdm$my_cohort, minCohortCount = 1)
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
