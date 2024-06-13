test_that("requiring presence in another table", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2)
  cdm_local$table <- dplyr::tibble(
    person_id = c(1, 3, 4),
    date_start = as.Date(c("2002-01-01", "2015-10-01", "2000-01-01")),
    date_end = as.Date(c("2002-01-01", "2015-10-01", "2000-01-01"))
  )
  cdm <- cdm_local |> copyCdm()

  start_cols <- colnames(cdm$cohort1)
  cdm$cohort2 <-  requireTableIntersect(cohort = cdm$cohort1,
                                            tableName = "table",
                                            targetStartDate = "date_start",
                                            targetEndDate = "date_end",
                                            window = list(c(-Inf, Inf)),
                                            name = "cohort2")
  expect_equal(colnames(cdm$cohort2), colnames(cdm$cohort1))


  expect_equal(cdm$cohort2 |> dplyr::pull("subject_id") |> sort(),
               cdm$cohort1 |> dplyr::pull("subject_id") |> sort())
  expect_equal(omopgenerics::attrition(cdm$cohort2)$reason,
               c("Initial qualifying events",
                 "In table table between -Inf & Inf days relative to cohort_start_date between 1 and Inf",
                 "Initial qualifying events",
                 "In table table between -Inf & Inf days relative to cohort_start_date between 1 and Inf"))

  cdm$cohort3 <-  requireTableIntersect(cohort = cdm$cohort1,
                                            tableName = "table",
                                            targetStartDate = "date_start",
                                            targetEndDate = "date_end",
                                            window = c(0, Inf),
                                            name = "cohort3")
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") == c(1, 3, 4, 1, 1, 1, 3)))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_start_date") ==
                    c("2001-03-30", "2015-03-25", "1997-10-22", "2000-06-23", "2001-07-16", "2001-12-04", "2015-03-05")))
  expect_equal(omopgenerics::attrition(cdm$cohort3)$reason,
               c("Initial qualifying events",
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
  expect_true(cdm$cohort4 |> dplyr::pull("subject_id") == 1)
  expect_true(cdm$cohort4 |> dplyr::pull("cohort_start_date") == "2003-06-15")
  expect_equal(omopgenerics::attrition(cdm$cohort4)$reason,
               c("Initial qualifying events",
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
  expect_true(all(cdm$cohort5 |> dplyr::pull("subject_id") == c(1, 1, 1, 1, 3)))
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2000-06-23", "2001-07-16", "2001-12-04", "2003-06-15", "2015-03-05")))
  expect_equal(omopgenerics::attrition(cdm$cohort5)$reason,
               c("Initial qualifying events",
                 "In table table between -Inf & 0 days relative to cohort_start_date between 1 and Inf, censoring at cohort_end_date",
                 "Initial qualifying events"))

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
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2)
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

  expect_true(cdm$cohort2 |> dplyr::pull("subject_id") |> length() == 0)
  expect_equal(omopgenerics::attrition(cdm$cohort2)$reason,
               c("Initial qualifying events",
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
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") == 1))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_start_date") == "2003-06-15"))
  expect_equal(omopgenerics::attrition(cdm$cohort3)$reason,
               c("Initial qualifying events",
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
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") == c(1, 3, 4, 1, 1, 1, 3)))
  expect_true(all((cdm$cohort4 |> dplyr::pull("cohort_start_date") ==
                 c("2001-03-30", "2015-03-25", "1997-10-22", "2000-06-23", "2001-07-16", "2001-12-04", "2015-03-05"))))
  expect_equal(omopgenerics::attrition(cdm$cohort4)$reason,
               c("Initial qualifying events",
                 "Not in table table between -Inf & 0 days relative to cohort_start_date, censoring at cohort_end_date",
                 "Initial qualifying events",
                 "Not in table table between -Inf & 0 days relative to cohort_start_date, censoring at cohort_end_date"))

  # cohort Id and name
  cdm$cohort1 <-  requireTableIntersect(cohort = cdm$cohort1,
                                        intersections = 0,
                                            cohortId = 1,
                                            tableName = "table",
                                            targetStartDate = "date_start",
                                            targetEndDate = "date_end",
                                            window = c(0, Inf),
                                            censorDate = NULL)
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 1, 1, 3)))
  expect_true(all((cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
                     c("2000-06-23", "2001-07-16", "2001-12-04", "2003-06-15", "2015-03-05"))))
  expect_equal(omopgenerics::attrition(cdm$cohort1)$reason,
               c("Initial qualifying events",
                 "Not in table table between 0 & Inf days relative to cohort_start_date",
                 "Initial qualifying events"))

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
  expect_equal(sort(cdm$cohort1 |>
                      requireTableIntersect(intersections = c(0, 0),
                                            tableName = "drug_exposure",
                                              window = c(-Inf, Inf),
                                              name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(4,5,6,7,8,9,10))


  # only one intersection
  expect_equal(sort(cdm$cohort1 |>
                      requireTableIntersect(intersections = c(1, 1),
                                              tableName = "drug_exposure",
                                              window = c(-Inf, Inf),
                                              name = "cohort1_test") |>
                      dplyr::pull("subject_id")),
               c(1))

  expect_equal(sort(cdm$cohort1 |>
                      requireTableIntersect(intersections = c(1),
                                              tableName = "drug_exposure",
                                              window = c(-Inf, Inf),
                                              name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(1))

  # 2 intersections
  expect_equal(sort(cdm$cohort1 |>
                      requireTableIntersect(intersections = c(2, 2),
                                              tableName = "drug_exposure",
                                              window = c(-Inf, Inf),
                                              name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(2))

  expect_equal(sort(cdm$cohort1 |>
                      requireTableIntersect(intersections = c(2),
                                              tableName = "drug_exposure",
                                              window = c(-Inf, Inf),
                                              name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(2))


  # 2 or more intersections
  expect_equal(sort(cdm$cohort1 |>
                      requireTableIntersect(intersections = c(2, Inf),
                                              tableName = "drug_exposure",
                                              window = c(-Inf, Inf),
                                              name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(2, 3))

  # 2 or 3 intersections
  expect_equal(sort(cdm$cohort1 |>
                      requireTableIntersect(intersections = c(2, 3),
                                              tableName = "drug_exposure",
                                              window = c(-Inf, Inf),
                                              name = "cohort1_test") |>
                      dplyr::pull("subject_id")), c(2, 3))



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

  PatientProfiles::mockDisconnect(cdm)

})
