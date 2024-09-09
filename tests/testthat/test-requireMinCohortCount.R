test_that("testing requireMinCohortCount", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 1) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = c("cohort1"),
      numberCohorts = 5
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
  expect_true(nrow(cdm$cohort1_b |>
                     dplyr::collect()) > 0)
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
  expect_true(cdm$cohort1_c |> dplyr::tally() |> dplyr::pull() == 4)


  # nobody to drop
  cdm$cohort1_c <- requireMinCohortCount(cdm$cohort1,
                                         minCohortCount = 0,
                                         name = "cohort1_c"
  )
  expect_identical(settings(cdm$cohort1_c), startSettings)
  expect_identical(
    attrition(cdm$cohort1_c) |>
      dplyr::filter(reason != "Fewer than minimum cohort count of 0"),
    startAttrition
  )

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
                                                    cohortId = "1",
                                                    name = "cohort2"
  ))
  expect_error(cdm$cohort2 <- requireMinCohortCount(cdm$cohort1,
                                                    cohortId = 2,
                                                    name = "cohort3"
  ))
  expect_error(cdm$cohort2 <- requireMinCohortCount(cdm$cohort1,
                                                    cohortId = 10,
                                                    name = "cohort2"
  ))

  PatientProfiles::mockDisconnect(cdm)
})
