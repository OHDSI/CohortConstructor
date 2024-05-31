test_that("requiring presence in another cohort", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2) |>
    omock::mockCohort(name = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- cdm_local |> copyCdm()

  cdm$cohort3 <-  requireCohortIntersectFlag(cohort = cdm$cohort1,
                                             targetCohortTable = "cohort2",
                                             targetCohortId = 1,
                                             window = c(-Inf, Inf),
                                             name = "cohort3")

  expect_true(all(cdm$cohort3  %>%
                    dplyr::distinct(subject_id) %>%
                    dplyr::pull() %in%
                    intersect(cdm$cohort1 %>%
                                dplyr::distinct(subject_id) %>%
                                dplyr::pull(),
                              cdm$cohort2 %>%
                                dplyr::filter(cohort_definition_id == 1) %>%
                                dplyr::distinct(subject_id) %>%
                                dplyr::pull())))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_1 between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "In cohort cohort_1 between -Inf & Inf days relative to cohort_start_date")))

  cdm$cohort4 <-  requireCohortIntersectFlag(cohort = cdm$cohort1,
                                             targetCohortTable = "cohort2",
                                             targetCohortId = 2,
                                             window = list(c(-Inf, Inf)),
                                             name = "cohort4")
  expect_true(all(cdm$cohort4 %>%
                    dplyr::distinct(subject_id) %>%
                    dplyr::pull() %in%
                    intersect(cdm$cohort1 %>%
                                dplyr::distinct(subject_id) %>%
                                dplyr::pull(),
                              cdm$cohort2 %>%
                                dplyr::filter(cohort_definition_id == 2) %>%
                                dplyr::distinct(subject_id) %>%
                                dplyr::pull())))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date")))

  # name
  cdm$cohort1 <-  requireCohortIntersectFlag(cohort = cdm$cohort1,
                                             targetCohortTable = "cohort2",
                                             targetCohortId = 2,
                                             window = c(-Inf, Inf))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date")))

  # censor date
  cdm$cohort5 <- requireCohortIntersectFlag(cohort = cdm$cohort2,
                                            targetCohortTable = "cohort1",
                                            targetCohortId = 2,
                                            window = c(0, Inf),
                                            censorDate = "cohort_end_date",
                                            name = "cohort5")
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_start_date") == c("2015-04-14", "2015-02-23")))
  expect_true(all(cdm$cohort5 |> dplyr::pull("subject_id") == c("3", "3")))
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_definition_id") == c("1", "2")))
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date",
                      "Initial qualifying events",
                      "In cohort cohort_2 between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date")))

  # cohort Id
  cdm$cohort6 <- requireCohortIntersectFlag(cohort = cdm$cohort2,
                                            cohortId = 2,
                                            targetCohortTable = "cohort1",
                                            targetCohortId = 1,
                                            window = c(0, Inf),
                                            censorDate = "cohort_end_date",
                                            name = "cohort6")
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1993-01-06", "1999-06-23", "2000-03-06", "2003-07-21",
                      "2015-02-23", "2015-04-14")))
  expect_true(all(cdm$cohort6 |> dplyr::pull("subject_id") |> sort() == c("1", "2", "2", "3", "3", "4")))
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_definition_id") |> sort() == c(rep("1", 4), rep("2", 2))))
  expect_true(all(omopgenerics::attrition(cdm$cohort6)$reason ==
                    c("Initial qualifying events",
                      "Initial qualifying events",
                      "In cohort cohort_1 between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date")))

  cdm$cohort7 <- requireCohortIntersectFlag(cohort = cdm$cohort2,
                                            cohortId = 2,
                                            targetCohortTable = "cohort1",
                                            targetCohortId = 1,
                                            window = c(0, Inf),
                                            censorDate = "cohort_end_date",
                                            name = "cohort7",
                                            negate = TRUE)
  expect_true(all(cdm$cohort7 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-06-23", "2000-03-06", "2003-07-21", "2015-02-02",
                      "2015-02-08", "2015-04-14")))
  expect_true(all(cdm$cohort7 |> dplyr::pull("subject_id") |> sort() == c("1", "2", "2", "3", "3", "3")))
  expect_true(all(cdm$cohort7 |> dplyr::pull("cohort_definition_id") |> sort() == c(rep("1", 4), rep("2", 2))))
  expect_true(all(omopgenerics::attrition(cdm$cohort7)$reason ==
                    c("Initial qualifying events",
                      "Initial qualifying events",
                      "Not in cohort cohort_1 between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date")))

  # expected errors
  # only support one target id at the moment
  expect_error(requireCohortIntersectFlag(cohort = cdm$cohort1,
                                          targetCohortTable = "cohort2",
                                          targetCohortId = c(1,2),
                                          window = c(-Inf, Inf)))
  expect_error(requireCohortIntersectFlag(cohort = cdm$cohort1,
                                          targetCohortTable = "cohort22", # does not exist
                                          targetCohortId = 1,
                                          window = c(-Inf, Inf)))
  expect_error(requireCohortIntersectFlag(cohort = cdm$cohort1,
                                          targetCohortTable = "cohort2",
                                          targetCohortId = 10, # does not exist
                                          window = c(-Inf, Inf)))
  expect_error(requireCohortIntersectFlag(cohort = cdm$cohort1,
                                          targetCohortTable = "cohort2",
                                          targetCohortId = NULL, # only one id supported
                                          window = c(-Inf, Inf)))
  expect_error(requireCohortIntersectFlag(cohort = cdm$cohort1,
                                          targetCohortTable = c("not_a_cohort", "lala"),
                                          targetCohortId = 1,
                                          window = c(-Inf, Inf)))
  PatientProfiles::mockDisconnect(cdm)

})

test_that("requiring absence in another cohort", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2) |>
    omock::mockCohort(name = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- cdm_local |> copyCdm()

  cdm$cohort3_inclusion <-  requireCohortIntersectFlag(cohort = cdm$cohort1,
                                                       targetCohortTable = "cohort2",
                                                       targetCohortId = 1,
                                                       window = c(-Inf, Inf),
                                                       name = "cohort3_inclusion")
  cdm$cohort3_exclusion <-  requireCohortIntersectFlag(cohort = cdm$cohort1,
                                                       targetCohortTable = "cohort2",
                                                       targetCohortId = 1,
                                                       window = c(-Inf, Inf),
                                                       negate = TRUE,
                                                       name = "cohort3_exclusion")
  in_both <- intersect(cdm$cohort3_inclusion %>%
                         dplyr::pull("subject_id") %>%
                         unique(),
                       cdm$cohort3_exclusion %>%
                         dplyr::pull("subject_id") %>%
                         unique())
  expect_true(length(in_both) == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort3_exclusion)$reason ==
                    c("Initial qualifying events",
                      "Not in cohort cohort_1 between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Not in cohort cohort_1 between -Inf & Inf days relative to cohort_start_date")))

  PatientProfiles::mockDisconnect(cdm)
})
