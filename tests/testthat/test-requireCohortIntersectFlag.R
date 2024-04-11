test_that("requiring presence in another cohort", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2) |>
    omock::mockCohort(tableName = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort3 <-  requireCohortIntersectFlag(x = cdm$cohort1,
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

  cdm$cohort4 <-  requireCohortIntersectFlag(x = cdm$cohort1,
                                             targetCohortTable = "cohort2",
                                             targetCohortId = 2,
                                             window = c(-Inf, Inf),
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
  cdm$cohort1 <-  requireCohortIntersectFlag(x = cdm$cohort1,
                                             targetCohortTable = "cohort2",
                                             targetCohortId = 2,
                                             window = c(-Inf, Inf))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "In cohort cohort_2 between -Inf & Inf days relative to cohort_start_date")))

  # censor date
  cdm$cohort5 <- requireCohortIntersectFlag(x = cdm$cohort2,
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

  # expected errors
  # only support one target id at the moment
  expect_error(requireCohortIntersectFlag(x = cdm$cohort1,
                                          targetCohortTable = "cohort2",
                                          targetCohortId = c(1,2),
                                          window = c(-Inf, Inf)))

  expect_error(requireCohortIntersectFlag(x = cdm$cohort1,
                                          targetCohortTable = "cohort22", # does not exist
                                          targetCohortId = 1,
                                          window = c(-Inf, Inf)))
  expect_error(requireCohortIntersectFlag(x = cdm$cohort1,
                                          targetCohortTable = "cohort2",
                                          targetCohortId = 10, # does not exist
                                          window = c(-Inf, Inf)))

  CDMConnector::cdm_disconnect(cdm)

})

test_that("requiring absence in another cohort", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2) |>
    omock::mockCohort(tableName = c("cohort2"), numberCohorts = 2, seed = 2)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort3_inclusion <-  requireCohortIntersectFlag(x = cdm$cohort1,
                                                       targetCohortTable = "cohort2",
                                                       targetCohortId = 1,
                                                       window = c(-Inf, Inf)) |>
    dplyr::compute(name = "cohort3_inclusion", temporary = FALSE)
  cdm$cohort3_exclusion <-  requireCohortIntersectFlag(x = cdm$cohort1,
                                                       targetCohortTable = "cohort2",
                                                       targetCohortId = 1,
                                                       window = c(-Inf, Inf),
                                                       negate = TRUE) |>
    dplyr::compute(name = "cohort3_exclusion", temporary = FALSE)

  in_both <- intersect(cdm$cohort3_inclusion %>%
                         dplyr::pull("subject_id") %>%
                         unique(),
                       cdm$cohort3_exclusion %>%
                         dplyr::pull("subject_id") %>%
                         unique())
  expect_true(length(in_both) == 0)

  CDMConnector::cdm_disconnect(cdm)
})
