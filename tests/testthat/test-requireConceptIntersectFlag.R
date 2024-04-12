test_that("require flag in concept", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2)
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
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")
  cdm$cohort3 <-  requireConceptIntersectFlag(x = cdm$cohort1,
                                              conceptSet = list(a = 1),
                                              window = c(-Inf, Inf),
                                              name = "cohort3")
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") ==
                    c(1, 1, 3, 4, 1, 1, 1, 3)))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") ==
                    c("2001-03-30", "2003-06-15", "2015-03-25", "1997-10-22", "2000-06-23",
                      "2001-07-16", "2001-12-04", "2015-03-05")))

  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date")))

  # censor date
  cdm$cohort5 <- requireConceptIntersectFlag(x = cdm$cohort1,
                                             conceptSet = list(a = 1),
                                             window = c(-Inf, Inf),
                                             censorDate = "cohort_end_date",
                                             name = "cohort5")
  expect_true(cdm$cohort5 |> dplyr::pull("subject_id") |> length() == 0)
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date, censoring at cohort_end_date",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date, censoring at cohort_end_date")))

  # name
  cdm$cohort1 <-  requireConceptIntersectFlag(x = cdm$cohort1,
                                              conceptSet = list(a = 1),
                                              window = c(-Inf, Inf))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Concept a between -Inf & Inf days relative to cohort_start_date")))



  # expected errors
  # only support one concept at the moment
 expect_error(
   requireConceptIntersectFlag(x = cdm$cohort1,
                               conceptSet = list(a = 1, b = 2),
                               window = c(-Inf, Inf))
 )
 expect_error(
   requireConceptIntersectFlag(x = cdm$cohort1,
                               conceptSet = NULL,
                               window = c(-Inf, Inf))
 )

  CDMConnector::cdm_disconnect(cdm)
})

test_that("requiring absence in another cohort", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2)
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
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort3_inclusion <-  requireConceptIntersectFlag(x = cdm$cohort1,
                                                       conceptSet = list(a = 1),
                                                       window = c(-Inf, Inf),
                                                       name = "cohort3_inclusion")
  cdm$cohort3_exclusion <-  requireConceptIntersectFlag(x = cdm$cohort1,
                                                       conceptSet = list(a = 1),
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
                      "Not in concept a between -Inf & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Not in concept a between -Inf & Inf days relative to cohort_start_date")))

  CDMConnector::cdm_disconnect(cdm)
})
