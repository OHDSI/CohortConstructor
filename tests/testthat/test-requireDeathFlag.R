test_that("requiring death", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2)
  cdm_local$death <- dplyr::tibble(
    person_id = c(1,3),
    death_date = as.Date(c("2013-06-29", "2015-04-14")),
    death_type_concept_id = NA
  )
  cdm <- cdm_local |> copyCdm()

  cdm$cohort3 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   window = c(0, Inf),
                                   name = "cohort3")
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") %in% c(1,3)))
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(subject_id %in% c(1,3)) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date")))

# censor
  cdm$cohort4 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   window = c(0, Inf),
                                   censorDate = "cohort_end_date",
                                   name = "cohort4")
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") == c(3, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date",
                      "Initial qualifying events", "Death between 0 & Inf days relative to cohort_start_date, censoring at cohort_end_date")))


  # index date
  cdm$cohort5 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   window = c(0, 365),
                                   indexDate = "cohort_end_date",
                                   name = "cohort5")
  expect_true(all(cdm$cohort5 |> dplyr::pull("subject_id") %in% 3))
  expect_true(all(
    cdm$cohort5 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("2015-03-25")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events", "Death between 0 & 365 days relative to cohort_end_date",
                      "Initial qualifying events", "Death between 0 & 365 days relative to cohort_end_date")))

  # cohort Id
  cdm$cohort6 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   cohortId = 1,
                                   window = c(0, 365),
                                   name = "cohort6")
  expect_true(all(cdm$cohort6 |> dplyr::pull("subject_id") |> sort() %in% c(1, 1, 1, 3, 3)))
  expect_true(all(
    cdm$cohort6 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("2000-06-23", "2001-07-16", "2001-12-04", "2015-03-05", "2015-03-25")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort6)$reason ==
                    c("Initial qualifying events",
                      "Death between 0 & 365 days relative to cohort_start_date",
                      "Initial qualifying events")))

  # name
  cdm$cohort1 <-  requireDeathFlag(cohort = cdm$cohort1, window = list(c(0, Inf)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events",
                      "Death between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events",
                      "Death between 0 & Inf days relative to cohort_start_date")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("not death", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 3)
  cdm_local$death <- dplyr::tibble(
    person_id = c(1,3),
    death_date = as.Date(c("2013-06-29", "2015-10-11")),
    death_type_concept_id = NA
  )
  cdm <- cdm_local |> copyCdm()

  cdm$cohort3 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   window = c(0, Inf),
                                   name = "cohort3",
                                   negate = TRUE)

  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") %in% c(2,4)))
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(subject_id %in% c(2,4)) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason ==
                    c("Initial qualifying events", "Alive between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events", "Alive between 0 & Inf days relative to cohort_start_date")))

  # censor Id
  cdm$cohort4 <-  requireDeathFlag(cohort = cdm$cohort1,
                                   cohortId = 1,
                                   window = c(0, Inf),
                                   name = "cohort4",
                                   negate = TRUE)
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 2, 4, 4, 4)))
  expect_true(all(
    cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1990-10-29", "1992-08-05", "1997-04-25", "2000-01-06", "2003-05-31", "2003-07-20")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events", "Alive between 0 & Inf days relative to cohort_start_date",
                      "Initial qualifying events")))

  PatientProfiles::mockDisconnect(cdm)
})
