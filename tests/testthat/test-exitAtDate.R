test_that("exit at observation end", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 2)
  cdm <- cdm_local |> copyCdm()
  # simple example - test it works
  cdm$cohort1 <- cdm$cohort |> exitAtObservationEnd(name = "cohort1")
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1997-10-22", "2000-06-23", "2001-03-30", "2015-03-05", "2015-03-25")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2013-06-29", "2013-06-29", "2013-12-31", "2015-10-11", "2015-10-11")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 3, 3, 4)))

  # test cohort id and name
  cdm$cohort <- cdm$cohort |> exitAtObservationEnd(cohortId = 1)
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1997-10-22", "2000-06-23", "2001-03-30", "2001-07-16", "2001-12-04", "2015-03-05", "2015-03-25")))
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2001-07-15", "2001-12-03", "2006-09-27", "2013-06-29", "2013-12-31", "2015-07-06", "2015-10-11")))
  expect_true(all(cdm$cohort |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 1, 1, 3, 3, 4)))
  expect_true(all(attrition(cdm$cohort)$reason == c("Initial qualifying events", "Exit at observation period end date", "Initial qualifying events")))

  # additional columns warning
  expect_message(cdm$cohort <- cdm$cohort |> dplyr::mutate(extra_col = 1) |> exitAtObservationEnd())
  expect_true(all(colnames(cdm$cohort) == c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  # expected errors
  expect_error(cdm$cohort |> exitAtObservationEnd(name = 1))
  expect_error(cdm$cohort |> exitAtObservationEnd(cohortId = "HI"))
  expect_error(cdm$person |> exitAtObservationEnd())
  PatientProfiles::mockDisconnect(cdm)
})

test_that("exit at death date", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 2)
  cdm_local$death <- dplyr::tibble(
    person_id = 1:2,
    death_date = as.Date(c("2013-06-29", "2003-06-15")),
    death_type_concept_id = NA
  )
  cdm <- cdm_local |> copyCdm()

  # simple example - require death TRUE works
  cdm$cohort1 <- cdm$cohort |> exitAtDeath(requireDeath = TRUE, name = "cohort1")
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("2000-06-23", "2001-03-30")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2013-06-29", "2013-06-29")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1)))
  expect_true(all(attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events", "No death recorded", "Exit at death", "Initial qualifying events", "No death recorded", "Exit at death")))

  # simple example - require death FALSE works
  cdm$cohort2 <- cdm$cohort |> exitAtDeath(requireDeath = FALSE, name = "cohort2")
  expect_true(all(cdm$cohort2 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1997-10-22", "2000-06-23", "2001-03-30", "2015-03-05", "2015-03-25")))
  expect_true(all(cdm$cohort2 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("1999-05-28", "2013-06-29", "2013-06-29", "2015-04-14", "2015-07-06")))
  expect_true(all(cdm$cohort2 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 3, 3, 4)))
  expect_true(all(attrition(cdm$cohort2)$reason ==
                    c("Initial qualifying events", "Exit at death", "Initial qualifying events", "Exit at death")))

  # cohort ID and name
  cdm$cohort <- cdm$cohort |> exitAtDeath(cohortId = 1, requireDeath = TRUE)
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("2000-06-23", "2001-03-30", "2001-07-16", "2001-12-04", "2015-03-05")))
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2001-07-15", "2001-12-03", "2006-09-27", "2013-06-29", "2015-07-06")))
  expect_true(all(cdm$cohort |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 1, 1, 3)))
  expect_true(all(attrition(cdm$cohort)$reason ==
                    c("Initial qualifying events", "No death recorded", "Exit at death", "Initial qualifying events")))

  # additional columns warning
  expect_message(cdm$cohort <- cdm$cohort |> dplyr::mutate(extra_col = 1) |> exitAtDeath())
  expect_true(all(colnames(cdm$cohort) == c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))

  # expected errors
  expect_error(cdm$cohort |> exitAtDeath(name = 1))
  expect_error(cdm$cohort |> exitAtDeath(cohortId = "HI"))
  expect_error(cdm$person |> exitAtDeath())
  expect_error(cdm$person |> exitAtDeath(requireDeath = 1))

  PatientProfiles::mockDisconnect(cdm)
})
