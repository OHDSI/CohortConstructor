test_that("exit at observation end", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 2,seed = 1)
  cdm <- cdm_local |> copyCdm()
  # simple example - test it works
  cdm$cohort1 <- cdm$cohort |> exitAtObservationEnd(name = "cohort1")
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2003-05-17", "2015-02-25")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2003-06-15", "2013-06-29", "2013-06-29", "2015-10-11")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 2, 3)))

  # test cohort id and name
  cdm$cohort <- cdm$cohort |> exitAtObservationEnd(cohortId = 1)
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17", "2015-02-25")))
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2001-11-27", "2002-01-29", "2002-06-12", "2003-06-15", "2005-01-15", "2013-06-29", "2015-10-11")))
  expect_true(all(cdm$cohort |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 1, 1, 1, 2, 3)))
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
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 2,seed = 1)
  cdm_local$death <- dplyr::tibble(
    person_id = 1:2,
    death_date = as.Date(c("2013-06-29", "2003-06-15")),
    death_type_concept_id = NA
  )
  cdm <- cdm_local |> copyCdm()

  # simple example - require death TRUE works
  cdm$cohort1 <- cdm$cohort |> exitAtDeath(requireDeath = TRUE, name = "cohort1")
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2003-05-17")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2003-06-15", "2013-06-29", "2013-06-29")))
  expect_true(all(cdm$cohort1 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 2)))
  expect_true(all(attrition(cdm$cohort1)$reason ==
                    c("Initial qualifying events", "No death recorded", "Exit at death", "Initial qualifying events", "No death recorded", "Exit at death")))

  # simple example - require death FALSE works
  cdm$cohort2 <- cdm$cohort |> exitAtDeath(requireDeath = FALSE, name = "cohort2")
  expect_true(all(cdm$cohort2 |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2003-05-17", "2015-02-25")))
  expect_true(all(cdm$cohort2 |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2003-06-15", "2013-06-29", "2013-06-29", "2015-04-30")))
  expect_true(all(cdm$cohort2 |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 2, 3)))
  expect_true(all(attrition(cdm$cohort2)$reason ==
                    c("Initial qualifying events", "Exit at death", "Initial qualifying events", "Exit at death")))

  # cohort ID and name
  cdm$cohort <- cdm$cohort |> exitAtDeath(cohortId = 1, requireDeath = TRUE)
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("1999-05-03", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2003-05-17")))
  expect_true(all(cdm$cohort |> dplyr::pull(cohort_end_date) |> sort() ==
                    c("2001-11-27", "2002-01-29", "2002-06-12", "2003-06-15", "2005-01-15", "2013-06-29")))
  expect_true(all(cdm$cohort |> dplyr::pull(subject_id) |> sort() ==  c(1, 1, 1, 1, 1, 2)))
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
