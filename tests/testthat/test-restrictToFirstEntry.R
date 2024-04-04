test_that("test restrict to first entry works", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1,1,1,1),
    subject_id = c(1,1,2,2),
    cohort_start_date = as.Date(c("2020-09-21","2021-09-20","2020-09-21","2021-09-20")),
    cohort_end_date = as.Date(c("2021-06-21","2021-12-20","2021-06-21","2021-12-20"))
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1,1,2,2),
    subject_id = c(1,1,1,1),
    cohort_start_date = as.Date(c("2020-09-21","2021-09-20","2020-09-21","2021-09-20")),
    cohort_end_date = as.Date(c("2021-06-21","2021-12-20","2021-06-21","2021-12-20"))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = 1:2,
    person_id = 1:2,
    observation_period_start_date = c(as.Date("2010-01-01"), as.Date("2010-01-01")),
    observation_period_end_date = c(as.Date("2022-01-01"), as.Date("2022-01-01")),
    period_type_concept_id = 32880
  )

  cdm <- DrugUtilisation::mockDrugUtilisation(cohort1 = cohort1,
                                              cohort2 = cohort2,
                                              observation_period = observation_period)

  expect_true(all(cdm$cohort1 |> CohortConstructor::restrictToFirstEntry() |>
    dplyr::pull(cohort_start_date) == c("2020-09-21", "2020-09-21")))

  expect_true(all(cdm$cohort1 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(subject_id) %in% c(1,2)))

  expect_true(all(cdm$cohort2 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(cohort_start_date) == c("2020-09-21", "2020-09-21")))

  expect_true(all(cdm$cohort2 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(subject_id) == c(1,1)))


})

test_that("restrictToFirstEntry, cohortIds & name arguments", {

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1,1,1,1),
    subject_id = c(1,1,2,2),
    cohort_start_date = as.Date(c("2020-09-21","2021-09-20","2020-09-21","2021-09-20")),
    cohort_end_date = as.Date(c("2021-06-21","2021-12-20","2021-06-21","2021-12-20"))
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1,1,2,2),
    subject_id = c(1,1,1,1),
    cohort_start_date = as.Date(c("2020-09-21","2021-09-20","2020-09-21","2021-09-20")),
    cohort_end_date = as.Date(c("2021-06-21","2021-12-20","2021-06-21","2021-12-20"))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = 1:2,
    person_id = 1:2,
    observation_period_start_date = c(as.Date("2010-01-01"), as.Date("2010-01-01")),
    observation_period_end_date = c(as.Date("2022-01-01"), as.Date("2022-01-01")),
    period_type_concept_id = 32880
  )

  cdm <- DrugUtilisation::mockDrugUtilisation(cohort1 = cohort1,
                                              cohort2 = cohort2,
                                              observation_period = observation_period)

  expect_true(all(cdm$cohort1 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(cohort_start_date) == c("2020-09-21", "2020-09-21")))

  expect_true(all(cdm$cohort1 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(subject_id) %in% c(1,2)))

  expect_true(all(cdm$cohort2 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(cohort_start_date) == c("2020-09-21", "2020-09-21")))

  expect_true(all(cdm$cohort2 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(subject_id) == c(1,1)))


})
