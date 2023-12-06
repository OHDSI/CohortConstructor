test_that("test restrict to first entry works", {

  cohort1 <- dplyr::tibble(cohort_definition_id = c(1,1,1,1),
                           subject_id = c(1,1,2,2),
                           cohort_start_date = as.Date(c("2020-09-21","2020-09-20","2020-09-21","2020-09-20")),
                           cohort_end_date = as.Date(c("2021-09-21","2021-09-20","2021-09-21","2021-09-20")))

  cohort2 <- dplyr::tibble(cohort_definition_id = c(1,1,2,2),
                           subject_id = c(1,1,1,1),
                           cohort_start_date = as.Date(c("2020-09-21","2020-09-20","2020-09-21","2020-09-20")),
                           cohort_end_date = as.Date(c("2021-09-21","2021-09-20","2021-09-21","2021-09-20")))

  cdm <- DrugUtilisation::mockDrugUtilisation(cohort1 = cohort1, cohort2 = cohort2)

  expect_true(all(cdm$cohort1 |> CohortConstructor::restrictToFirstEntry() |>
    dplyr::pull(cohort_start_date) == c("2020-09-20", "2020-09-20")))

  expect_true(all(cdm$cohort1 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(subject_id) %in% c(1,2)))

  expect_true(all(cdm$cohort2 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(cohort_start_date) == c("2020-09-20", "2020-09-20")))

  expect_true(all(cdm$cohort2 |> CohortConstructor::restrictToFirstEntry() |>
                    dplyr::pull(subject_id) == c(1,1)))


})
