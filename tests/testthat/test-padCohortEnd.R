test_that("simple example", {

  cdm <-  omock::mockCdmFromTables(tables = list(
    cohort =
      data.frame(
        cohort_definition_id = 1L,
        subject_id = c(1L, 2L),
        cohort_start_date = as.Date(c("2020-01-03","2020-01-03")),
        cohort_end_date = as.Date(c("2020-01-04", "2020-01-04"))
      )
  ))
  cdm <- cdm |> copyCdm()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2020-01-01", "2020-01-01")),
      observation_period_end_date = as.Date(c("2020-01-10", "2020-01-30")),
      period_type_concept_id = 1L
    )
  )

  cdm$cohort_1 <- padCohortEnd(cdm$cohort,
                                 days = 10,
                                 name = "cohort_1")

  expect_true(nrow(cdm$cohort_1 |>
    dplyr::collect()) == 2)
  # same cohort start
  expect_true(all(cdm$cohort_1 |>
                     dplyr::pull("cohort_start_date") ==
                   as.Date("2020-01-03")))
  # new cohort end
  expect_true(all(cdm$cohort_1 |>
                    dplyr::filter(subject_id == 1) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2020-01-10"))) # end of obs
  expect_true(all(cdm$cohort_1 |>
                    dplyr::filter(subject_id == 2) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2020-01-14"))) # cohort end plus 10 days

  # before cohort start - should be dropped if before cohort start
  cdm$cohort_2 <- padCohortEnd(cdm$cohort,
                               days = -2,
                               name = "cohort_2")
  expect_true(nrow(cdm$cohort_2 |>
                     dplyr::collect()) == 0)


})

test_that("overlapping entries", {

  cdm <-  omock::mockCdmFromTables(tables = list(
    cohort =
      data.frame(
        cohort_definition_id = c(1L, 1L, 2L, 2L),
        subject_id = c(1L, 1L, 1L, 2L),
        cohort_start_date = as.Date(c("2020-01-03",
                                      "2020-01-08",
                                      "2020-01-08",
                                      "2020-01-03")),
        cohort_end_date = as.Date(c("2020-01-04",
                                    "2020-01-10",
                                    "2020-01-10",
                                    "2020-01-04"))
      )
  ))
  cdm <- cdm |> copyCdm()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "observation_period",
    table = data.frame(
      observation_period_id = c(1L, 2L),
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date(c("2020-01-01", "2020-01-01")),
      observation_period_end_date = as.Date(c("2020-01-10", "2020-01-30")),
      period_type_concept_id = 1L
    )
  )

  # by adding 10 days, person one will have overlapping entries
  # which should be collapsed
  # there entry in the other cohort should be unchanged
  cdm$cohort_1 <- padCohortEnd(cdm$cohort,
                               days = 10,
                               name = "cohort_1")
  expect_true(nrow(cdm$cohort_1 |>
                     dplyr::collect()) == 3)

  # new cohort end
  expect_true(all(cdm$cohort_1 |>
                    dplyr::filter(subject_id == 1,
                                  cohort_definition_id == 1) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2020-01-10"))) # end of obs
  expect_true(all(cdm$cohort_1 |>
                    dplyr::filter(subject_id == 2) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2020-01-14"))) # cohort end plus 10 days

  # leave one cohort unchanged
  cdm$cohort_2 <- padCohortEnd(cdm$cohort,
                               days = 10,
                               name = "cohort_2",
                               cohortId = 2)
  expect_true(nrow(cdm$cohort_2 |>
                     dplyr::collect()) == 4)


  expect_identical(cdm$cohort_2 |>
    dplyr::filter(subject_id == 2) |>
    dplyr::filter(cohort_definition_id == 2) |>
    dplyr::pull("cohort_end_date"), as.Date("2020-01-14"))

  expect_identical(sort(cdm$cohort |>
    dplyr::filter(cohort_definition_id == 1) |>
    dplyr::pull("cohort_end_date")), sort(cdm$cohort_2 |>
        dplyr::filter(cohort_definition_id == 1)  |>
    dplyr::pull("cohort_end_date")))

})
