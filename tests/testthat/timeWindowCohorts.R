test_that("timeWindowCohorts works", {
  skip_on_cran()
  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      "person" = dplyr::tibble(
        "person_id" = 1:4L,
        "gender_concept_id" = 0L,
        "year_of_birth" = 0L,
        "month_of_birth" = 0L,
        "day_of_birth" = 0L,
        "race_concept_id" = 0L,
        "ethnicity_concept_id" = 0L
      ),
      "observation_period" = dplyr::tibble(
        "observation_period_id" = 1:4L,
        "person_id" = 1:4L,
        "observation_period_start_date" = as.Date("2000-01-01"),
        "observation_period_end_date" = as.Date("2024-01-01"),
        "period_type_concept_id" = 0L
      )
    ),
    cdmName = "mock db",
    cohortTables = list(
      "cohort1" = dplyr::tibble(
        "cohort_definition_id" = as.integer(c(1, 1, 1, 2, 2, 3)),
        "subject_id" = as.integer(c(1, 2, 1, 3, 4, 1)),
        "cohort_start_date" = as.Date(c(
          "2020-01-01", "2020-01-02", "2020-07-01", "2020-01-04", "2020-01-05",
          "2020-01-06"
        )),
        "cohort_end_date" = as.Date(c(
          "2020-06-01", "2020-09-02", "2020-07-23", "2020-01-04", "2020-01-05",
          "2020-01-06"
        )),
        "extra_column" = c(1, 2, 3, 4, 5, 6)
      )
    )
  ) |>
    copyCdm()

  # test it works ----
  cdm$cohort2 <- cdm$cohort1 |>
    timeWindowCohorts(window = list("m1" = c(0, 30), "m2" = c(31, 60), c(0, Inf)), name = "cohort2")
  expect_equal(
    settings(cdm$cohort2),
    dplyr::tibble(
      cohort_definition_id = 1:12L,
      cohort_name = c(paste0("cohort_1_", c("m1", "m2", "0_to_inf")), paste0("cohort_2_", c("m1", "m2", "0_to_inf")), paste0("cohort_3_", c("m1", "m2", "0_to_inf")), paste0("cohort_", 1:3)),
      window = c(rep(c("[0, 30]", "[31, 60]", "[0, Inf]"), 3), NA, NA, NA),
      target_cohort_name = c(rep("cohort_1", 3), rep("cohort_2", 3), rep("cohort_3", 3), rep(NA, 3))
    )
  )
  expect_equal(
    attrition(cdm$cohort2)[1:8, "reason"] |> dplyr::pull(),
    c('Initial qualifying events', 'Records within [0, 30] days from cohort entry',
      'Initial qualifying events', 'Records within [31, 60] days from cohort entry',
      'Initial qualifying events', 'Records within [0, Inf] days from cohort entry',
      'Initial qualifying events', 'Records within [0, 30] days from cohort entry')
  )
  expect_equal(
    collectCohort(cdm$cohort2, 1),
    dplyr::tibble(
      subject_id = c(1L, 1L, 2L),
      cohort_start_date = as.Date(c("2020-01-01", "2020-07-01", "2020-01-02")),
      cohort_end_date = as.Date(c("2020-01-31", "2020-07-23", "2020-02-01"))
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    collectCohort(cdm$cohort1, 1),
    collectCohort(cdm$cohort2, 3),
    ignore_attr = TRUE
  )
  expect_equal(
    collectCohort(cdm$cohort1, 1),
    collectCohort(cdm$cohort2, 10),
    ignore_attr = TRUE
  )

  # check 1 day in split cohort, cohortId and keepOriginalCohorts ----
  cdm$cohort3 <- cdm$cohort1 |>
    timeWindowCohorts(window = list("m1" = c(1, 22), "m2" = c(22, 23)), name = "cohort3", keepOriginalCohorts = FALSE, cohortId = 1)
  expect_equal(
    collectCohort(cdm$cohort3, 1),
    dplyr::tibble(
      subject_id = c(1L, 1L, 2L),
      cohort_start_date = as.Date(c("2020-01-02", "2020-07-02", "2020-01-03")),
      cohort_end_date = as.Date(c("2020-01-23", "2020-07-23", "2020-01-24"))
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    collectCohort(cdm$cohort3, 2),
    dplyr::tibble(
      subject_id = c(1L, 1L, 2L),
      cohort_start_date = as.Date(c("2020-01-23", "2020-07-23", "2020-01-24")),
      cohort_end_date = as.Date(c("2020-01-24", "2020-07-23", "2020-01-25"))
    ),
    ignore_attr = TRUE
  )
  expect_true(nrow(settings(cdm$cohort3)) == 2)
  expect_true(nrow(attrition(cdm$cohort3)) == 4)

  # check codelist and name----
  cdm$cohort1 <- cdm$cohort1 |>
    omopgenerics::newCohortTable(
      cohortCodelistRef = dplyr::tibble(
        "cohort_definition_id" = 1L,
        "codelist_name" = "a",
        "concept_id" = 1L,
        "codelist_type" = "index event"
      )
    )
  cdm$cohort1 <- cdm$cohort1 |>
    timeWindowCohorts(window = list( c(1, 22)), keepOriginalCohorts = TRUE, cohortId = 1)
  expect_equal(
    settings(cdm$cohort1),
    dplyr::tibble(
      cohort_definition_id = 1:4L,
      cohort_name = c("cohort_1_1_to_22", paste0("cohort_", 1:3)),
      window = c("[1, 22]", NA, NA, NA),
      target_cohort_name = c("cohort_1", rep(NA, 3))
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    attr(cdm$cohort1, "cohort_codelist") |> dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = 1:2L,
      codelist_name = "a",
      "concept_id" = 1L,
      "codelist_type" = "index event"
    ),
    ignore_attr = TRUE
  )
})
