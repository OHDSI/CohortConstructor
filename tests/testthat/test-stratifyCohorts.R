test_that("simple stratification", {
  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      "person" = dplyr::tibble(
        "person_id" = 1:4,
        "gender_concept_id" = 0,
        "year_of_birth" = 0L,
        "race_concept_id" = 0L,
        "ethnicity_concept_id" = 0L
      ),
      "observation_period" = dplyr::tibble(
        "observation_period_id" = 1:4,
        "person_id" = 1:4,
        "observation_period_start_date" = as.Date("2000-01-01"),
        "observation_period_end_date" = as.Date("2030-01-01"),
        "period_type_concept_id" = 0L
      )
    ),
    cdmName = "mock db",
    cohortTables = list(
      "cohort1" = dplyr::tibble(
        "cohort_definition_id" = c(1, 1, 1, 2, 2, 3),
        "subject_id" = c(1, 2, 1, 3, 4, 1),
        "cohort_start_date" = as.Date(c(
          "2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05",
          "2020-01-06"
        )),
        "cohort_end_date" = as.Date(c(
          "2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05",
          "2020-01-06"
        )),
        "extra_column" = c(1, 2, 3, 4, 5, 6),
        "blood_type" = c("A", "B", "A", "A", "B", "0"),
        "sex" = c("Female", "Female", "Female", "Male", "Female", "Male"),
        "age_group" = c("child", "adult", "adult", "child", "child", "child")
      )
    )
  )

  con <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copyCdmTo(con = con, cdm = cdm, schema = "main")

  expect_error(cdm$new_cohort <- stratifyCohorts(cdm$cohort1))
  expect_no_error(
    cdm$new_cohort <- stratifyCohorts(cdm$cohort1, name = "new_cohort")
  )
  expect_warning(
    expect_no_error(
      cdm$new_cohort <- cdm$cohort1 |>
        stratifyCohorts(
          strata = list(c("blood_type", "age_group"), "sex"),
          name = "new_cohort"
        )
    )
  )
  # 3 cdi * 3 blood * 2 age + 3 cdi * 2 sex
  expect_true(cdm$new_cohort |> settings() |> nrow() == 24)

  cdi <- settings(cdm$new_cohort) |>
    dplyr::filter(.data$blood_type == "A" & .data$age_group == "adult")
  expect_true(nrow(cdi) == 3)

  attritionCdi <- attrition(cdm$new_cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cdi$cohort_definition_id) |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id)
  expect_true(nrow(attritionCdi) == 9)
  expect_identical(
    attritionCdi$reason |> unique() |> tolower() |> sort(), c(
      "filter strata: age_group == adult", "filter strata: blood_type == a",
      "initial qualifying events"
    )
  )
  expect_true(all(attritionCdi$number_records == c(3, 2, 1, 2, 1, 0, 1, 0, 0)))
  expect_true(all(attritionCdi$number_subjects == c(2, 2, 1, 2, 1, 0, 1, 0, 0)))
  expect_true(all(attritionCdi$excluded_records == c(0, 1, 1, 0, 1, 1, 0, 1, 0)))
  expect_true(all(attritionCdi$excluded_subjects == c(0, 0, 1, 0, 1, 1, 0, 1, 0)))

  duckdb::dbDisconnect(conn = con, shutdown = TRUE)
})