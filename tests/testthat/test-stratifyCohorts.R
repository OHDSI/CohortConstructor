test_that("simple stratification", {
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
  ) |>
    copyCdm()

  expect_no_error(cdm$cohort1 <- stratifyCohorts(cdm$cohort1, strata = list()))
  expect_no_error(
    cdm$new_cohort <- stratifyCohorts(cdm$cohort1, name = "new_cohort", strata = list())
  )
  expect_no_error(
    cdm$new_cohort <- cdm$cohort1 |>
      stratifyCohorts(
        strata = list(c("blood_type", "age_group"), "sex"),
        removeStrata = TRUE,
        name = "new_cohort"
      )
  )
  # 3 cdi * 3 blood * 2 age + 3 cdi * 2 sex
  expect_true(cdm$new_cohort |> settings() |> nrow() == 24)
  expect_true(
    all(colnames(cdm$new_cohort) %in% c(
      'cohort_definition_id', 'subject_id', 'cohort_start_date', 'cohort_end_date', 'extra_column'
    ))
  )
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

  expect_no_error(
    cdm$new_cohort_1 <- cdm$cohort1 |>
      stratifyCohorts(
        cohortId = "cohort_1",
        strata = list(c("blood_type", "age_group"), "sex"),
        removeStrata = FALSE,
        name = "new_cohort_1"
      )
  )
  # 3 cdi * 3 blood * 2 age + 3 cdi * 2 sex
  expect_true(cdm$new_cohort_1 |> settings() |> nrow() == 8)

  cdi <- settings(cdm$new_cohort_1) |>
    dplyr::filter(.data$blood_type == "A" & .data$age_group == "adult")
  expect_true(nrow(cdi) == 1)

  attritionCdi <- attrition(cdm$new_cohort_1) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cdi$cohort_definition_id) |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id)
  expect_true(nrow(attritionCdi) == 3)
  expect_identical(
    attritionCdi$reason |> unique() |> tolower() |> sort(), c(
      "filter strata: age_group == adult", "filter strata: blood_type == a",
      "initial qualifying events"
    )
  )
  expect_true(all(attritionCdi$number_records == c(3, 2, 1)))
  expect_true(all(attritionCdi$number_subjects == c(2, 2, 1)))
  expect_true(all(attritionCdi$excluded_records == c(0, 1, 1)))
  expect_true(all(attritionCdi$excluded_subjects == c(0, 0, 1)))
  expect_identical(colnames(cdm$new_cohort_1), c('cohort_definition_id', 'subject_id', 'cohort_start_date', 'cohort_end_date',
    'extra_column', 'blood_type', 'sex', 'age_group'))

  # test settings drop columns
  expect_message(
    cdm$new_cohort2 <- cdm$new_cohort_1 |>
      stratifyCohorts(
        strata = list(c("blood_type", "age_group"), "sex"),
        name = "new_cohort2"
      )
  )
  expect_identical(colnames(cdm$new_cohort2), c('cohort_definition_id', 'subject_id', 'cohort_start_date', 'cohort_end_date', 'extra_column'))

  cdm$new_cohort3 <- cdm$new_cohort |>
    stratifyCohorts(
      cohortId = 1,
      strata = list(),
      name = "new_cohort3"
    )
  expect_identical(collectCohort(cdm$new_cohort2, 1), collectCohort(cdm$new_cohort3, 1))

  # empty cohort
  cdm <- omopgenerics::emptyCohortTable(cdm, "empty_cohort")
  cdm$empty_cohort <- cdm$empty_cohort|> PatientProfiles::addSex(name = "empty_cohort")
  expect_no_error(cdm$empty_cohort <- stratifyCohorts(cdm$empty_cohort, strata = list("sex")))
  expect_true(cdm$empty_cohort |> dplyr::tally() |>dplyr::pull("n") == 0)

  # extra columns in settings and strata
  attr(cdm$cohort1, "cohort_set") <- attr(cdm$cohort1, "cohort_set") |> dplyr::mutate(sex = NA)
  expect_message(cdm$extracols <- stratifyCohorts(cdm$cohort1, strata = list("sex"), name = "extracols"))
  expect_true(all(settings(cdm$extracols)$sex |> unique() == c("Female", "Male")))

  # no column in the cohort
  expect_error(cdm$new_cohort <- stratifyCohorts(cdm$cohort1, strata = list("not_a_column")))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  # test indexes
  if (dbToTest == "postgres CDMConnector") {
    expect_true(
      DBI::dbGetQuery(CDMConnector::cdmCon(cdm), paste0("SELECT * FROM pg_indexes WHERE tablename = 'coco_test_new_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX coco_test_new_cohort_subject_id_cohort_start_date_idx ON public.coco_test_new_cohort USING btree (subject_id, cohort_start_date)"
    )
  }

  dropCreatedTables(cdm = cdm)
})

