test_that("ageCohort works", {
  skip_on_cran()
  asTibble <- function(x) {
    x <- dplyr::collect(x) |>
      dplyr::mutate(cohort_definition_id = 1L)
    attr(x, "cdm_reference") <- NULL
    attr(x, "cohort_set") <- NULL
    attr(x, "cohort_attrition") <- NULL
    attr(x, "cohort_codelist") <- NULL
    return(x)
  }

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = dplyr::tibble(
        person_id = c(1L, 2L, 3L),
        year_of_birth = c(1990L, 1989L, NA),
        month_of_birth = c(NA, 6L, 8L),
        day_of_birth = c(5L, NA, 14L),
        gender_concept_id = 0L,
        race_concept_id = 0L,
        ethnicity_concept_id = 0L
      ),
      observation_period = dplyr::tibble(
        observation_period_id = c(1L, 2L, 3L),
        person_id = c(1L, 1L, 2L),
        observation_period_start_date = as.Date("1990-01-01") + c(20L, 100L, 0L),
        observation_period_end_date = as.Date("1990-01-01") + c(50L, 1000L, 10000L),
        period_type_concept_id = 0L
      )
    ),
    cdmName = "test"
  ) |>
    copyCdm()

  # birthday 0
  expect_no_error(cdm$cohort_0 <- ageCohort(cdm = cdm, name = "cohort_0"))
  expect_true(nrow(attrition(cdm$cohort_0)) == 3)
  expect_true(all(attrition(cdm$cohort_0)$excluded_records == c(0L, 1L, 2L)))

  # birthday 20 days
  expect_no_error(cdm$cohort_1 <- ageCohort(cdm = cdm, name = "cohort_1", age = 20, ageUnit = "days"))
  expect_true(cohortCount(cdm$cohort_1)$number_records == 1L)
  expect_true(cdm$cohort_1 |> dplyr::pull("cohort_start_date") == as.Date("1990-01-25"))
  expect_true(all(attrition(cdm$cohort_1)$excluded_records == c(0L, 1L, 1L)))

  # birthday 70 days
  expect_no_error(cdm$cohort_2 <- ageCohort(cdm = cdm, name = "cohort_2", age = 70, ageUnit = "days"))
  expect_true(cohortCount(cdm$cohort_2)$number_records == 0L)
  expect_true(all(attrition(cdm$cohort_2)$excluded_records == c(0L, 1L, 2L)))

  # birthday 1 year
  expect_no_error(cdm$cohort_3 <- ageCohort(cdm = cdm, name = "cohort_3", age = 1, ageUnit = "years"))
  expect_true(cohortCount(cdm$cohort_3)$number_records == 2L)
  expect_true(
    cdm$cohort_3 |>
      dplyr::filter(.data$subject_id == 1) |>
      dplyr::pull("cohort_start_date") == as.Date("1991-01-05")
  )
  expect_true(
    cdm$cohort_3 |>
      dplyr::filter(.data$subject_id == 2) |>
      dplyr::pull("cohort_start_date") == as.Date("1990-06-01")
  )
  expect_true(all(attrition(cdm$cohort_3)$excluded_records == c(0L, 1L, 0L)))

  # birthday 10 years
  expect_no_error(cdm$cohort_4 <- ageCohort(cdm = cdm, name = "cohort_4", age = 10, ageUnit = "years"))
  expect_true(cohortCount(cdm$cohort_4)$number_records == 1L)
  expect_true(
    cdm$cohort_4 |>
      dplyr::filter(.data$subject_id == 2) |>
      dplyr::pull("cohort_start_date") == as.Date("1999-06-01")
  )
  expect_true(all(attrition(cdm$cohort_4)$excluded_records == c(0L, 1L, 1L)))

  # all the above + tune names
  expect_no_error(cdm$cohort_5 <- ageCohort(
    cdm = cdm,
    name = "cohort_5",
    age = c(0, 20, 70, 1, 10),
    ageUnit = c("years", "days", "days", "years", "years"),
    cohortName = "custom_{ageUnit}_{age}"
  ))
  expect_true(nrow(attrition(cdm$cohort_5)) == 15)
  expect_identical(
    settings(cdm$cohort_5)$cohort_name,
    c("custom_years_0", "custom_days_20", "custom_days_70", "custom_years_1", "custom_years_10")
  )
  expect_identical(
    asTibble(cdm$cohort_0),
    cdm$cohort_5 |>
      dplyr::filter(cohort_definition_id == 1) |>
      asTibble()
  )
  expect_identical(
    asTibble(cdm$cohort_1),
    cdm$cohort_5 |>
      dplyr::filter(cohort_definition_id == 2) |>
      asTibble()
  )
  expect_identical(
    asTibble(cdm$cohort_2),
    cdm$cohort_5 |>
      dplyr::filter(cohort_definition_id == 3) |>
      asTibble()
  )
  expect_identical(
    asTibble(cdm$cohort_3),
    cdm$cohort_5 |>
      dplyr::filter(cohort_definition_id == 4) |>
      asTibble()
  )
  expect_identical(
    asTibble(cdm$cohort_4),
    cdm$cohort_5 |>
      dplyr::filter(cohort_definition_id == 5) |>
      asTibble()
  )

  # age_unit also works
  expect_no_error(cdm$cohort_6 <- ageCohort(
    cdm = cdm,
    name = "cohort_6",
    age = c(0, 20, 70, 1, 10),
    ageUnit = c("years", "days", "days", "years", "years"),
    cohortName = "custom_{age_unit}_{age}"
  ))
  expect_identical(settings(cdm$cohort_5), settings(cdm$cohort_6))

  # provide names
  expect_no_error(cdm$cohort_7 <- ageCohort(
    cdm = cdm,
    name = "cohort_7",
    age = c(0, 20),
    ageUnit = c("years"),
    cohortName = c("cohort_1", "hi")
  ))
  expect_identical(settings(cdm$cohort_7)$cohort_name, c("cohort_1", "hi"))

  # wrong use of cohortName
  expect_error(cdm$cohort_8 <- ageCohort(
    cdm = cdm,
    name = "cohort_8",
    age = c(0, 20),
    ageUnit = c("years"),
    cohortName = "just_one_name"
  ))
  expect_error(cdm$cohort_8 <- ageCohort(
    cdm = cdm,
    name = "cohort_8",
    age = c(0),
    ageUnit = c("years"),
    cohortName = c("too_many_1", "too_many_2")
  ))

  # error of ageUnit
  expect_error(cdm$cohort_8 <- ageCohort(
    cdm = cdm,
    name = "cohort_8",
    age = c(0, 20),
    ageUnit = c("years", "days", "years")
  ))

  # empty cohort
  expect_warning(cdm$cohort_8 <- ageCohort(
    cdm = cdm,
    name = "cohort_8",
    age = integer()
  ))

  dropCreatedTables(cdm = cdm)
})
