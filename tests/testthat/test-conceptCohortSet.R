test_that("expected errors and messages", {

  cdm <- omock::mockCdmReference() |>
    omock::mockPerson() |>
    omock::mockObservationPeriod()
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1,
      "concept_name" = "my concept",
      "domain_id" = "adsf",
      "vocabulary_id" = NA,
      "concept_class_id" = NA,
      "concept_code" = NA,
      "valid_start_date" = NA,
      "valid_end_date" = NA
    )
  )

  # not a cdm reference
  expect_error(conceptCohortSet(cdm = NULL))

  # wrong naming
  expect_error(conceptCohortSet(cdm = cdm, name = NA))
  expect_error(conceptCohortSet(cdm = cdm, name = 1))
  expect_error(conceptCohortSet(cdm = cdm, name = c("ass", "asdf")))
  expect_error(conceptCohortSet(cdm = cdm, name = "AAAA"))
  expect_no_error(x <- conceptCohortSet(cdm = cdm, name = "my_cohort"))
  expect_true("my_cohort" == omopgenerics::tableName(x))

  # empty cohort
  expect_no_error(x <- conceptCohortSet(cdm = cdm, conceptSet = NULL))
  expect_true(inherits(x, "cohort_table"))
  expect_true(x |> dplyr::collect() |> nrow() == 0)

  # not codelist
  expect_error(x <- conceptCohortSet(cdm = cdm, conceptSet = 1))
  expect_error(x <- conceptCohortSet(cdm = cdm, conceptSet = list(1)))
  expect_message(expect_message(
    x <- conceptCohortSet(cdm = cdm, conceptSet = list(a = 1))
  ))
  expect_true(inherits(x, "cohort_table"))
  expect_true(x |> dplyr::collect() |> nrow() == 0)
  expect_true(omopgenerics::tableName(x) == "cohort")
  expect_true(setdiff(names(omopgenerics::cdmReference(x)), names(cdm)) == "cohort")
  expect_identical(setdiff(names(cdm), names(omopgenerics::cdmReference(x))), character())
  expect_equal(
    settings(x), dplyr::tibble("cohort_definition_id" = 1L, "cohort_name" = "a")
  )
  expect_true(nrow(attrition(x)) == 1)
  # currently only standard concepts are includes in cohortCodelist see https://github.com/oxford-pharmacoepi/CohortConstructor/issues/74
  expect_warning(expect_equal(
    cohortCodelist(x, 1), omopgenerics::newCodelist(list())
  ))
  expect_message(expect_message(
    x <- conceptCohortSet(cdm = cdm, conceptSet = list(a = 2))
  ))

  # verbose
  expect_error(x <- conceptCohortSet(cdm = cdm, verbose = "my_cohort"))
  expect_error(x <- conceptCohortSet(cdm = cdm, verbose = c(T, F)))
  expect_error(x <- conceptCohortSet(cdm = cdm, verbose = as.logical(NA)))

})

test_that("simple example", {
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTable(cohortTable = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1,
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1,
      "concept_name" = "my concept",
      "domain_id" = "drUg",
      "vocabulary_id" = NA,
      "concept_class_id" = NA,
      "concept_code" = NA,
      "valid_start_date" = NA,
      "valid_end_date" = NA
    )
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = 1:11,
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
      "drug_type_concept_id" = 1
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2020-01-01")
      )
  )

  expect_no_error(cohort <- conceptCohortSet(cdm = cdm, conceptSet = list(a = 1)))

  expect_true(cohort |> dplyr::tally() |> dplyr::pull() == 4)
  expect_true(cohortCount(cohort)$number_records == 4)
  expect_true(cohortCount(cohort)$number_subjects == 2)
  expect_true(attrition(cohort) |> nrow() == 1)
  expect_identical(
    settings(cohort),
    dplyr::tibble("cohort_definition_id" = 1L, "cohort_name" = "a")
  )
  expect_identical(cohortCodelist(cohort, 1), omopgenerics::newCodelist(list(a = 1)))
  cohort <- cohort |>
    dplyr::collect() |>
    dplyr::as_tibble() |>
    dplyr::arrange(subject_id, cohort_start_date)
  attr(cohort, "cohort_attrition") <- NULL
  attr(cohort, "cohort_codelist") <- NULL
  attr(cohort, "cohort_set") <- NULL
  expect_equal(
    cohort,
    dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 1L, 1L, 2L),
      "cohort_start_date" = as.Date(c(0, 1500, 1800, 10), origin = "2020-01-01"),
      "cohort_end_date" = as.Date(c(800, 1600, 1804, 2000), origin = "2020-01-01")
    )
  )

})

test_that("simple example duckdb", {
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTable(cohortTable = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1,
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1,
      "concept_name" = "my concept",
      "domain_id" = "drUg",
      "vocabulary_id" = NA,
      "concept_class_id" = NA,
      "concept_code" = NA,
      "valid_start_date" = NA,
      "valid_end_date" = NA
    )
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = 1:11,
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
      "drug_type_concept_id" = 1
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2020-01-01")
      )
  )

  cdm <- CDMConnector::copyCdmTo(con = DBI::dbConnect(duckdb::duckdb()), cdm = cdm, schema = "main")

  expect_no_error(cohort <- conceptCohortSet(cdm = cdm, conceptSet = list(a = 1)))

  expect_true(cohort |> dplyr::tally() |> dplyr::pull() == 4)
  expect_true(cohortCount(cohort)$number_records == 4)
  expect_true(cohortCount(cohort)$number_subjects == 2)
  expect_true(attrition(cohort) |> nrow() == 1)
  expect_identical(
    settings(cohort),
    dplyr::tibble("cohort_definition_id" = 1L, "cohort_name" = "a")
  )
  expect_identical(cohortCodelist(cohort, 1), omopgenerics::newCodelist(list(a = 1)))
  cohort <- cohort |>
    dplyr::collect() |>
    dplyr::as_tibble() |>
    dplyr::arrange(subject_id, cohort_start_date)
  attr(cohort, "cohort_attrition") <- NULL
  attr(cohort, "cohort_codelist") <- NULL
  attr(cohort, "cohort_set") <- NULL
  expect_equal(
    cohort,
    dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 1L, 1L, 2L),
      "cohort_start_date" = as.Date(c(0, 1500, 1800, 10), origin = "2020-01-01"),
      "cohort_end_date" = as.Date(c(800, 1600, 1804, 2000), origin = "2020-01-01")
    )
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})
