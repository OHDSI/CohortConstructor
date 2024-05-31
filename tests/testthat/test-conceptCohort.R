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
  expect_error(conceptCohort(cdm = NULL, name = "cohort", conceptSet = NULL))

  # wrong naming
  expect_error(conceptCohort(cdm = cdm, name = NA, conceptSet = NULL))
  expect_error(conceptCohort(cdm = cdm, name = 1, conceptSet = NULL))
  expect_error(conceptCohort(cdm = cdm, name = c("ass", "asdf"), conceptSet = NULL))
  expect_error(conceptCohort(cdm = cdm, name = "AAAA", conceptSet = NULL))
  expect_no_error(x <- conceptCohort(cdm = cdm, name = "my_cohort", conceptSet = NULL))
  expect_true("my_cohort" == omopgenerics::tableName(x))

  # empty cohort
  expect_no_error(x <- conceptCohort(cdm = cdm, conceptSet = NULL, name = "cohort"))
  expect_true(inherits(x, "cohort_table"))
  expect_true(x |> dplyr::collect() |> nrow() == 0)

  # not codelist
  expect_error(x <- conceptCohort(cdm = cdm, conceptSet = 1, name = "cohort"))
  expect_error(x <- conceptCohort(cdm = cdm, conceptSet = list(1), name = "cohort"))
  expect_message(expect_message(
    x <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort")
  ))
  expect_true(inherits(x, "cohort_table"))
  expect_true(x |> dplyr::collect() |> nrow() == 0)
  expect_true(omopgenerics::tableName(x) == "cohort")
  expect_true(setdiff(names(omopgenerics::cdmReference(x)),
                      names(cdm)) == "cohort")
  expect_identical(setdiff(names(cdm), names(omopgenerics::cdmReference(x))), character())
  expect_equal(
    settings(x), dplyr::tibble("cohort_definition_id" = 1L, "cohort_name" = "a")
  )
  expect_true(nrow(attrition(x)) == 1)
  expect_true(nrow(attr(x, "cohort_codelist")) == 1)
  expect_message(expect_message(
    x <- conceptCohort(cdm = cdm, conceptSet = list(a = 2), name = "cohort")
  ))

})

test_that("simple example", {
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
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

  cdm <- cdm |> copyCdm()

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort"))

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

  PatientProfiles::mockDisconnect(cdm)
})

test_that("simple example duckdb", {
  testthat::skip_on_cran()
   cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
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

  cdm <- cdm |> copyCdm()

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort"))

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

  PatientProfiles::mockDisconnect(cdm)
})

test_that("concepts from multiple cdm tables duckdb", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1,
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    ))) |>
    omock::mockConditionOccurrence() |>
    omock::mockDrugExposure()
  cdm <- CDMConnector::copyCdmTo(con = DBI::dbConnect(duckdb::duckdb()),
                                 cdm = cdm, schema = "main")

  cs <- list("a" = cdm$condition_occurrence |>
    dplyr::select("condition_concept_id") |>
    head(1)|>
    dplyr::pull(),
    "b" = cdm$drug_exposure |>
    dplyr::select("drug_concept_id") |>
    head(1) |>
    dplyr::pull())

  expect_no_error(cohort <- conceptCohort(cdm = cdm,
                                          conceptSet = cs,
                                          name = "my_new_cohort"))

})

test_that("excluded concepts in codelist", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
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

  cdm <- cdm |> copyCdm()

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1:2), name = "cohort1"))
  expect_true(all(attr(cohort, "cohort_codelist") |> dplyr::pull("concept_id") |> sort() == 1:2))

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 2:3), name = "cohort2"))
  expect_true(all(attr(cohort, "cohort_codelist") |> dplyr::pull("concept_id") |> sort() == 2:3))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("out of observation", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod()
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1, 2),
    "concept_name" = c("my concept 1", "my concept 2"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:13,
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 4, 4),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, 430, -10),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, 400, -100),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )

  cdm <- cdm_local |> copyCdm()

  # start end after (subject 2, and some of 1)
  # start end before (subject 3)
  # end event < start event (subject 4)
  cdm$cohort1 <- conceptCohort(cdm = cdm,
                               conceptSet = list(a = 1, b = 2), name = "cohort1")
  expect_true(all(c("cohort_table", "cdm_table") %in% class(cdm$cohort1)))
  # person 1 has two cohort entries
  # the first - same dates as drug exposure
  # the second - limit cohort end based on observation period end date
  expect_true(all(cdm$cohort1 |>
                dplyr::pull("subject_id") == 1))
  expect_true(cdm$cohort1 |>
                dplyr::filter(cohort_definition_id == 1) |>
                dplyr::pull("cohort_start_date") == "2010-01-01")
  expect_true(cdm$cohort1 |>
                dplyr::filter(cohort_definition_id == 1) |>
                dplyr::pull("cohort_end_date") == "2012-03-11")
  expect_true(cdm$cohort1 |>
                dplyr::filter(cohort_definition_id == 2) |>
                dplyr::pull("cohort_start_date") == "2012-01-21")
  expect_true(cdm$cohort1 |>
                dplyr::filter(cohort_definition_id == 2) |>
                dplyr::pull("cohort_end_date") == "2013-06-29")

  expect_true(all(omopgenerics::settings(cdm$cohort1)$cohort_name == c("a", "b")))
  expect_true(cohortCodelist(cdm$cohort1, 1)$a == 1)
  expect_true(cohortCodelist(cdm$cohort1, 2)$b == 2)


  # event starts in, ends out (subject 1)
  # event starts out, end in (subject 3)
  # no concept 2
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod()
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1, 2),
    "concept_name" = c("my concept 1", "my concept 2"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:4,
    "person_id" = c(1, 3, 4, 2),
    "drug_concept_id" = 1,
    "drug_exposure_start_date" = as.Date(c("2004-01-01", "2014-01-01", "2001-01-01", "2000-01-01")),
    "drug_exposure_end_date" = as.Date(c("2015-01-01", "2015-05-01", "2002-01-01", "2000-02-02")),
    "drug_type_concept_id" = 1
  )
  cdm <- cdm_local |> copyCdm()

  cdm$cohort2 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1, b = 2), name = "cohort2")
  expect_true(all(c("cohort_table", "cdm_table") %in% class(cdm$cohort2)))
  expect_true(all(cdm$cohort2 |> dplyr::pull("subject_id") |> sort() == c(1, 2, 4)))
  expect_true(all(cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2000-01-01", "2001-01-01","2004-01-01")))
  expect_true(all(cdm$cohort2 |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("2000-02-02", "2002-01-01","2013-06-29")))
  expect_true(all(omopgenerics::settings(cdm$cohort2)$cohort_name |> sort() ==
                    c("a", "b")))
  expect_true(cohortCodelist(cdm$cohort2, 1)$a == 1)
  expect_true(cohortCodelist(cdm$cohort2, 2)$b == 2)

  PatientProfiles::mockDisconnect(cdm)

  # start date > end date (subject 1)
  # overlapping and out of observation (subject 2)
  # out of observation (subject 3)
  # overlapping (subject 4)
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod()
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1, 2),
    "concept_name" = c("my concept 1", "my concept 2"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:6,
    "person_id" = c(1, 2, 2, 3, 4, 4),
    "drug_concept_id" = c(1, 1, 1, 1, 2, 2),
    "drug_exposure_start_date" = as.Date(c("2004-01-01", "2014-01-01", "2015-04-01", "2000-01-01", "2000-01-01", "1999-01-01")),
    "drug_exposure_end_date" = as.Date(c("2003-01-01", "2015-05-01", "2015-07-01", "2000-02-02", "2000-01-01", "2001-01-01")),
    "drug_type_concept_id" = 1
  )
  cdm <- cdm_local |> copyCdm()

  # empty cohort
  cdm$cohort3 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort3")
  expect_true(all(c("cohort_table", "cdm_table") %in% class(cdm$cohort3)))
  expect_true(cdm$cohort3 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(cohortCodelist(cdm$cohort3, 1)$a == 1)

  # empty cohort
  cdm$cohort4 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1, b = 2), name = "cohort4")
  expect_true(all(c("cohort_table", "cdm_table") %in% class(cdm$cohort4)))
  expect_true(cdm$cohort4 |> dplyr::tally() |> dplyr::pull("n") == 1)
  expect_true(cohortCodelist(cdm$cohort4, 1)$a == 1)
  expect_true(cohortCodelist(cdm$cohort4, 2)$b == 2)
  expect_true(cdm$cohort4 |> dplyr::pull("subject_id") |> sort() == 4)
  expect_true(cdm$cohort4 |> dplyr::pull("cohort_start_date") == "1999-01-01")

  PatientProfiles::mockDisconnect(cdm)
})

test_that("table not present in the cdm", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1,
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1:2,
      "concept_name" = c("my concept", "my other concept"),
      "domain_id" = c("drug", "condition"),
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
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
      "drug_type_concept_id" = 1
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2020-01-01")
      )
  )

  cdm <- cdm |> copyCdm()

  expect_warning(cdm$conceptcohort <- conceptCohort(cdm, list(a = 1, b = 1, c = 1:2, d = 2), name = "conceptcohort"))
  expect_true(all(cdm$conceptcohort |> dplyr::pull(cohort_definition_id) |> unique() |> sort() == 1:3))
  expect_true(all(cdm$conceptcohort |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-11", "2020-01-11",
                      "2020-01-11", "2024-02-09", "2024-02-09", "2024-02-09", "2024-12-05",
                      "2024-12-05", "2024-12-05", "2024-12-08", "2024-12-08", "2024-12-08")))

  PatientProfiles::mockDisconnect(cdm)
})
