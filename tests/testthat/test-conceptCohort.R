test_that("expected errors and messages", {
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson() |>
    omock::mockObservationPeriod()
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1L,
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
  expect_error(conceptCohort(cdm = cdm, conceptSet = NULL, name = "cohort"))

  # empty cohort from empty concept
  expect_no_error(x <- conceptCohort(cdm = cdm, conceptSet = list(), name = "cohort"))
  expect_true(inherits(x, "cohort_table"))
  expect_true(x |> dplyr::collect() |> nrow() == 0)
  expect_equal(
    colnames(settings(x)) |> sort(),
    c("cdm_version", "cohort_definition_id", "cohort_name", "vocabulary_version")
  )
  expect_true(settings(x) |> nrow() == 0)
  expect_true(attrition(x) |> nrow() == 0)

  # not codelist
  expect_error(x <- conceptCohort(cdm = cdm, conceptSet = 1L, name = "cohort"))
  expect_error(x <- conceptCohort(cdm = cdm, conceptSet = list(1L), name = "cohort"))
  expect_message(expect_message(
    x <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L), name = "cohort")
  ))
  expect_true(inherits(x, "cohort_table"))
  expect_true(x |> dplyr::collect() |> nrow() == 0)
  expect_true(omopgenerics::tableName(x) == "cohort")
  expect_true(setdiff(names(omopgenerics::cdmReference(x)),
                      names(cdm)) == "cohort")
  expect_identical(setdiff(names(cdm), names(omopgenerics::cdmReference(x))), character())
  expect_equal(
    settings(x),
    dplyr::tibble(
      "cohort_definition_id" = 1L, "cohort_name" = "a",
      "cdm_version" = attr(cdm, "cdm_version"), "vocabulary_version" = "mock")
  )
  expect_true(nrow(attrition(x)) == 1)
  expect_true(nrow(attr(x, "cohort_codelist")) == 1)
  expect_message(expect_message(
    x <- conceptCohort(cdm = cdm, conceptSet = list(a = 2L), name = "cohort")
  ))

})

test_that("simple example", {
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1L,
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
      "drug_exposure_id" = 1:11 |> as.integer(),
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1) |> as.integer(),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1) |> as.integer(),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
      "drug_type_concept_id" = 1
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2020-01-01")
      )
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "cohort", table = dplyr::tibble(
      "cohort_definition_id" = c(1, 1, 2) |> as.integer(),
      "subject_id" = c(1, 2, 3) |> as.integer(),
      "cohort_start_date" = c(0, 300, 1500) |> as.Date(origin = "2020-01-01"),
      "cohort_end_date" = c(400, 800, 1600) |> as.Date(origin = "2020-01-01")
    )
  )

  cdm <- cdm |> copyCdm()

  isDuckdb <- attr(omopgenerics::cdmSource(cdm), "source_type") == "duckdb"
  if(isDuckdb){
    startTempTables <- countDuckdbTempTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
    startPermanentTables <- countDuckdbPermanentTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
  }
  expect_no_error(cohort <- conceptCohort(cdm = cdm,
                                          conceptSet = list(a = 1L),
                                          name = "my_cohort"))
  if(isDuckdb){
    endTempTables <- countDuckdbTempTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
    endPermanentTables <- countDuckdbPermanentTables(
      con = attr(omopgenerics::cdmSource(cdm),
                 "dbcon"))
    # we should have only added 4 permanent tables (the new cohort table and
    # three tables with settings, attrition, and codelist)
    # no temp tables will have been created
    expect_true(startTempTables == endTempTables)
    expect_true(
      startPermanentTables + 4 == endPermanentTables
    )
  }

  expect_true(cohort |> dplyr::tally() |> dplyr::pull() == 4)
  expect_true(cohortCount(cohort)$number_records == 4)
  expect_true(cohortCount(cohort)$number_subjects == 2)
  expect_identical(
    settings(cohort),
    dplyr::tibble(
      "cohort_definition_id" = 1L, "cohort_name" = "a",
      "cdm_version" = attr(cdm, "cdm_version"), "vocabulary_version" = "mock"
    )
  )
  expect_identical(
    attrition(cohort) |> dplyr::as_tibble(),
    dplyr::tibble(
      "cohort_definition_id" = 1L,
      "number_records" = c(9L, 9L, 9L, 4L),
      "number_subjects" = 2L,
      "reason_id" = 1:4L,
      "reason" = c(
        "Initial qualifying events", "Record start <= record end",
        "Record in observation", "Collapse overlapping records"
      ),
      "excluded_records" = c(0L, 0L, 0L, 5L),
      "excluded_subjects" = 0L
    )
  )
  expect_identical(cohortCodelist(cohort, 1), omopgenerics::newCodelist(list(a = 1L)))
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

  # subsetCohort ----
  cdm$cohort <- cdm$cohort |> omopgenerics::newCohortTable()
  cdm$cohort <- cdm$cohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = dplyr::tibble(
        "cohort_definition_id" = c(1L, 2L, 3L),
        "cohort_name" = paste0("cohort", 1:3),
      ),
      cohortAttritionRef = dplyr::bind_rows(
        attrition(cdm$cohort),attrition(cdm$cohort)[1] |> dplyr::mutate("cohort_definition_id" = 3L)
      )
    )
  cohort2 <- conceptCohort(cdm = cdm,
                           conceptSet = list(a = 1L),
                           name = "my_cohort2",
                           subsetCohort = cdm$cohort)
  expect_equal(collectCohort(cohort2, 1), collectCohort(cohort, 1))
  cohort3 <- conceptCohort(cdm = cdm,
                           conceptSet = list(a = 1L),
                           name = "my_cohort3",
                           subsetCohort = cdm$cohort,
                           subsetCohortId = 2)
  expect_true(nrow(collectCohort(cohort3, 1)) == 0)
  expect_error(conceptCohort(cdm = cdm,
                             conceptSet = list(a = 1L),
                             name = "my_cohort3",
                             subsetCohort = cdm$cohort,
                             subsetCohortId = 3))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("simple example duckdb", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = 1:3L,
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1L,
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
      "drug_exposure_id" = 1:11L,
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1) |> as.integer(),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1) |> as.integer(),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
      "drug_type_concept_id" = 1L
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2020-01-01")
      )
  )

  cdm <- cdm |> copyCdm()

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L), name = "cohort"))

  expect_true(cohort |> dplyr::tally() |> dplyr::pull() == 4)
  expect_true(cohortCount(cohort)$number_records == 4L)
  expect_true(cohortCount(cohort)$number_subjects == 2L)
  # expect_true(attrition(cohort) |> nrow() == 1)
  expect_identical(
    settings(cohort),
    dplyr::tibble(
      "cohort_definition_id" = 1L, "cohort_name" = "a",
      "cdm_version" = attr(cdm, "cdm_version"), "vocabulary_version" = "mock"
    )
  )
  expect_identical(cohortCodelist(cohort, 1), omopgenerics::newCodelist(list(a = 1L)))
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
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    ))) |>
    omock::mockConditionOccurrence() |>
    omock::mockDrugExposure()
  cdm <- copyCdm(cdm)

  cs <- list("a" = cdm$condition_occurrence |>
               dplyr::select("condition_concept_id") |>
               head(1)|>
               dplyr::pull() |>
               as.integer(),
             "b" = cdm$drug_exposure |>
               dplyr::select("drug_concept_id") |>
               head(1) |>
               dplyr::pull() |>
               as.integer())

  expect_no_error(cohort <- conceptCohort(cdm = cdm,
                                          conceptSet = cs,
                                          name = "my_new_cohort"))
  PatientProfiles::mockDisconnect(cdm)
})

test_that("excluded concepts in codelist", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1L,
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
      "drug_exposure_id" = 1:11L,
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1) |> as.integer(),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1) |> as.integer(),
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

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1:2L), name = "cohort1"))
  expect_true(all(attr(cohort, "cohort_codelist") |> dplyr::pull("concept_id") |> sort() == 1:2))

  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 2:3L), name = "cohort2"))
  expect_true(all(attr(cohort, "cohort_codelist") |> dplyr::pull("concept_id") |> sort() == 2:3))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("out of observation", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1)
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1L, 2L),
    "concept_name" = c("my concept 1", "my concept 2"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:13L,
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 4, 4) |> as.integer(),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2) |> as.integer(),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, 430, -10),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, 400, -100),
    "drug_type_concept_id" = 1L
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
                               conceptSet = list(a = 1L, b = 2L), name = "cohort1")
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
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1)
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1L, 2L),
    "concept_name" = c("my concept 1", "my concept 2"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:4L,
    "person_id" = c(1L, 3L, 4L, 2L),
    "drug_concept_id" = 1L,
    "drug_exposure_start_date" = as.Date(c("2004-01-01", "2014-01-01", "2001-01-01", "2000-01-01")),
    "drug_exposure_end_date" = as.Date(c("2015-01-01", "2015-05-01", "2002-01-01", "2000-02-02")),
    "drug_type_concept_id" = 1L
  )
  cdm <- cdm_local |> copyCdm()

  cdm$cohort2 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L, b = 2L), name = "cohort2")
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
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1)
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1L, 2L),
    "concept_name" = c("my concept 1", "my concept 2"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:6L,
    "person_id" = c(1, 2, 2, 3, 4, 4) |> as.integer(),
    "drug_concept_id" = c(1, 1, 1, 1, 2, 2) |> as.integer(),
    "drug_exposure_start_date" = as.Date(c("2004-01-01", "2014-01-01", "2015-04-01", "2000-01-01", "2000-01-01", "1999-01-01")),
    "drug_exposure_end_date" = as.Date(c("2003-01-01", "2015-05-01", "2015-07-01", "2000-02-02", "2000-01-01", "2001-01-01")),
    "drug_type_concept_id" = 1L
  )
  cdm <- cdm_local |> copyCdm()

  # empty cohort
  cdm$cohort3 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L), name = "cohort3")
  expect_true(all(colnames(cdm$cohort3) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))
  expect_true(all(c("cohort_table", "cdm_table") %in% class(cdm$cohort3)))
  expect_true(cdm$cohort3 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(cohortCodelist(cdm$cohort3, 1)$a == 1)

  # empty cohort
  cdm$cohort4 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L, b = 2L), name = "cohort4")
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
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1:2L,
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
      "drug_exposure_id" = 1:11L,
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1) |> as.integer(),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1) |> as.integer(),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
      "drug_type_concept_id" = 1L
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2020-01-01")
      )
  )

  cdm <- cdm |> copyCdm()

  expect_warning(cdm$conceptcohort <- conceptCohort(cdm, list(a = 1L, b = 1L, c = 1:2L, d = 2L), name = "conceptcohort"))
  expect_true(all(cdm$conceptcohort |> dplyr::pull(cohort_definition_id) |> unique() |> sort() == 1:3))
  expect_true(all(cdm$conceptcohort |> dplyr::pull(cohort_start_date) |> sort() ==
                    c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-11", "2020-01-11",
                      "2020-01-11", "2024-02-09", "2024-02-09", "2024-02-09", "2024-12-05",
                      "2024-12-05", "2024-12-05", "2024-12-08", "2024-12-08", "2024-12-08")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("cohort exit as event start date", {
  skip_on_cran()
   cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1L,
      "concept_name" = "my concept",
      "domain_id" = "drug",
      "vocabulary_id" = NA,
      "concept_class_id" = NA,
      "concept_code" = NA,
      "valid_start_date" = NA,
      "valid_end_date" = NA
    )
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = c(1L, 2L),
      "person_id" = c(1L, 1L),
      "drug_concept_id" = c(1L, 1L),
      "drug_exposure_start_date" = as.Date(c("2020-01-01",
                                             "2020-01-04")),
      "drug_exposure_end_date" = as.Date(c("2020-01-10",
                                           "2020-01-14")),
      "drug_type_concept_id" = 1L
    ) )

  cdm <- cdm |> copyCdm()

  expect_no_error(cdm$cohort_1 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L),
                                                name = "cohort_1",
                                                exit = "event_end_date"))
  # one entry from first start to second end
  expect_true(nrow(cdm$cohort_1 |>
                     dplyr::collect()) == 1)
  expect_true(cdm$cohort_1 |>
                dplyr::pull("cohort_start_date") == as.Date(c("2020-01-01")))
  expect_true(cdm$cohort_1 |>
                dplyr::pull("cohort_end_date") == as.Date(c("2020-01-14")))

  # exit as event start
  expect_no_error(cdm$cohort_1 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L),
                                                name = "cohort_1",
                                                exit = "event_start_date"))
  expect_true(nrow(cdm$cohort_1 |>
                     dplyr::collect()) == 2)
  expect_identical(sort(as.Date(cdm$cohort_1 |>
                                  dplyr::pull("cohort_start_date"))),
                   c(as.Date("2020-01-01"),
                     "2020-01-04"))
  expect_identical(sort(as.Date(cdm$cohort_1 |>
                                  dplyr::pull("cohort_end_date"))),
                   c(as.Date("2020-01-01"),
                     "2020-01-04"))

  # expected error
  expect_error(cdm$cohort_1 <- conceptCohort(cdm = cdm,
                                             conceptSet = list(a = 1L),
                                             name = "cohort_1",
                                             exit = "not_an_option"))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("use source field concepts", {
  skip_on_cran()
   cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 99L,
      "concept_name" = "my_non_standard_concept",
      "domain_id" = "drug",
      "vocabulary_id" = NA,
      "concept_class_id" = NA,
      "concept_code" = NA,
      "valid_start_date" = NA,
      "valid_end_date" = NA
    )
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = c(1L, 2L),
      "person_id" = c(1L, 1L),
      "drug_concept_id" = c(1L, 1L),
      "drug_exposure_start_date" = as.Date(c("2020-01-01",
                                             "2020-01-04")),
      "drug_exposure_end_date" = as.Date(c("2020-01-10",
                                           "2020-01-14")),
      "drug_type_concept_id" = 1L,
      "drug_source_concept_id" = 99L
    ) )

  cdm <- cdm |> copyCdm()

  # no records if we only look at standard concepts
  expect_no_error(cdm$cohort_1a <- conceptCohort(cdm = cdm,
                                                 conceptSet = list(a = 99L),
                                                 name = "cohort_1a",
                                                 exit = "event_end_date",
                                                 useSourceFields = FALSE))
  expect_true(nrow(cdm$cohort_1a |>
                     dplyr::collect()) == 0)

  # records if we also look at source fields
  expect_no_error(cdm$cohort_1b <- conceptCohort(cdm = cdm,
                                                 conceptSet = list(a = 99L),
                                                 name = "cohort_1b",
                                                 exit = "event_end_date",
                                                 useSourceFields = TRUE))
  expect_true(nrow(cdm$cohort_1b |>
                     dplyr::collect()) > 0)


})

test_that("missing event end dates", {
  testthat::skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockCdmFromTables(tables = list("cohort" = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date("2020-01-01"),
      "cohort_end_date" = as.Date("2029-12-31")
    )))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1L,
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
      "drug_exposure_id" = 1:11L,
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1) |> as.integer(),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1) |> as.integer(),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804),
      "drug_type_concept_id" = 1L
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01"),
        "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2020-01-01")
      )
  )
  cdm <- cdm |> copyCdm()
  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L), name = "cohort"))
  startCount <- cohortCount(cohort) |>
    dplyr::pull("number_subjects")

  # with missing end dates we should have the same subject count
  # as their missing end date will have been replaced by the start date
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = 1:11L,
      "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1) |> as.integer(),
      "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1) |> as.integer(),
      "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803),
      "drug_exposure_end_date" = as.Date(NA),
      "drug_type_concept_id" = 1L
    ) |>
      dplyr::mutate(
        "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2020-01-01")
      )
  )
  expect_no_error(cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L), name = "cohort"))
  endCount <- cohortCount(cohort) |>
    dplyr::pull("number_subjects")

  expect_identical(startCount, endCount)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("test indexes - postgres", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")
  skip_if(!testIndexes)

  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                       host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                       password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    write_schema = c(schema =  Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
                     prefix = "cc_"),
    achilles_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  )

  cdm$cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 24134L), name = "cohort")
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_cohort_subject_id_cohort_start_date_idx ON public.cc_cohort USING btree (subject_id, cohort_start_date)"
  )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdm_disconnect(cdm = cdm)
})


