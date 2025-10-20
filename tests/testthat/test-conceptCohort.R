test_that("initial tests", {
  skip_on_cran()
  cdm <- omock::mockPerson(nPerson = 3)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = dplyr::tibble(
      "observation_period_id" = c(1L, 2L, 3L),
      "person_id" = c(1L, 2L, 3L),
      "observation_period_start_date" = as.Date(c("2000-01-01")),
      "observation_period_end_date" = as.Date(c("2025-01-01")),
      "period_type_concept_id" = NA_integer_
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = c(1L, 2L, 4L, 5L, 99L, 1177480L, 43157344L),
      "concept_name" = c("concept 1", "concept 2", "concept 3", "concept 4", "my_non_standard_concept", "ibuprofen", "Acetaminophen Oral Tablet [PARACETAMOL ALMUS]"),
      "domain_id" = c("drug", "drug", "observation", "condition", "drug", "drug", "drug"),
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )
  )
  # person 1 - 2 overlapping records and another
  # person 2 - 2 non-overlapping records
  # person 3 one record
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure",
    table = dplyr::tibble(
      "drug_exposure_id" = 1:15 |> as.integer(),
      "person_id" = c(1, 1, 1, 2, 2, 3, 1, 2, 3, 1, 1, 1, 1, 1, 2) |> as.integer(),
      "drug_concept_id" = c(1, 1,  1, 1, 1, 2, 3, 3, 5, 1177480L, 1177480L, 1177480L, 43157344L, 43157344L, 43157344L) |> as.integer(),
      "drug_exposure_start_date" = as.Date(c(
        "2020-07-11", "2020-10-27", "2024-02-09", "2022-01-20", "2023-01-11",
        "2022-03-11", "2020-01-01", "2020-01-04", "2005-01-04", "2000-01-01",
        "2001-08-01", "2000-03-01", "2000-11-01", "2000-02-01", "1999-11-01"
      )),
      "drug_exposure_end_date" = as.Date(c(
        "2020-11-11", "2020-10-29", "2024-02-10", "2022-01-21", "2023-01-12",
        "2022-03-12", "2020-01-10", "2020-01-14", "2005-01-04", "2000-10-02",
        "2001-08-03", "2000-03-01", "2000-11-01", "2001-02-01", "2001-02-01"
      )),
      "drug_type_concept_id" = 1L,
      "drug_source_concept_id" = c(rep(NA_integer_, 7), 99L, rep(NA_integer_, 7))
    )
  )
  # end date < start date
  # end date = NA
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "condition_occurrence",
    table = dplyr::tibble(
      "condition_occurrence_id" = 1:3 |> as.integer(),
      "person_id" = c(1, 1, 1) |> as.integer(),
      "condition_concept_id" = 5L,
      "condition_start_date" = as.Date(c("2019-09-11",
                                         "2018-10-27",
                                         "2016-02-09")),
      "condition_end_date" = as.Date(c("2019-10-11",
                                       "2017-10-29",
                                       NA)),
      "condition_type_concept_id" = 1L,
      "condition_source_concept_id" = NA_integer_
    )
  )
  # subset cohort
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "subset_cohort",
    table = dplyr::tibble(
      "cohort_definition_id" = c(1L, 2L),
      "subject_id" = c(1L, 2L),
      "cohort_start_date" = as.Date(c("2019-09-11", "2018-10-27")),
      "cohort_end_date" = as.Date(c("2019-10-11", "2018-10-29"))
    )
  )
  cdm <- cdm |> copyCdm()
  cdm$subset_cohort <- cdm$subset_cohort |> omopgenerics::newCohortTable()

  # expected errors and messages ----
  # not a cdm reference
  expect_error(conceptCohort(cdm = NULL, name = "cohort", conceptSet = NULL))

  # wrong naming
  expect_error(conceptCohort(cdm = cdm, name = NA, conceptSet = NULL))
  expect_error(conceptCohort(cdm = cdm, name = 1, conceptSet = NULL))
  expect_error(conceptCohort(cdm = cdm, name = c("ass", "asdf"), conceptSet = NULL))
  expect_message(conceptCohort(cdm = cdm, name = "aaaa", conceptSet = NULL))

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
    x <- conceptCohort(cdm = cdm, conceptSet = list(a = 3L), name = "cohort")
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
  expect_true(nrow(dplyr::collect(attr(x, "cohort_codelist"))) == 1)

  # simple exampe ----
  if(dbToTest == "duckdb CDMConnector") {
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
  if(dbToTest == "duckdb CDMConnector") {
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
      "number_records" = c(5L, 5L, 5L, 4L),
      "number_subjects" = 2L,
      "reason_id" = 1:4L,
      "reason" = c(
        "Initial qualifying events", "Record in observation", "Not missing record date",
        "Merge overlapping records"
      ),
      "excluded_records" = c(0L, 0L, 0L, 1L),
      "excluded_subjects" = 0L
    )
  )
  expect_identical(cohortCodelist(cohort, 1), omopgenerics::newCodelist(list(a = 1L)))
  cohort <- cohort |>
    dplyr::collect() |>
    dplyr::as_tibble() |>
    dplyr::arrange(subject_id, cohort_start_date)

  # Concepts from >1 table  + subset cohort ----
  # warning: concept 4 from a table not present in the cdm
  expect_warning(cohort <- conceptCohort(
    cdm = cdm,
    conceptSet = list("c" = c(1L, 2L, 3L, 4L, 5L)),
    name = "new_cohort"
  ))
  expect_equal(
    collectCohort(cohort, 1),
    dplyr::tibble(
      subject_id = as.integer(c(rep(1, 5), 2, 2, 3)),
      cohort_start_date = as.Date(c("2016-02-09", "2018-10-27", "2019-09-11", "2020-07-11", "2024-02-09", "2022-01-20", "2023-01-11", "2022-03-11")),
      cohort_end_date = as.Date(c("2016-02-09", "2018-10-27", "2019-10-11", "2020-11-11", "2024-02-10", "2022-01-21", "2023-01-12", "2022-03-12"))
    )
  )
  # Keep non used concepts:
  expect_true(all(attr(cohort, "cohort_codelist") |> dplyr::pull("concept_id") |> sort() == 1:5))
  # subset all cohorts
  expect_warning(cohort <- conceptCohort(
    cdm = cdm,
    conceptSet = list("c" = c(1L, 2L, 3L, 4L, 5L)),
    subsetCohort = "subset_cohort",
    name = "new_cohort"
  ))
  expect_equal(
    collectCohort(cohort, 1),
    dplyr::tibble(
      subject_id = as.integer(c(rep(1, 5), 2, 2)),
      cohort_start_date = as.Date(c("2016-02-09", "2018-10-27", "2019-09-11", "2020-07-11", "2024-02-09", "2022-01-20", "2023-01-11")),
      cohort_end_date = as.Date(c("2016-02-09", "2018-10-27", "2019-10-11", "2020-11-11", "2024-02-10", "2022-01-21", "2023-01-12"))
    )
  )
  # subset to cohort id 1
  expect_warning(cohort <- conceptCohort(
    cdm = cdm,
    conceptSet = list("c" = c(1L, 2L, 3L, 4L, 5L)),
    subsetCohort = "subset_cohort",
    subsetCohortId = 2,
    name = "new_cohort"
  ))
  expect_equal(
    collectCohort(cohort, 1),
    dplyr::tibble(
      subject_id = c(2L, 2L),
      cohort_start_date = as.Date(c("2022-01-20", "2023-01-11")),
      cohort_end_date = as.Date(c("2022-01-21", "2023-01-12"))
    )
  )

  # Exit at entry + use source fields ----
  # no source concept search
  expect_warning(cohort <- conceptCohort(
    cdm = cdm,
    conceptSet = list("c" = c(1L, 2L, 3L, 4L, 5L, 99L)),
    exit = "event_start_date",
    name = "new_cohort"
  ))
  expect_equal(
    collectCohort(cohort, 1),
    dplyr::tibble(
      subject_id = as.integer(c(rep(1, 6), 2, 2, 3)),
      cohort_start_date = as.Date(c("2016-02-09", "2018-10-27", "2019-09-11", "2020-07-11", "2020-10-27", "2024-02-09", "2022-01-20", "2023-01-11", "2022-03-11")),
      cohort_end_date = cohort_start_date
    )
  )
  expect_equal(
    attrition(cohort)$reason,
    c('Initial qualifying events', 'Record in observation', 'Not missing record date', 'Drop duplicate records')
  )
  expect_warning(cohort <- conceptCohort(
    cdm = cdm,
    conceptSet = list("c" = c(1L, 2L, 3L, 4L, 5L, 99L)),
    exit = "event_start_date",
    useSourceFields = TRUE,
    name = "new_cohort"
  ))
  expect_equal(
    collectCohort(cohort, 1),
    dplyr::tibble(
      subject_id = as.integer(c(rep(1, 6), 2, 2, 2, 3)),
      cohort_start_date = as.Date(c("2016-02-09", "2018-10-27", "2019-09-11", "2020-07-11", "2020-10-27", "2024-02-09", "2020-01-04", "2022-01-20", "2023-01-11", "2022-03-11")),
      cohort_end_date = cohort_start_date
    )
  )
  expect_error(conceptCohort(
    cdm = cdm,
    conceptSet = list("c" = c(1L, 2L, 3L, 4L, 5L)),
    exit = "not_and_option",
    name = "new_cohort"
  ))
  cdm$condition_occurrence <- cdm$condition_occurrence |>
    dplyr::select(!"condition_source_concept_id")
  expect_error(
    conceptCohort(
      cdm = cdm,
      conceptSet = list("c" = c(1L, 2L, 3L, 4L, 5L, 99L)),
      exit = "event_start_date",
      useSourceFields = TRUE,
      name = "new_cohort"
    )
  )

  # Indexes ----
  if (dbToTest == "postgres CDMConnector" & testIndexes) {
    expect_true(
      DBI::dbGetQuery(CDMConnector::cdmCon(cdm), paste0("SELECT * FROM pg_indexes WHERE tablename = 'coco_test_new_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX coco_test_new_cohort_subject_id_cohort_start_date_idx ON public.coco_test_new_cohort USING btree (subject_id, cohort_start_date)"
    )
  }

  # Test table argument ----
  cohort <- conceptCohort(
    cdm = cdm, conceptSet = list("a" = 5L), name = "cohort"
  )
  expect_true(nrow(cohort |> dplyr::collect()) == 3)
  cohort <- conceptCohort(
    cdm = cdm, conceptSet = list("a" = 5L), name = "cohort", table = "drug_exposure"
  )
  expect_true(nrow(cohort |> dplyr::collect()) == 1)

  # Concept set expression ----
  codes <- CodelistGenerator::codesFromConceptSet(
    cdm = cdm,
    path = here::here("extras", "concept_set"),
    type = "concept_set_expression"
  )
  cdm$cohort <- conceptCohort(
    cdm = cdm,
    conceptSet = codes,
    name = "cohort"
  )
  expect_equal(
    collectCohort(cdm$cohort, 1),
    dplyr::tibble(
      subject_id = 1L,
      cohort_start_date = as.Date(c("2000-01-01", "2001-08-01")),
      cohort_end_date = as.Date(c("2000-10-02", "2001-08-03"))
    )
  )
  expect_equal(
    collectCohort(cdm$cohort, 2),
    dplyr::tibble(
      subject_id = 1,
      cohort_start_date = as.Date(c("2000-02-01")),
      cohort_end_date = as.Date(c("2001-02-01"))
    )
  )

  # error if missing columns
  cdm$drug_exposure <- cdm$drug_exposure |>
    dplyr::select(!"drug_source_concept_id")
  expect_no_error(conceptCohort(cdm, conceptSet = list("a" = 5L),
                name = "cohort",
                table = "drug_exposure"))
  expect_error(conceptCohort(cdm, conceptSet = list("a" = 5L),
                useSourceFields = TRUE,
                name = "cohort",
                table = "drug_exposure"))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  dropCreatedTables(cdm = cdm)
})


test_that("out of observation", {
  skip_on_cran()

  person <- dplyr::tibble(
    person_id = 1:4,
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L),
    year_of_birth = c(1997L, 1963L, 1986L, 1978L),
    month_of_birth = c(8L, 1L, 3L, 11L),
    day_of_birth = c(22L, 27L, 10L, 8L),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:4L,
    person_id = 1:4L,
    observation_period_start_date = as.Date(c("2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09")),
    observation_period_end_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31")),
    period_type_concept_id = NA_integer_
  )

  cdm <- omock::mockCdmReference() |>
    omopgenerics::insertTable(name = "observation_period", table = obs) |>
    omopgenerics::insertTable(name = "person", table = person) |>
    omock::mockVocabularyTables(concept = dplyr::tibble(
      "concept_id" = c(1L, 2L),
      "concept_name" = c("my concept 1", "my concept 2"),
      "domain_id" = "Drug",
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )) |>
    omopgenerics::insertTable(name = "drug_exposure", table = dplyr::tibble(
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
      )) |>
    copyCdm()

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
                    dplyr::pull("subject_id") %in% c(1, 4)))
  expect_true(all(cdm$cohort1 |>
                dplyr::filter(cohort_definition_id == 1) |>
                dplyr::pull("cohort_start_date") |> sort() == as.Date(c("2010-01-01", "2011-03-07"))))
  expect_true(all(cdm$cohort1 |>
                    dplyr::filter(cohort_definition_id == 1) |>
                    dplyr::pull("cohort_end_date") |> sort() == as.Date(c("2011-03-07", "2012-03-11"))))
  expect_true(all(cdm$cohort1 |>
                    dplyr::filter(cohort_definition_id == 2) |>
                    dplyr::pull("cohort_start_date") |> sort() == as.Date(c("2009-12-22", "2012-01-21"))))
  expect_true(all(cdm$cohort1 |>
                    dplyr::filter(cohort_definition_id == 2) |>
                    dplyr::pull("cohort_end_date") |> sort() == as.Date(c("2009-12-22", "2013-06-29"))))

  expect_true(all(omopgenerics::settings(cdm$cohort1)$cohort_name == c("a", "b")))
  expect_true(cohortCodelist(cdm$cohort1, 1)$a == 1)
  expect_true(cohortCodelist(cdm$cohort1, 2)$b == 2)

  dropCreatedTables(cdm = cdm)

  # event starts in, ends out (subject 1)
  # event starts out, end in (subject 3)
  # no concept 2
  person <- dplyr::tibble(
    person_id = 1:4L,
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L),
    year_of_birth = c(1997L, 1963L, 1986L, 1978L),
    month_of_birth = c(8L, 1L, 3L, 11L),
    day_of_birth = c(22L, 27L, 10L, 8L),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:4L,
    person_id = 1:4L,
    observation_period_start_date = as.Date(c("2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09")),
    observation_period_end_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31")),
    period_type_concept_id = NA_integer_
  )

  cdm <- omock::mockCdmReference() |>
    omopgenerics::insertTable(name = "observation_period", table = obs) |>
    omopgenerics::insertTable(name = "person", table = person) |>
    omock::mockVocabularyTables(concept = dplyr::tibble(
      "concept_id" = c(1L, 2L),
      "concept_name" = c("my concept 1", "my concept 2"),
      "domain_id" = "Drug",
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )) |>
    omopgenerics::insertTable(name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = 1:4L,
      "person_id" = c(1L, 3L, 4L, 2L),
      "drug_concept_id" = 1L,
      "drug_exposure_start_date" = as.Date(c("2004-01-01", "2014-01-01", "2001-01-01", "2000-01-01")),
      "drug_exposure_end_date" = as.Date(c("2015-01-01", "2015-05-01", "2002-01-01", "2000-02-02")),
      "drug_type_concept_id" = 1L
    )) |>
    copyCdm()

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

  dropCreatedTables(cdm = cdm)

  # start date > end date (subject 1)
  # overlapping and out of observation (subject 2)
  # out of observation (subject 3)
  # overlapping (subject 4)
  person <- dplyr::tibble(
    person_id = 1:4,
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L),
    year_of_birth = c(1997L, 1963L, 1986L, 1978L),
    month_of_birth = c(8L, 1L, 3L, 11L),
    day_of_birth = c(22L, 27L, 10L, 8L),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date(c("2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09")),
    observation_period_end_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31")),
    period_type_concept_id = NA_integer_
  )

  cdm <- omock::mockCdmReference() |>
    omopgenerics::insertTable(name = "observation_period", table = obs) |>
    omopgenerics::insertTable(name = "person", table = person) |>
    omock::mockVocabularyTables(concept = dplyr::tibble(
      "concept_id" = c(1L, 2L),
      "concept_name" = c("my concept 1", "my concept 2"),
      "domain_id" = "Drug",
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )) |>
    omopgenerics::insertTable(name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = 1:6L,
      "person_id" = c(1, 2, 2, 3, 4, 4) |> as.integer(),
      "drug_concept_id" = c(1, 1, 1, 1, 2, 2) |> as.integer(),
      "drug_exposure_start_date" = as.Date(c("2004-01-01", "2014-01-01", "2015-04-01", "2000-01-01", "2000-01-01", "1999-01-01")),
      "drug_exposure_end_date" = as.Date(c("2003-01-01", "2015-05-01", "2015-07-01", "2000-02-02", "2000-01-01", "2001-01-01")),
      "drug_type_concept_id" = 1L
    )) |>
    copyCdm()

  # empty cohort
  cdm$cohort3 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L), name = "cohort3")
  expect_true(all(colnames(cdm$cohort3) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))
  expect_true(all(c("cohort_table", "cdm_table") %in% class(cdm$cohort3)))
  expect_true(cdm$cohort3 |> dplyr::tally() |> dplyr::pull("n") == 1)
  expect_true(cohortCodelist(cdm$cohort3, 1)$a == 1)

  # empty cohort
  cdm$cohort4 <- conceptCohort(cdm = cdm, conceptSet = list(a = 1L, b = 2L), name = "cohort4")
  expect_true(all(c("cohort_table", "cdm_table") %in% class(cdm$cohort4)))
  expect_true(cdm$cohort4 |> dplyr::tally() |> dplyr::pull("n") == 2)
  expect_true(cohortCodelist(cdm$cohort4, 1)$a == 1)
  expect_true(cohortCodelist(cdm$cohort4, 2)$b == 2)
  expect_true(all(cdm$cohort4 |> dplyr::pull("subject_id") |> sort() == c(1, 4)))
  expect_true(all(cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() == as.Date(c("1999-01-01", "2004-01-01"))))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("overlap option", {
  skip_on_cran()
  cdm <- omock::mockPerson(nPerson = 1)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = dplyr::tibble(
      "observation_period_id" = c(1L, 2L),
      "person_id" = c(1L, 2L),
      "observation_period_start_date" = as.Date(c("2020-01-01")),
      "observation_period_end_date" = as.Date(c("2020-01-30")),
      "period_type_concept_id" = NA_integer_
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = c(1L, 2L, 3L, 4L, 5L, 6L),
      "person_id" = c(1L, 1L, 1L, 1L, 2L, 1L),
      "drug_concept_id" = c(1L, 1L, 1L, 1L, 1L, 2L),
      # 3 overlapping records
      "drug_exposure_start_date" = as.Date(c("2020-01-01",
                                             "2020-01-04",
                                             "2020-01-05",
                                             "2020-01-20",
                                             "2020-01-01",
                                             "2020-01-02")),
      "drug_exposure_end_date" = as.Date(c("2020-01-06",
                                           "2020-01-08",
                                           "2020-01-06",
                                           "2020-01-21",
                                           "2020-01-21",
                                           "2020-01-03")),
      "drug_type_concept_id" = 1L
    ) )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = c(1L, 2L),
      "concept_name" = c("concept 1", "concept 2"),
      "domain_id" = "drug",
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )
  )

  cdm <- cdm |> copyCdm()

  expect_no_error(cdm$cohort_1 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L,
                                                                  b = 2L),
                                                name = "cohort_1",
                                                exit = "event_end_date",
                                                overlap = "merge"))
  expect_true(nrow(cdm$cohort_1 |>
                     dplyr::collect()) == 4)
  expect_true(all(sort(cdm$cohort_1 |>
                         dplyr::pull("cohort_start_date")) ==
                    as.Date(c("2020-01-01",
                              "2020-01-01",
                              "2020-01-02",
                              "2020-01-20"))))
  expect_true(all(sort(cdm$cohort_1 |>
                         dplyr::pull("cohort_end_date")) ==
                    as.Date(c("2020-01-03",
                              "2020-01-08",
                              "2020-01-21",
                              "2020-01-21"))))
  expect_true(
    attrition(cdm$cohort_1) |> dplyr::filter(reason_id == 4 & cohort_definition_id == 1) |> dplyr::pull(excluded_records) == 2
  )

  expect_no_error(cdm$cohort_2 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L,
                                                                  b = 2L),
                                                name = "cohort_2",
                                                exit = "event_end_date",
                                                overlap = "extend"))
  expect_true(all(sort(cdm$cohort_2 |>
                         dplyr::pull("cohort_start_date")) ==
                    as.Date(c("2020-01-01",
                              "2020-01-01",
                              "2020-01-02",
                              "2020-01-20"))))
  expect_true(all(sort(cdm$cohort_2 |>
                         dplyr::pull("cohort_end_date")) ==
                    as.Date(c("2020-01-03",
                              "2020-01-11", # now 11 instead of 8 (3 days overlap)
                              "2020-01-21",
                              "2020-01-21"))))


  # only overlapping records
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = c(1L, 2L),
      "person_id" = c(1L, 1L),
      "drug_concept_id" = c(1L, 1L),
      "drug_exposure_start_date" = as.Date(c("2020-01-01",
                                             "2020-01-04")),
      "drug_exposure_end_date" = as.Date(c("2020-01-06",
                                           "2020-01-08")),
      "drug_type_concept_id" = 1L
    ))

  expect_no_error(cdm$cohort_3 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L,
                                                                  b = 2L),
                                                name = "cohort_3",
                                                exit = "event_end_date",
                                                overlap = "extend"))
  expect_true(nrow(cdm$cohort_3 |>
                     dplyr::collect()) == 1)
  expect_true(all(sort(cdm$cohort_3 |>
                         dplyr::pull("cohort_start_date")) ==
                    as.Date(c("2020-01-01"))))
  expect_true(all(sort(cdm$cohort_3 |>
                         dplyr::pull("cohort_end_date")) ==
                    as.Date(c("2020-01-10"))))

  # no overlapping records
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = c(1L, 2L),
      "person_id" = c(1L, 1L),
      "drug_concept_id" = c(1L, 1L),
      "drug_exposure_start_date" = as.Date(c("2020-01-01",
                                             "2020-01-05")),
      "drug_exposure_end_date" = as.Date(c("2020-01-03",
                                           "2020-01-10")),
      "drug_type_concept_id" = 1L
    ))

  expect_no_error(cdm$cohort_4 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L,
                                                                  b = 2L),
                                                name = "cohort_4",
                                                exit = "event_end_date",
                                                overlap = "extend"))
  expect_true(nrow(cdm$cohort_4 |>
                     dplyr::collect()) == 2)
  expect_true(all(sort(cdm$cohort_4 |>
                         dplyr::pull("cohort_start_date")) ==
                    as.Date(c("2020-01-01", "2020-01-05"))))
  expect_true(all(sort(cdm$cohort_4 |>
                         dplyr::pull("cohort_end_date")) ==
                    as.Date(c("2020-01-03", "2020-01-10"))))

  # wrong input
  expect_error(cdm$cohort_5 <- conceptCohort(cdm = cdm,
                                             conceptSet = list(a = 1L),
                                             name = "cohort_5",
                                             exit = "event_end_date",
                                             overlap = "another"))



  # When extending, overlapping with the following record
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = c(1L, 2L, 3L),
      "person_id" = c(1L, 1L, 1L),
      "drug_concept_id" = c(1L, 1L, 1L),
      "drug_exposure_start_date" = as.Date(c("2020-01-01", "2020-01-05",  "2020-01-13")),
      "drug_exposure_end_date" = as.Date(c("2020-01-08", "2020-01-11",  "2020-01-15")),
      "drug_type_concept_id" = 1L
    ))
  expect_no_error(cdm$cohort_6 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L),
                                                name = "cohort_6",
                                                exit = "event_end_date",
                                                overlap = "extend"))

  expect_true((cdm$cohort_6 |> dplyr::pull("cohort_start_date")) == as.Date("2020-01-01"))
  expect_true((cdm$cohort_6 |> dplyr::pull("cohort_end_date")) == as.Date("2020-01-16"))

  # Check if "extend" exceeds observation_period_end_date (when only one observation period)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure", table = dplyr::tibble(
      "drug_exposure_id" = c(1L, 2L),
      "person_id" = c(1L, 1L),
      "drug_concept_id" = c(1L, 1L),
      "drug_exposure_start_date" = as.Date(c("2020-01-15", "2020-01-20")),
      "drug_exposure_end_date" = as.Date(c("2020-01-28", "2020-01-28")),
      "drug_type_concept_id" = 1L
    ))

  expect_no_error(cdm$cohort_7 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L),
                                                name = "cohort_7",
                                                exit = "event_end_date",
                                                overlap = "extend"))
  expect_true((cdm$cohort_7 |> dplyr::pull("cohort_start_date")) == as.Date("2020-01-15"))
  expect_true((cdm$cohort_7 |> dplyr::pull("cohort_end_date")) == as.Date("2020-01-30"))

  # Check if "extend" exceeds observation_period_end_date (when more than one observation period)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = dplyr::tibble(
      "observation_period_id" = c(1L, 1L, 2L),
      "person_id" = c(1L, 1L, 2L),
      "observation_period_start_date" = as.Date(c("2020-01-01","2020-03-01","2020-01-01")),
      "observation_period_end_date" = as.Date(c("2020-01-30","2020-03-30","2020-01-30")),
      "period_type_concept_id" = c(NA_integer_,NA_integer_,NA_integer_)
    ))

  expect_no_error(cdm$cohort_8 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L),
                                                name = "cohort_8",
                                                exit = "event_end_date",
                                                overlap = "extend"))
  expect_true((cdm$cohort_8 |> dplyr::pull("cohort_end_date")) == as.Date("2020-01-30"))

  # check attrition when >1 observation period
  expect_true(all(unique(attrition(cdm$cohort_8)$excluded_records) == c(0, 1)))

  dropCreatedTables(cdm = cdm)
})

test_that("useRecordsBeforeObservation TRUE", {
  skip_on_cran()
  cdm <- omock::mockPerson(nPerson = 3)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = dplyr::tibble(
      "observation_period_id" = c(1L, 2L, 3L),
      "person_id" = c(1L, 1L, 2L),
      "observation_period_start_date" = as.Date(c(
        "2000-02-01", "2000-10-01", "2000-01-01"
      )),
      "observation_period_end_date" = as.Date(c(
        "2000-05-01", "2000-12-01", "2000-12-01"
      )),
      "period_type_concept_id" = NA_integer_
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1L,
      "concept_name" = "concept 1",
      "domain_id" = "drug",
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )
  )
  # person 1 - 2 record in obs, 2 records out with one eding out and the other in aother observation period
  # person 2 - one record all out starting before and ending after
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure",
    table = dplyr::tibble(
      "drug_exposure_id" = 1:6 |> as.integer(),
      "person_id" = c(1, 1, 1, 1, 1, 2) |> as.integer(),
      "drug_concept_id" = 1L,
      "drug_exposure_start_date" = as.Date(c(
        "2000-01-01", "2001-08-01", "2000-03-01", "2000-11-01", "2000-02-01", "1999-11-01"
      )),
      "drug_exposure_end_date" = as.Date(c(
        "2000-10-02", "2001-08-03", "2000-03-01", "2000-11-01", "2001-02-01", "2001-02-01"
      )),
      "drug_type_concept_id" = 1L
    )
  )

  cdm <- cdm |> copyCdm()

  cdm$cohort <- conceptCohort(cdm, list(a = 1L), name = "cohort", useRecordsBeforeObservation = TRUE)
  expect_equal(
    dplyr::tibble(
      subject_id = c(1L, 1L, 2L),
      cohort_start_date = as.Date(c("2000-02-01", "2000-11-01", "2000-01-01")),
      cohort_end_date = as.Date(c("2000-05-01", "2000-11-01", "2000-12-01"))
    ),
    collectCohort(cdm$cohort, 1)
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)

  cdm <- omock::mockPerson(nPerson = 3)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = dplyr::tibble(
      "observation_period_id" = 1:6L,
      "person_id" = c(1L, 1L, 1L, 1L, 2L, 2L),
      "observation_period_start_date" = as.Date(c(
        "2000-02-01", "2000-10-01", "2001-01-01", "2002-01-01", "2001-01-01", "2002-01-01"
      )),
      "observation_period_end_date" = as.Date(c(
        "2000-05-01", "2000-12-01", "2001-12-01", "2002-12-01", "2001-12-01", "2002-12-01"
      )),
      "period_type_concept_id" = NA_integer_
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = 1L,
      "concept_name" = "concept 1",
      "domain_id" = "drug",
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )
  )
  # 1st: start in one obs ends in another
  # 2nd: statrt out and end out one observation period further
  # 3rd: start and end out
  # 4th: start and end in (to be merged with 3rd)
  # 5th: start out end out in subsequent obs
  # 6th: starts out and ends out of last obs
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure",
    table = dplyr::tibble(
      "drug_exposure_id" = 1:6 |> as.integer(),
      "person_id" = c(1, 1, 1, 1, 2, 2) |> as.integer(),
      "drug_concept_id" = 1L,
      "drug_exposure_start_date" = as.Date(c(
        "2000-04-01", "2000-12-03", "2001-12-11", "2002-01-01", "2000-02-01", "2004-02-01"
      )),
      "drug_exposure_end_date" = as.Date(c(
        "2000-11-01", "2001-08-03", "2001-12-12", "2002-11-01", "2003-02-01", "2004-02-01"
      )),
      "drug_type_concept_id" = 1L
    )
  ) |>
    copyCdm()

  cdm$cohort <- conceptCohort(cdm, list(a = 1L, b = 1L), name = "cohort", useRecordsBeforeObservation = TRUE)

  expect_equal(
    dplyr::tibble(
      subject_id = c(1L, 1L, 1L, 2L),
      cohort_start_date = as.Date(c("2000-04-01", "2001-01-01", "2002-01-01", "2001-01-01")),
      cohort_end_date = as.Date(c("2000-05-01", "2001-08-03", "2002-11-01", "2001-12-01"))
    ),
    collectCohort(cdm$cohort, 1)
  )
  expect_equal(
    collectCohort(cdm$cohort, 1),
    collectCohort(cdm$cohort, 2)
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})
