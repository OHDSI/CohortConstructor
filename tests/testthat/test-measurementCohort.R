test_that("mearurementCohorts works", {
  skip_on_cran()
  cdm <- omock::mockPerson(nPerson = 5)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "cohort_1", table =dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = c(1L, 1L, 2L, 3L, 4L),
      cohort_start_date = as.Date(c(
        "2001-04-03", "2002-05-07", "1999-07-26", "2015-02-19", "1990-09-07"
      )),
      cohort_end_date = as.Date(c(
        "2002-05-06", "2005-11-07", "2002-09-17", "2015-06-27", "2008-02-19"
      ))
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "cohort_2", table = dplyr::tibble(
      cohort_definition_id = c(rep(1L, 5), rep(2L, 5)),
      subject_id = c(1L, 1L, 2L, 3L, 4L, 1L, 1L, 2L, 3L, 4L),
      cohort_start_date = as.Date(c(
        # Cohort 1
        "2001-04-03", "2002-05-07", "1999-07-26", "2015-02-19", "1990-09-07",
        # Cohort 2
        "2004-04-07", "2004-05-03", "2000-02-27", "2015-03-10", "1995-05-15"
      )),
      cohort_end_date = as.Date(c(
        # Cohort 1
        "2002-05-06", "2005-11-07", "2002-09-17", "2015-06-27", "2008-02-19",
        # Cohort 2
        "2004-05-02", "2005-05-25", "2001-05-18", "2015-06-15", "1995-11-14"
      ))
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = dplyr::tibble(
      observation_period_id = 1:5L,
      person_id = 1:5L,
      observation_period_start_date = as.Date(c(
        "2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09", "2012-03-18"
      )),
      observation_period_end_date = as.Date(c(
        "2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31", "2013-02-10"
      )),
      period_type_concept_id = NA_integer_
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "person", table = dplyr::tibble(
      person_id = 1:5,
      gender_concept_id = c(8507L, 8507L, 8507L, 8532L, 8507L),
      year_of_birth = c(1997L, 1963L, 1986L, 1978L, 1973L),
      month_of_birth = c(8L, 1L, 3L, 11L, 3L),
      day_of_birth = c(22L, 27L, 10L, 8L, 2L),
      race_concept_id = NA_integer_,
      ethnicity_concept_id = NA_integer_
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "measurement", table = dplyr::tibble(
      measurement_id = 1:7L,
      person_id = as.integer(c(1, 1, 2, 3, 3, 1, 1)),
      measurement_concept_id = c(4326744, 4298393, 4298393, 45770407, 45770407, 123456, 123456) |> as.integer(),
      measurement_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20", "1900-01-01", "2050-01-01")),
      measurement_type_concept_id = NA_integer_,
      value_as_number = c(100, 125, NA, NA, NA, NA, NA),
      value_as_concept_id = c(0, 0, 0, 4124457, 999999, 0, 0) |> as.integer(),
      unit_concept_id = c(8876, 8876, 0, 0, 0, 0, 0) |> as.integer(),
      measurement_source_concept_id = c(99L, 99L, rep(NA_integer_, 5))
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      concept_id = c(4326744, 4298393, 45770407, 8876, 4124457, 999999, 123456) |> as.integer(),
      concept_name = c("Blood pressure", "Systemic blood pressure",
                       "Baseline blood pressure", "millimeter mercury column",
                       "Normal range", "Normal", "outObs"),
      domain_id = "Measurement",
      vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "UCUM", "SNOMED", "SNOMED", "hi"),
      standard_concept = "S",
      concept_class_id = c("Observable Entity", "Observable Entity",
                           "Observable Entity", "Unit", "Qualifier Value",
                           "Qualifier Value", "hi"),
      concept_code = NA_character_,
      valid_start_date = as.Date(NA),
      valid_end_date = as.Date(NA),
      invalid_reason = NA_character_
    ))
  cdm <- cdm |> copyCdm()

  if (dbToTest == "duckdb CDMConnector") {
    startTempTables <- countDuckdbTempTables(con = CDMConnector::cdmCon(cdm))
    startPermanentTables <- countDuckdbPermanentTables(con = CDMConnector::cdmCon(cdm))
  }

  # simple example
  cdm$cohort <- measurementCohort(
    cdm = cdm,
    name = "cohort",
    conceptSet = list("normal_blood_pressure" = c(4326744L, 4298393L, 45770407L)),
    valueAsConcept = list("normal_blood_pressure" = c(4124457)),
    valueAsNumber = list("normal_blood_pressure" = list("8876" = c(70L, 120L))),
    table = "measurement"
  )

  expect_true(
    all(colnames(attr(cdm$cohort, "cohort_codelist")) == c("cohort_definition_id", "codelist_name", "concept_id", "codelist_type"))
  )

  if (dbToTest == "duckdb CDMConnector") {
    endTempTables <- countDuckdbTempTables(con = CDMConnector::cdmCon(cdm))
    endPermanentTables <- countDuckdbPermanentTables(con = CDMConnector::cdmCon(cdm))
    # we should have only added 4 permanent tables (the new cohort table and
    # three tables with settings, attrition, and codelist)
    # no temp tables will have been created
    expect_true(startTempTables == endTempTables)
    expect_true(startPermanentTables + 4 == endPermanentTables
    )
  }

  expect_identical(collectCohort(cdm$cohort, 1), dplyr::tibble(
    subject_id = c(1L, 3L),
    cohort_start_date = as.Date(c("2000-07-01", "2015-02-19")),
    cohort_end_date = as.Date(c("2000-07-01", "2015-02-19"))
  ))
  expect_identical(
    cdm$cohort |> attrition() |> dplyr::as_tibble(),
    dplyr::tibble(
      "cohort_definition_id" = 1L,
      "number_records" = rep(2L, 4),
      "number_subjects" = rep(2L, 4),
      "reason_id" = 1:4L,
      "reason" = c(
        "Initial qualifying events",
        "Record in observation",
        "Not missing record date",
        "Drop duplicate records"
      ),
      "excluded_records" = c(0L, 0L, 0L, 0L),
      "excluded_subjects" = c(0L, 0L, 0L, 0L),
    )
  )
  expect_true(settings(cdm$cohort)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id |> sort() == c(4298393, 4326744, 45770407)))
  expect_identical(settings(cdm$cohort), dplyr::tibble(
    "cohort_definition_id" = 1L, "cohort_name" = "normal_blood_pressure",
    "cdm_version" = attr(cdm, "cdm_version"), "vocabulary_version" = "mock"
  ))

  # non valid concept ----
  cdm$cohort3 <- measurementCohort(
    cdm = cdm,
    name = "cohort3",
    conceptSet = list("normal_blood_pressure" = c(4326744L, 4298393L, 45770407L, 12345L)),
    valueAsConcept = list("normal_blood_pressure" = c(4124457)),
    valueAsNumber = list("normal_blood_pressure" = list("8876" = c(70, 120))),
    table = "measurement"
  )
  expect_identical(collectCohort(cdm$cohort3, 1), dplyr::tibble(
    subject_id = c(1L, 3L),
    cohort_start_date = as.Date(c("2000-07-01", "2015-02-19")),
    cohort_end_date = as.Date(c("2000-07-01", "2015-02-19"))
  ))
  expect_true(settings(cdm$cohort3)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort3, "cohort_codelist") |> dplyr::collect()
  expect_true(all(c(4298393, 4326744, 45770407) %in% codes$concept_id))

  # no subjects with concept ----
  cdm$cohort4 <- measurementCohort(
    cdm = cdm,
    name = "cohort4",
    conceptSet = list("normal_blood_pressure" = c(4L)),
    valueAsConcept = list("normal_blood_pressure" = c(4124457)),
    table = "measurement"
  )
  expect_true(settings(cdm$cohort4)$cohort_name == "normal_blood_pressure")
  expect_true(cdm$cohort4 |> dplyr::tally() |> dplyr::pull() == 0)

  # > 1 number values ----
  cdm$cohort5 <- measurementCohort(
    cdm = cdm,
    name = "cohort5",
    conceptSet = list("blood_pressure" = c(4326744L, 4298393L, 45770407L)),
    valueAsNumber = list("high_blood_pressure" = list("8876" = c(70, 120), "908" = c(800, 900))),
    table = "measurement"
  )
  expect_identical(collectCohort(cdm$cohort5, 1), dplyr::tibble(
    subject_id = 1L,
    cohort_start_date = as.Date(c("2000-07-01")),
    cohort_end_date = as.Date(c("2000-07-01"))
  ))
  expect_true(settings(cdm$cohort5)$cohort_name == "high_blood_pressure")
  codes <- attr(cdm$cohort5, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(4298393, 4326744, 45770407)))

  # concept values ----
  cdm$cohort6 <- measurementCohort(
    cdm = cdm,
    name = "cohort6",
    conceptSet = list("blood_pressure" = c(4326744L, 4298393L, 45770407L)),
    valueAsConcept = list("normal_blood_pressure" = c(4124457, 999999, 12345)),
    table = "measurement"
  )
  expect_identical(collectCohort(cdm$cohort6, 1), dplyr::tibble(
    subject_id = c(3L, 3L),
    cohort_start_date = as.Date(c("2015-02-19", "2015-02-20")),
    cohort_end_date = as.Date(c("2015-02-19", "2015-02-20"))
  ))
  expect_true(settings(cdm$cohort6)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort6, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(4298393, 4326744, 45770407)))

  # more than 1 filter ----
  cdm$cohort7 <- measurementCohort(
    cdm = cdm,
    name = "cohort7",
    conceptSet = list("blood_pressure" = c(4326744L, 4298393L, 45770407L)),
    valueAsConcept = list("c1" = c(4124457, 999999, 12345)),
    valueAsNumber = list("c2" = list("8876" = c(70, 120), "908" = c(800, 900))),
    table = "measurement"
  )
  expect_identical(collectCohort(cdm$cohort7, 2), dplyr::tibble(
    subject_id = c(1L),
    cohort_start_date = as.Date(c("2000-07-01")),
    cohort_end_date = as.Date(c("2000-07-01"))
  ))
  expect_identical(collectCohort(cdm$cohort7, 1), dplyr::tibble(
    subject_id = as.integer(c(3, 3)),
    cohort_start_date = as.Date(c("2015-02-19","2015-02-20")),
    cohort_end_date = as.Date(c("2015-02-19", "2015-02-20"))
  ))
  expect_identical(
    attrition(cdm$cohort7) |> dplyr::as_tibble(),
    dplyr::tibble(
      "cohort_definition_id" = c(rep(1L, 4), rep(2L, 4)),
      "number_records" = c(rep(2L, 4), rep(1L, 4)),
      "number_subjects" = c(rep(1L, 4), rep(1L, 4)),
      "reason_id" = rep(1:4L, 2),
      "reason" = c(
        "Initial qualifying events",
        "Record in observation",
        "Not missing record date",
        "Drop duplicate records",
        "Initial qualifying events",
        "Record in observation",
        "Not missing record date",
        "Drop duplicate records"
      ),
      "excluded_records" = 0L,
      "excluded_subjects" = 0L
    )
  )
  expect_true(all(all(settings(cdm$cohort7)$cohort_name == c("c1", "c2"))))
  codes <- attr(cdm$cohort7, "cohort_codelist") |> dplyr::collect()
  expect_true(codes$codelist_name |> unique() == "blood_pressure")
  expect_true(all(codes$cohort_definition_id |> unique() == 1:2))

  cdm$cohort7 <- measurementCohort(
    cdm = cdm,
    name = "cohort7",
    conceptSet = list("blood_pressure" = c(4326744L, 4298393L, 45770407L)),
    valueAsConcept = list("c1" = c(4124457, 999999, 12345), "c2" = c(4124457, 999999, 12345)),
    valueAsNumber = list("c2" = list("8876" = c(70, 120), "908" = c(800, 900)), "c3" = list(c(0,100))),
    table = "measurement"
  )
  expect_equal(settings(cdm$cohort7)$cohort_name, paste0("c", 1:3))
  expect_equal(
    collectCohort(cdm$cohort7, 1),
    dplyr::tibble(
      subject_id = 3L,
      cohort_start_date = as.Date(c("2015-02-19", "2015-02-20")),
      cohort_end_date = cohort_start_date
    )
  )
  expect_equal(
    collectCohort(cdm$cohort7, 2),
    dplyr::tibble(
      subject_id = c(1L, 3L,3L),
      cohort_start_date = as.Date(c("2000-07-01", "2015-02-19", "2015-02-20")),
      cohort_end_date = cohort_start_date
    )
  )
  expect_equal(
    collectCohort(cdm$cohort7, 3),
    dplyr::tibble(
      subject_id = c(1L),
      cohort_start_date = as.Date(c("2000-07-01")),
      cohort_end_date = cohort_start_date
    )
  )
  # out of obs ----
  cdm$cohort8 <- measurementCohort(
    cdm = cdm,
    name = "cohort8",
    conceptSet = list("c1" = c(123456L)),
    valueAsConcept = list("c1" = c(0)),
    table = "measurement"
  )
  expect_true(all(colnames(cdm$cohort8) ==
                    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")))
  expect_true(cdm$cohort8 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(settings(cdm$cohort8)$cohort_name == "c1")
  codes <- attr(cdm$cohort8, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(123456)))

  # Indexes ----
  if (dbToTest == "postgres CDMConnector" & testIndexes) {
    expect_true(
      DBI::dbGetQuery(CDMConnector::cdmCon(cdm), paste0("SELECT * FROM pg_indexes WHERE tablename = 'coco_test_cohort8';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX coco_test_cohort8_subject_id_cohort_start_date_idx ON public.coco_test_cohort8 USING btree (subject_id, cohort_start_date)"
    )
  }

  # empty cohort but non empty codelist ----
  cdm$cohort9 <- measurementCohort(
    cdm = cdm,
    name = "cohort9",
    conceptSet = list("c1" = c(1234567L)),
    valueAsConcept = list("c1" = c(1111)),
    table = "measurement"
  )
  expect_true(cdm$cohort9 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(settings(cdm$cohort9)$cohort_name == "c1")
  expect_identical(colnames(settings(cdm$cohort9)) |> sort(), c("cdm_version", "cohort_definition_id", "cohort_name", "vocabulary_version"))
  codes <- attr(cdm$cohort9, "cohort_codelist") |> dplyr::collect()
  expect_true(nrow(codes) == 1)

  # empty cohort and empty codelist ----
  cdm$cohort10 <- measurementCohort(
    cdm = cdm,
    name = "cohort10",
    conceptSet = list(),
    valueAsConcept = list("c1" = c(1111)),
    table = "measurement"
  )
  expect_true(cdm$cohort10 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(cdm$cohort10 |> attrition() |> nrow() == 0)
  expect_true(cdm$cohort10 |> settings() |> nrow() == 1)
  expect_identical(colnames(settings(cdm$cohort10)) |> sort(), c("cdm_version", "cohort_definition_id", "cohort_name", "vocabulary_version"))

  # Source concepts + subset cohort ----
  cdm$cohort11 <- measurementCohort(
    cdm = cdm,
    name = "cohort11",
    conceptSet = list("source" = 99L),
    valueAsConcept = list("c1" = 0),
    table = "measurement"
  )
  expect_true(nrow(collectCohort(cdm$cohort11, 1)) == 0)
  cdm$cohort12 <- measurementCohort(
    cdm = cdm,
    name = "cohort12",
    conceptSet = list("source" = 99L),
    valueAsConcept = list("c1" = 0),
    table = "measurement",
    useSourceFields = TRUE
  )
  expect_true(nrow(collectCohort(cdm$cohort12, 1)) == 2)
  cdm$cohort13 <- measurementCohort(
    cdm = cdm,
    name = "cohort13",
    conceptSet = list("source" = 99L),
    valueAsConcept = list("c1" = 0),
    table = "measurement",
    useSourceFields = TRUE,
    subsetCohort = "cohort7",
    subsetCohortId = 1
  )
  expect_true(nrow(collectCohort(cdm$cohort13, 1)) == 0)

  # Expected behaviour ----
  # simple example
  expect_error(
    measurementCohort(
      cdm = cdm,
      name = "cohort",
      conceptSet = list(c(4326744L, 4298393L, 45770407L)),
      valueAsConcept = c(4124457L),
      valueAsNumber = list("8876" = c(70L, 120L)),
      table = "measurement"
    )
  )
  expect_error(
    measurementCohort(
      cdm = cdm,
      name = "cohort",
      conceptSet = list("name " = c(4326744L, 4298393L, 45770407L)),
      valueAsConcept = c(4124457),
      valueAsNumber = list("8876" = c(700, 120)),
      table = "measurement"
    )
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("useRecordsBeforeObservation TRUE + edge cases", {
  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = dplyr::tibble(
        person_id = 1:3L,
        gender_concept_id = 8532L,
        year_of_birth = 1950L,
        race_concept_id = 0L,
        ethnicity_concept_id = 0L
      ),
      observation_period = dplyr::tibble(
        "observation_period_id" = c(1L, 2L, 3L),
        "person_id" = c(1L, 1L, 2L),
        "observation_period_start_date" = as.Date(c(
          "2000-02-01", "2000-10-01", "2000-01-01"
        )),
        "observation_period_end_date" = as.Date(c(
          "2000-05-01", "2000-12-01", "2000-12-01"
        )),
        "period_type_concept_id" = NA_integer_
      )
    ),
    cdmName = "mock"
  ) |>
    omock::mockVocabularyTables(concept = dplyr::tibble(
      "concept_id" = 1L,
      "concept_name" = "concept 1",
      "domain_id" = "measurement",
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )) |>
    # person 1 - out before, out after, in, in
    # person 2 - out before
    omopgenerics::insertTable(
      name = "measurement",
      table = dplyr::tibble(
        "measurement_id" = 1:6 |> as.integer(),
        "person_id" = c(1, 1, 1, 1, 1, 2) |> as.integer(),
        "measurement_concept_id" = 1L,
        "measurement_date" = as.Date(c(
          "2000-01-01", "2001-08-01", "2000-03-01", "2000-11-01", "2000-02-01", "1999-11-01"
        )),
        "measurement_type_concept_id" = 1L,
        "value_as_number" = 1,
        "value_as_concept_id" = as.integer(1),
        "unit_concept_id" = as.integer(1)
      )
    ) |>
    copyCdm()

  # Record outside observation ----
  cdm$cohort <- measurementCohort(cdm, conceptSet = list(a = 1L), valueAsConcept = list("a" = 1L), name = "cohort", useRecordsBeforeObservation = TRUE)
  expect_equal(
    dplyr::tibble(
      subject_id = c(1L, 1L, 1L, 2L),
      cohort_start_date = as.Date(c("2000-02-01", "2000-03-01", "2000-11-01", "2000-01-01")),
      cohort_end_date = as.Date(c("2000-02-01", "2000-03-01", "2000-11-01", "2000-01-01"))
    ),
    collectCohort(cdm$cohort, 1)
  )

  # No record outside observation
  cdm$cohort <- measurementCohort(cdm, list(a = 2L), valueAsConcept = list("a" = 1L), name = "cohort")
  attrition <- attrition(cdm$cohort)
  class(attrition) <- c("tbl_df", "tbl", "data.frame")
  expect_equal(
    attrition,
    dplyr::tibble(
      cohort_definition_id = 1L,
      number_records = 0L,
      number_subjects = 0L,
      reason_id = 1L,
      reason = "Initial qualifying events",
      excluded_records = 0L,
      excluded_subjects = 0L
    )
  )

  # Edge cases ----
  # empty table after filtering
  expect_warning(
    cdm$cohort2 <- measurementCohort(cdm, list(a = 1L), name = "cohort2", valueAsConcept = list(a = 0))
  )
  attrition <- attrition(cdm$cohort2)
  class(attrition) <- c("tbl_df", "tbl", "data.frame")
  expect_equal(
    attrition,
    dplyr::tibble(
      cohort_definition_id = 1L,
      number_records = 0L,
      number_subjects = 0L,
      reason_id = 1L,
      reason = "Initial qualifying events",
      excluded_records = 0L,
      excluded_subjects = 0L
    )
  )

  # empty measurement table
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "measurement",
    table = dplyr::tibble(
      "measurement_id" = as.integer(),
      "person_id" = as.integer(),
      "measurement_concept_id" = as.integer(),
      "measurement_date" = as.Date(NA),
      "measurement_type_concept_id" = as.integer(),
      "value_as_number" = as.integer(),
      "value_as_concept_id" = as.integer(),
      "unit_concept_id" = as.integer()
    )
  )
  cdm$cohort <- measurementCohort(cdm, list(a = 1L), valueAsConcept = list("a" = 1L), name = "cohort")
  attrition <- attrition(cdm$cohort)
  class(attrition) <- c("tbl_df", "tbl", "data.frame")
  expect_equal(
    attrition,
    dplyr::tibble(
      cohort_definition_id = 1L,
      number_records = 0L,
      number_subjects = 0L,
      reason_id = 1L,
      reason = "Initial qualifying events",
      excluded_records = 0L,
      excluded_subjects = 0L
    )
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})
