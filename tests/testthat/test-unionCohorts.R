test_that("unionCohorts works", {
  skip_on_cran()

  obs <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L, 4L),
    person_id = c(1L, 2L, 3L, 4L),
    observation_period_start_date = as.Date(c(
      "2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09"
    )),
    observation_period_end_date = as.Date(c(
      "2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31"
    )),
    "period_type_concept_id" = NA_integer_
  )
  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5) |> as.integer(),
    subject_id = c(1, 1, 2, 3, 1, 1, 1, 1, 1, 1, 2, 3, 1, 2, 3, 3, 1) |> as.integer(),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13",
      "2001-08-30", "2002-01-09", "1999-06-04", "2015-01-22",
      "2002-11-30", "2000-06-07", "2015-04-05", "2015-04-11",
      "2011-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15",
      "2002-01-08", "2007-01-17", "2002-06-07", "2015-06-01",
      "2009-06-12", "2002-02-01", "2015-04-10", "2015-06-22",
      "2011-01-01"
    ))
  )
  cdm <- omock::mockCdmFromTables(tables = list("cohort1" = cohort_1)) |>
    omopgenerics::insertTable(name = "observation_period", table = obs) |>
    copyCdm()
  cdm$cohort1 <- cdm$cohort1 |>
    omopgenerics::newCohortTable(
      cohortCodelistRef = dplyr::tibble(
        cohort_definition_id = 1:4L,
        codelist_name = paste0("c", 1:4),
        concept_id = 1:4L,
        codelist_type = "index event"
      )
    )

  # test union cohort and codelist ----
  cdm$cohort2 <- unionCohorts(cdm$cohort1, name = "cohort2")
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1999-05-03", "2001-03-24", "2011-01-01", "2015-01-22")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2002-06-07", "2009-06-12", "2011-01-01", "2015-06-22")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("subject_id") |> sort() == c(1, 1:3)
  ))
  expect_true(all(attrition(cdm$cohort2) ==
                    dplyr::tibble(
                      cohort_definition_id = 1,
                      number_records = 4,
                      number_subjects = 3,
                      reason_id = 1,
                      reason = "Initial qualifying events",
                      excluded_records = 0,
                      excluded_subjects = 0
                    )))
  expect_true(settings(cdm$cohort2)$cohort_name == "cohort_1_cohort_2_cohort_3_cohort_4_cohort_5")
  codes <- attr(cdm$cohort2, "cohort_codelist") |> dplyr::collect()
  expect_equal(
    codes,
    dplyr::tibble(
      cohort_definition_id = 1L,
      codelist_name = paste0("c", 1:4),
      concept_id = 1:4L,
      codelist_type = "index event"
    ),
    ignore_attr = TRUE
  )

  # choose cohort Id and test codelist  ----
  cdm$cohort3 <- unionCohorts(cdm$cohort1, cohortId = 1:2, name = "cohort3")
  expect_true(all(
    cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1999-05-03", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13", "2015-02-25")
  ))
  expect_true(all(
    cdm$cohort3 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2001-06-15", "2001-11-27", "2002-01-29", "2002-06-12", "2005-07-19", "2015-04-30")
  ))
  expect_true(all(
    cdm$cohort3 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 1, 1, 2, 3)
  ))
  expect_true(all(
    attrition(cdm$cohort3) ==
      dplyr::tibble(
        cohort_definition_id = 1,
        number_records = 6,
        number_subjects = 3,
        reason_id = 1,
        reason = "Initial qualifying events",
        excluded_records = 0,
        excluded_subjects = 0
      )))
  expect_true(settings(cdm$cohort3)$cohort_name == "cohort_1_cohort_2")
  codes <- attr(cdm$cohort3, "cohort_codelist") |> dplyr::collect()
  expect_equal(
    codes,
    dplyr::tibble(
      cohort_definition_id = 1L,
      codelist_name = paste0("c", 1:2),
      concept_id = 1:2L,
      codelist_type = "index event"
    ),
    ignore_attr = TRUE
  )

  cdm$cohort4 <- unionCohorts(cdm$cohort1, cohortId = c("cohort_1", "cohort_2"), name = "cohort4")
  expect_identical(collectCohort(cdm$cohort3, 1), collectCohort(cdm$cohort4, 1))
  expect_identical(collectCohort(cdm$cohort3, 2), collectCohort(cdm$cohort4, 2))

  # union 2 empty cohorts
  cdm$cohort5 <- conceptCohort(cdm = cdm, conceptSet = list("a"= 1L, "b" = 2L), name = "cohort5")
  cdm$cohort6 <- cdm$cohort5 |> unionCohorts(name = "cohort6")
  expect_true(nrow(attrition(cdm$cohort6)) == 1)
  expect_true(attrition(cdm$cohort6)$number_records == 0)

  # test gap and keep original cohorts ----
  cdm$cohort7 <- unionCohorts(
    cdm$cohort1, name = "cohort7", gap = 10000, keepOriginalCohorts = TRUE, cohortId = c(1:3, 5)
  )
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort7, 1))
  expect_equal(collectCohort(cdm$cohort1, 2), collectCohort(cdm$cohort7, 2))
  expect_equal(collectCohort(cdm$cohort1, 3), collectCohort(cdm$cohort7, 3))
  expect_equal(collectCohort(cdm$cohort1, 4), collectCohort(cdm$cohort7, 4))
  expect_equal(
    collectCohort(cdm$cohort7, 6),
    dplyr::tibble(
      subject_id = 1:3L,
      cohort_start_date = as.Date(c("2001-03-24", "1999-05-03", "2015-01-22")),
      cohort_end_date = as.Date(c("2011-01-01", "2002-06-07", "2015-06-01"))
    )
  )

  # Indexes ----
  if (dbToTest == "postgres CDMConnector" & testIndexes) {
    expect_true(
      DBI::dbGetQuery(CDMConnector::cdmCon(cdm), paste0("SELECT * FROM pg_indexes WHERE tablename = 'coco_test_cohort7';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX coco_test_cohort7_subject_id_cohort_start_date_idx ON public.coco_test_cohort7 USING btree (subject_id, cohort_start_date)"
    )
  }

  # Test same name and keep cohorts ----
  cdm$cohort1 <- cdm$cohort1 |>
    CohortConstructor::unionCohorts(name = "cohort1", keepOriginalCohorts = TRUE)
  expect_true(all(settings(cdm$cohort1)$cohort_name %in% c("cohort_1", "cohort_2", "cohort_3", "cohort_4", "cohort_5", "cohort_1_cohort_2_cohort_3_cohort_4_cohort_5")))

  # Expected errors/warnings/messages ----
  expect_error(
    cohort <- unionCohorts(cdm$cohort1,
                           cohortId = 1,
                           gap = 0,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort1,
                           cohortId = NULL,
                           gap = -1,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort1,
                           cohortId = NULL,
                           gap = NA,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort1,
                           cohortId = NULL,
                           gap = Inf,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort1,
                           cohortId = "1",
                           gap = 1,
                           cohortName = NULL,
                           name = "cohort1")
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("multiple observation periods", {
  skip_on_cran()

  person <- dplyr::tibble(
    person_id = c(1L, 2L, 3L, 4L),
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L),
    year_of_birth = c(1997L, 1963L, 1986L, 1978L),
    month_of_birth = c(8L, 1L, 3L, 11L),
    day_of_birth = c(22L, 27L, 10L, 8L),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  cdm <- omock::mockCdmReference() |>
    omopgenerics::insertTable(name = "person", table = person) |>
    omopgenerics::insertTable(name = "observation_period", table = dplyr::tibble(
      "observation_period_id" = as.integer(1:7),
      "person_id" = as.integer(c(1, 1, 1, 2, 2, 3, 4)),
      "observation_period_start_date" = as.Date(c(
        "2000-01-01", "2001-01-01", "2003-01-01", "2001-01-01", "2002-01-01",
        "2000-01-01", "2000-01-01"
      )),
      "observation_period_end_date" =as.Date(c(
        "2000-12-20", "2002-01-01", "2005-01-01", "2001-12-31", "2004-01-01",
        "2004-01-01", "2003-01-01"
      )),
      "period_type_concept_id" = NA_integer_
    )) |>
    omopgenerics::insertTable(name = "cohort", table = dplyr::tibble(
      "cohort_definition_id" = as.integer(c(1, 1, 1, 1, 2, 2)),
      "subject_id" = as.integer(c(1, 1, 1, 2, 2, 1)),
      "cohort_start_date" = as.Date(c(
        "2000-01-01", "2000-12-01", "2001-01-01", "2001-01-01", "2002-01-01", "2003-01-01"
      )),
      "cohort_end_date" =as.Date(c(
        "2000-05-20", "2000-12-20", "2001-04-01", "2001-12-30", "2003-01-01", "2004-01-01"
      ))
    )) |>
    copyCdm()

  cdm$cohort <- cdm$cohort |> omopgenerics::newCohortTable() |> unionCohorts(gap = 99999)
  expect_identical(collectCohort(cdm$cohort, 1), dplyr::tibble(
    "subject_id" = as.integer(c(1, 1, 1, 2, 2)),
    "cohort_start_date" = as.Date(c("2000-01-01", "2001-01-01", "2003-01-01", "2001-01-01", "2002-01-01")),
    "cohort_end_date" = as.Date(c("2000-12-20", "2001-04-01", "2004-01-01", "2001-12-30", "2003-01-01"))
  ))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})
