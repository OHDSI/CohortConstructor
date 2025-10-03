test_that("requireDateRange", {
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
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date(c("2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09")),
    observation_period_end_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31")),
    period_type_concept_id = NA_integer_
  )

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    subject_id = c(1L, 1L, 2L, 3L, 1L, 1L, 1L, 1L),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15"
    ))
  )

  cohort_2 <- dplyr:: tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    subject_id = c(1L, 2L, 2L, 3L, 1L, 1L, 2L, 3L),
    cohort_start_date = as.Date(c(
      "2003-05-08", "2000-01-11", "2000-05-28", "2015-01-25",
      "2000-06-17", "2004-12-12", "1999-07-11", "2015-02-02"
    )),
    cohort_end_date = as.Date(c(
      "2005-04-08", "2000-05-27", "2001-09-08", "2015-04-26",
      "2004-12-11", "2007-09-06", "2002-03-26", "2015-08-12"
    ))
  )

  cdm <- omock::mockCdmFromTables(
    tables = list("cohort1" = cohort_1, "cohort2" = cohort_2)
  ) |>
    omopgenerics::insertTable(name = "observation_period", table = obs) |>
    omopgenerics::insertTable(name = "person", table = person) |>
    copyCdm()

  # empty result
  cdm$cohort1 <- cdm$cohort1 |>
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2011-01-01")))
  expect_true(all(cohortCount(cdm$cohort1)$number_records == c(0, 0)))
  expect_true(all(cohortCount(cdm$cohort1)$number_subjects == c(0, 0)))
  expect_true(cdm$cohort1 |> dplyr::tally() |> dplyr::pull("n") == 0)

  cdm$cohort1 <- cdm$cohort2 |>
    requireInDateRange(dateRange = as.Date(c("2010-01-01", "2020-01-01")),
                       name = "cohort1")
  expect_true(cdm$cohort1 |>
                dplyr::pull("subject_id") |> unique() == 3L)
  expect_true(all(cdm$cohort1 |>
                    dplyr::arrange(.data$cohort_start_date) |>
                    dplyr::pull("cohort_start_date") ==
                    c("2015-01-25", "2015-02-02")))

  # index date
  cdm$cohort3 <- cdm$cohort2 |>
    dplyr::mutate(new_index_date = as.Date("2000-03-30")) |>
    requireInDateRange(dateRange = as.Date(c("2000-01-01", "2001-01-01")),
                       name = "cohort3",
                       indexDate = "new_index_date")
  expect_identical(cdm$cohort3 |> dplyr::pull("cohort_start_date"), cdm$cohort2 |> dplyr::pull("cohort_start_date"))

  # 1 cohort id
  cdm$cohort4 <- cdm$cohort2 |>
    requireInDateRange(dateRange = as.Date(c("2000-01-01", "2001-01-01")),
                       cohortId = 1,
                       name = "cohort4")
  expect_true(all(attrition(cdm$cohort4)$reason ==
                    c("Initial qualifying events",
                      "cohort_start_date after 2000-01-01",
                      "cohort_start_date before 2001-01-01",
                      "Initial qualifying events")))
  expect_true(all(cohortCount(cdm$cohort4)$number_records == c(2,4)))
  expect_true(all(cohortCount(cdm$cohort4)$number_subjects == c(1,3)))
  expect_true(all(cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-07-11", "2000-01-11", "2000-05-28", "2000-06-17", "2004-12-12", "2015-02-02")))
  # NA
  expect_no_error(
    cdm$cohort5 <- cdm$cohort2 |>
      requireInDateRange(dateRange = as.Date(c(NA, "2010-01-01")), name = "cohort5")
  )
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-07-11", "2000-01-11", "2000-05-28", "2000-06-17", "2003-05-08", "2004-12-12")))
  expect_true(all(attrition(cdm$cohort5)$reason ==
                    c("Initial qualifying events",
                      "cohort_start_date before 2010-01-01",
                      "Initial qualifying events",
                      "cohort_start_date before 2010-01-01")))

  expect_no_error(
    cdm$cohort6 <- cdm$cohort2 |>
      requireInDateRange(dateRange = as.Date(c("2000-01-01", NA)), name = "cohort6")
  )
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2000-01-11", "2000-05-28", "2000-06-17", "2003-05-08", "2004-12-12", "2015-01-25", "2015-02-02")))
  expect_true(all(attrition(cdm$cohort6)$reason ==
                    c("Initial qualifying events",
                      "cohort_start_date after 2000-01-01",
                      "Initial qualifying events",
                      "cohort_start_date after 2000-01-01")))
  expect_no_error(
    cdm$cohort7 <- cdm$cohort2 |>
      requireInDateRange(dateRange = as.Date(c(NA, NA)), name = "cohort7")
  )
  expect_identical(cdm$cohort7 |> dplyr::collect(), cdm$cohort2 |> dplyr::collect())

  # expect error
  expect_error(requireInDateRange(cohort = "a"))
  expect_error(cdm$cohort1 |>
                 requireInDateRange(dateRange = as.Date(c("2010-01-01"))))
  expect_error(cdm$cohort1 |>
                 requireInDateRange(dateRange = as.Date(c("2010-01-01", "2010-01-01",
                                                          "2009-01-01"))))
  expect_error(cdm$cohort1 |>
                 requireInDateRange(dateRange = c("a", "b")))
  expect_error(
    cdm$cohort1 |>
      requireInDateRange(dateRange = as.Date(c("2010-01-01", "2010-01-01")), indexDate = "subject_id")
  )
  expect_error(
    cdm$cohort1 |>
      requireInDateRange(dateRange = as.Date(c("2011-01-01", "2010-01-01")))
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("trim cohort dates", {
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
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date(c("2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09")),
    observation_period_end_date = as.Date(c("2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31")),
    period_type_concept_id = NA_integer_
  )

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    subject_id = c(1L, 1L, 2L, 3L, 1L, 1L, 1L, 1L),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15"
    ))
  )

  cohort_2 <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    subject_id = c(1L, 2L, 2L, 3L, 1L, 1L, 2L, 3L),
    cohort_start_date = as.Date(c(
      "2003-05-08", "2000-01-11", "2000-05-28", "2015-01-25",
      "2000-06-17", "2004-12-12", "1999-07-11", "2015-02-02"
    )),
    cohort_end_date = as.Date(c(
      "2005-04-08", "2000-05-27", "2001-09-08", "2015-04-26",
      "2004-12-11", "2007-09-06", "2002-03-26", "2015-08-12"
    ))
  )

  cdm <- omock::mockCdmFromTables(
    tables = list("cohort1" = cohort_1, "cohort2" = cohort_2)
  ) |>
    omopgenerics::insertTable(name = "observation_period", table = obs) |>
    omopgenerics::insertTable(name = "person", table = person) |>
    copyCdm()

  cdm$cohort1 <- cdm$cohort1 |>
    trimToDateRange(dateRange = as.Date(c("2001-01-01", "2005-01-01")))

  expect_identical(sort(cdm$cohort1 |>
                          dplyr::pull("subject_id")), as.integer(c(1, 1, 1, 1, 1, 1, 2)))
  expect_true(all(cdm$cohort1 |>
                    dplyr::pull("cohort_start_date") ==
                    c("2003-05-17", "2004-03-11", "2001-01-01", "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13")))
  expect_true(all(cdm$cohort1 |>
                    dplyr::pull("cohort_end_date") ==
                    c("2004-03-10", "2005-01-01", "2001-06-15", "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-01")))

  # cohort id
  cdm$cohort3 <- cdm$cohort2 |>
    trimToDateRange(dateRange = as.Date(c("2001-01-01", "2005-01-01")),
                    cohortId = "cohort_1",
                    name = "cohort3")
  expect_true(omopgenerics::cohortCount(cdm$cohort3)$number_records[1] == 2)
  expect_identical(sort(cdm$cohort3 |>
                          dplyr::pull("subject_id")), as.integer(c(1, 1, 1, 2, 2, 3)))
  expect_identical(omopgenerics::attrition(cdm$cohort3)$reason[
    omopgenerics::attrition(cdm$cohort3)$cohort_definition_id == 1], c("Initial qualifying events", "cohort_start_date trimmed >= 2001-01-01", "cohort_end_date trimmed <= 2005-01-01"))
  expect_identical(omopgenerics::attrition(cdm$cohort3)$reason[
    omopgenerics::attrition(cdm$cohort3)$cohort_definition_id == 2], "Initial qualifying events")

  # NA
  cdm$cohort4 <- cdm$cohort2 |>
    trimToDateRange(dateRange = as.Date(c(NA, "2005-01-01")),
                    cohortId = 1,
                    name = "cohort4")
  expect_identical(sort(cdm$cohort4 |> dplyr::pull("cohort_end_date")), as.Date(c("2000-05-27", "2001-09-08", "2002-03-26", "2004-12-11", "2005-01-01", "2007-09-06", "2015-08-12")))
  expect_identical(omopgenerics::attrition(cdm$cohort4)$reason, c("Initial qualifying events", "cohort_end_date trimmed <= 2005-01-01", "Initial qualifying events"))

  cdm$cohort5 <- cdm$cohort2 |>
    trimToDateRange(dateRange = as.Date(c("2005-01-01", NA)),
                    cohortId = 1,
                    name = "cohort5")
  expect_identical(sort(cdm$cohort5 |> dplyr::pull("cohort_start_date")), as.Date(c("1999-07-11", "2000-06-17", "2004-12-12", "2005-01-01", "2015-01-25", "2015-02-02")))
  expect_identical(omopgenerics::attrition(cdm$cohort5)$reason, c("Initial qualifying events", "cohort_start_date trimmed >= 2005-01-01", "Initial qualifying events"))

  cdm$cohort6 <- cdm$cohort2 |>
    trimToDateRange(dateRange = as.Date(c(NA, NA)),
                    cohortId = 1,
                    name = "cohort6")
  expect_identical(cdm$cohort6 |> dplyr::collect(), cdm$cohort2 |> dplyr::collect())

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("test indexes - postgres, and atFirst", {
  skip_on_cran()
  skip_if(!testIndexes)

  if (dbToTest == "postgres CDMConnector") {
    cdm <- omock::mockCdmFromTables(tables = list(
      my_cohort = data.frame(
        cohort_definition_id = 1L,
        subject_id = 1L,
        cohort_start_date = as.Date("2009-01-02"),
        cohort_end_date = as.Date("2009-01-03"),
        other_date = as.Date("2009-01-01")
      )
    )) |>
      copyCdm()

    con <- CDMConnector::cdmCon(cdm = cdm)

    omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::contains("og_"))

    cdm$my_cohort <- requireInDateRange(cdm$my_cohort, dateRange = as.Date(c("1990-01-01", "2020-01-01")))
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    cdm$my_cohort <- trimToDateRange(cdm$my_cohort, dateRange = as.Date(c("1990-01-01", "2020-01-01")))
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    # atFirst
    cohort <- dplyr::tibble(
      cohort_definition_id = c(rep(1L, 4), rep(2L, 4)),
      subject_id = c(1L, 1L, 2L, 3L, rep(1L, 4)),
      cohort_start_date = as.Date(c(
        "2008-05-17", "2009-03-11", "2010-05-03", "2010-02-25",
        "2008-03-24", "2008-11-28", "2010-01-30", "2009-06-13"
      )),
      cohort_end_date = as.Date(c(
        "2009-03-10", "2009-07-19", "2010-06-15", "2010-04-30",
        "2008-11-27", "2008-01-29", "2010-06-12", "2010-01-15"
      ))
    )
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = "my_cohort",
                                     table = cohort)
    cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort, .softValidation = TRUE)
    cdm$my_cohort_1 <- requireInDateRange(cohort = cdm$my_cohort,
                                          dateRange = as.Date(c("2008-05-12", NA)),
                                          atFirst = TRUE,
                                          name = "my_cohort_1")
    expect_equal(
      collectCohort(cdm$my_cohort_1, 2),
      dplyr::tibble(
        subject_id = 1L,
        cohort_start_date = as.Date(NULL),
        cohort_end_date = as.Date(NULL)
      )
    )
    expect_equal(
      collectCohort(cdm$my_cohort_1, 1),
      dplyr::tibble(
        subject_id = c(1L, 1L, 2L, 3L),
        cohort_start_date = as.Date(c(
          "2008-05-17", "2009-03-11", "2010-05-03", "2010-02-25"
        )),
        cohort_end_date = as.Date(c(
          "2009-03-10", "2009-07-19", "2010-06-15", "2010-04-30"
        ))
      )
    )
    expect_equal(
      attrition(cdm$my_cohort_1)$reason,
      c('Initial qualifying events',
        "cohort_start_date after 2008-05-12. Requirement applied to the first entry",
        'Initial qualifying events',
        "cohort_start_date after 2008-05-12. Requirement applied to the first entry"
      ))

    cdm$my_cohort_2 <- requireInDateRange(cohort = cdm$my_cohort,
                                          dateRange = as.Date(c("2008-01-01", "2008-02-01")),
                                          atFirst = TRUE,
                                          cohortId = 2,
                                          name = "my_cohort_2")
    expect_equal(
      collectCohort(cdm$my_cohort_2, 2),
      dplyr::tibble(
        subject_id = 1L,
        cohort_start_date = as.Date(NULL),
        cohort_end_date = as.Date(NULL)
      )
    )
    expect_equal(
      attrition(cdm$my_cohort_2)$reason,
      c('Initial qualifying events',
        'Initial qualifying events',
        "cohort_start_date after 2008-01-01. Requirement applied to the first entry",
        "cohort_start_date before 2008-02-01. Requirement applied to the first entry"
      ))

    expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

    dropCreatedTables(cdm = cdm)
  }

})

