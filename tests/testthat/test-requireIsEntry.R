test_that("test restrict to first entry works", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 3, 3),
    cohort_start_date = as.Date(c("2001-05-29", "2002-10-24", "2004-01-08", "1999-07-30", "2015-01-23", "2015-02-17")),
    cohort_end_date = as.Date(c("2002-10-23", "2004-01-07", "2009-10-03", "2001-10-02", "2015-02-16", "2015-03-10"))
  )

  cohort_2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    subject_id = c(1, 1, 1, 2, 3, 3, 1, 1, 2, 2, 2, 3),
    cohort_start_date = as.Date(c(
      "2001-05-29", "2002-10-24", "2004-01-08", "1999-07-30", "2015-01-23", "2015-02-17",
      "2002-10-09", "2003-09-12", "1999-04-16", "2000-03-09", "2000-05-05", "2015-02-22"
    )),
    cohort_end_date = as.Date(c(
      "2002-10-23", "2004-01-07", "2009-10-03", "2001-10-02", "2015-02-16", "2015-03-10",
      "2003-09-11", "2009-03-19", "2000-03-08", "2000-05-04", "2001-04-01", "2015-03-23"
    ))
  )

  cdm <- omock::mockCdmFromTables(
    tables = list("cohort1" = cohort_1, "cohort2" = cohort_2)
  ) |>
    copyCdm()

  expect_true(all(
    cdm$cohort1 |>
      CohortConstructor::requireIsFirstEntry() |>
      dplyr::pull(cohort_start_date) |>
      sort() == c("1999-07-30", "2001-05-29", "2015-01-23")
  ))

  expect_true(all(cdm$cohort1 |> CohortConstructor::requireIsFirstEntry() |>
                    dplyr::pull(subject_id) %in% 1:3))

  expect_true(all(
    cdm$cohort2 |>
      CohortConstructor::requireIsFirstEntry() |>
      dplyr::pull(cohort_start_date) |>
      sort() == c("1999-04-16", "1999-07-30", "2001-05-29", "2002-10-09", "2015-01-23", "2015-02-22")
  ))

  expect_true(all(
    cdm$cohort2 |>
      CohortConstructor::requireIsFirstEntry() |>
      dplyr::pull(subject_id) |>
      sort() == c(1, 1, 2, 2, 3, 3)
  ))

  dropCreatedTables(cdm = cdm)
})

test_that("requireIsFirstEntry, cohortIds & name arguments", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 1, 1, 2, 3, 3, 1, 1, 2, 2, 2, 3, 1, 2, 2, 2, 2, 2),
    cohort_start_date = as.Date(c(
      "2001-05-29", "2002-10-24", "2004-01-08", "1999-07-30", "2015-01-23", "2015-02-17",
      "2002-10-09", "2003-09-12", "1999-04-16", "2000-03-09", "2000-05-05", "2015-02-22",
      "2002-09-28", "1999-08-25", "1999-12-14", "1999-12-24", "2000-04-02", "2000-08-11"
    )),
    cohort_end_date = as.Date(c(
      "2002-10-23", "2004-01-07", "2009-10-03", "2001-10-02", "2015-02-16", "2015-03-10",
      "2003-09-11", "2009-03-19", "2000-03-08", "2000-05-04", "2001-04-01", "2015-03-23",
      "2008-10-13", "1999-12-13", "1999-12-23", "2000-04-01", "2000-08-10", "2000-09-13"
    ))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort" = cohort_1)) |>
    copyCdm()

  expect_no_error(
    cdm$new_cohort <- CohortConstructor::requireIsFirstEntry(
      cohort = cdm$cohort,
      cohortId = 1,
      name = "new_cohort")
  )

  counts <- omopgenerics::cohortCount(cdm$cohort)
  counts_new <- omopgenerics::cohortCount(cdm$new_cohort)

  expect_identical(counts |> dplyr::filter(cohort_definition_id %in% 2:3), counts_new |> dplyr::filter(cohort_definition_id %in% 2:3))
  expect_false(counts |> dplyr::filter(cohort_definition_id == 1) |> dplyr::pull(number_records) ==
                 counts_new |> dplyr::filter(cohort_definition_id == 1) |> dplyr::pull(number_records))
  expect_identical(counts_new |> dplyr::filter(cohort_definition_id == 1), dplyr::tibble(cohort_definition_id = 1L, number_records = 3L, number_subjects = 3L))
  expect_true(all(
    cdm$new_cohort |>
      dplyr::pull(cohort_start_date) |>
      sort() == c("1999-04-16", "1999-07-30", "1999-08-25", "1999-12-14",
                  "1999-12-24", "2000-03-09", "2000-04-02", "2000-05-05",
                  "2000-08-11", "2001-05-29", "2002-09-28", "2002-10-09",
                  "2003-09-12", "2015-01-23", "2015-02-22")
    ))
  expect_true(all(
    cdm$new_cohort |>
      dplyr::pull(cohort_end_date) |>
      sort() == c("1999-12-13", "1999-12-23", "2000-03-08", "2000-04-01",
                  "2000-05-04", "2000-08-10", "2000-09-13", "2001-04-01",
                  "2001-10-02", "2002-10-23", "2003-09-11", "2008-10-13",
                  "2009-03-19", "2015-02-16", "2015-03-23")))
  expect_true(all(cdm$new_cohort |> dplyr::pull(subject_id) |> sort() == c(
    1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3)))
  expect_true(all(
    omopgenerics::attrition(cdm$new_cohort)$reason  ==
      c("Initial qualifying events", "Restricted to first entry",
        "Initial qualifying events", "Initial qualifying events")
  ))

  cdm$new_cohort_2 <- CohortConstructor::requireIsEntry(
    entryRange = 1,
    cohort = cdm$cohort,
    cohortId = 1,
    name = "new_cohort_2")

  expect_identical(
    nrow(cdm$new_cohort |> dplyr::collect()),
    nrow(cdm$new_cohort_2 |> dplyr::collect()))

  dropCreatedTables(cdm = cdm)
})

test_that("errors", {
  skip_on_cran()
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(numberCohorts = 1, recordPerson = 2,seed = 1) |>
    copyCdm()

  expect_error(cdm$cohort |> requireIsFirstEntry(name = 1))
  expect_error(cdm$cohort1 <- cdm$cohort |> requireIsFirstEntry(name = "cohort2"))
  expect_warning(cdm$cohort |> requireIsFirstEntry(cohortId = Inf))
  expect_error(cdm$cohort |> dplyr::collect() |> requireIsFirstEntry())
  expect_warning(cdm$cohort |> requireIsFirstEntry(cohortId = c(1, 5)))

  dropCreatedTables(cdm = cdm)
})

test_that("requireIsLastEntry", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1,
                             2, 2, 2, 2, 2, 2,
                             3, 3, 3, 3, 3, 3),
    subject_id = c(1, 1, 1, 2, 3, 3,
                   1, 1, 2, 2, 2, 3,
                   1, 2, 2, 2, 2, 2),
    cohort_start_date = as.Date(c(
      "2001-05-29", "2002-10-24", "2004-01-08",
      "1999-07-30", "2015-01-23", "2015-02-17",
      "2002-10-09", "2003-09-12", "1999-04-16",
      "2000-03-09", "2000-05-05", "2015-02-22",
      "2002-09-28", "1999-08-25", "1999-12-14",
      "1999-12-24", "2000-04-02", "2000-08-11"
    )),
    cohort_end_date = as.Date(c(
      "2002-10-23", "2004-01-07", "2009-10-03",
      "2001-10-02", "2015-02-16", "2015-03-10",
      "2003-09-11", "2009-03-19", "2000-03-08",
      "2000-05-04", "2001-04-01", "2015-03-23",
      "2008-10-13", "1999-12-13", "1999-12-23",
      "2000-04-01", "2000-08-10", "2000-09-13"
    ))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort" = cohort_1)) |>
    copyCdm()

  cdm$new_cohort <- CohortConstructor::requireIsLastEntry(
    cohort = cdm$cohort,
    cohortId = 1,
    name = "new_cohort")

  counts <- omopgenerics::cohortCount(cdm$cohort)
  counts_new <- omopgenerics::cohortCount(cdm$new_cohort)

  expect_identical(counts |> dplyr::filter(cohort_definition_id %in% 2:3), counts_new |> dplyr::filter(cohort_definition_id %in% 2:3))
  expect_false(counts |> dplyr::filter(cohort_definition_id == 1) |> dplyr::pull(number_records) ==
                 counts_new |> dplyr::filter(cohort_definition_id == 1) |> dplyr::pull(number_records))
  expect_identical(counts_new |> dplyr::filter(cohort_definition_id == 1), dplyr::tibble(cohort_definition_id = 1L, number_records = 3L, number_subjects = 3L))
  expect_true(all(cdm$new_cohort |>  dplyr::pull(cohort_start_date) |> sort() == c(
    "1999-04-16", "1999-07-30", "1999-08-25", "1999-12-14", "1999-12-24",
    "2000-03-09", "2000-04-02", "2000-05-05", "2000-08-11", "2002-09-28",
    "2002-10-09", "2003-09-12", "2004-01-08", "2015-02-17", "2015-02-22"
  )))
  expect_true(all(cdm$new_cohort |> dplyr::pull(cohort_end_date) |> sort() == c(
    "1999-12-13", "1999-12-23", "2000-03-08", "2000-04-01", "2000-05-04",
    "2000-08-10", "2000-09-13", "2001-04-01", "2001-10-02", "2003-09-11",
    "2008-10-13", "2009-03-19", "2009-10-03", "2015-03-10", "2015-03-23"
  )))
  expect_true(all(cdm$new_cohort |> dplyr::pull(subject_id) |> sort() == c(
    1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
  )))
  expect_true(all(omopgenerics::attrition(cdm$new_cohort)$reason == c(
    c("Initial qualifying events", "Restricted to last entry", "Initial qualifying events",
      "Initial qualifying events"))
  ))

  # errors
  expect_error(cdm$cohort |> requireIsLastEntry(name = 1))
  expect_error(cdm$cohort1 <- cdm$cohort |> requireIsLastEntry(name = "cohort2"))
  expect_warning(cdm$cohort |> requireIsLastEntry(cohortId = Inf))
  expect_error(cdm$cohort |> dplyr::collect() |> requireIsLastEntry())
  expect_warning(cdm$cohort |> requireIsLastEntry(cohortId = c(1, 5)))

  dropCreatedTables(cdm = cdm)
})

test_that("requireEntry", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = rep(1, 40),
    subject_id = c(
      1, 1, 1,
      2, 2, 2,
      3, 3, 3,
      4, 4, 4, 4,
      5, 5, 5, 5, 5, 5,
      6, 6, 6, 6, 6,
      7, 7, 7, 7,
      8, 8,
      9, 9, 9, 9, 9, 9, 9, 9,
      10, 10
    ) |> as.integer(),
    cohort_start_date = as.Date(c(
      "2001-01-01", "2001-05-22", "2002-09-22",
      "1999-05-11", "2000-01-19", "2000-07-31",
      "2015-02-03", "2015-04-08", "2015-04-13",
      "1991-08-05", "1993-02-22", "1995-05-12", "1996-09-03",
      "2012-04-17", "2012-04-20", "2012-04-28", "2012-06-06", "2012-06-10", "2012-06-13",
      "2011-02-24", "2011-04-25", "2011-07-09", "2011-08-16", "2011-12-13",
      "2014-03-11", "2014-03-12", "2014-03-13", "2014-03-15",
      "1986-12-24", "1990-12-06",
      "1988-06-30", "1988-07-27", "1989-06-09", "1989-06-21", "1989-08-09", "1989-10-14", "1990-05-31", "1993-09-28",
      "2019-11-24", "2019-11-25"
    )),
    cohort_end_date = as.Date(c(
      "2001-05-21", "2002-09-21", "2010-04-28",
      "2000-01-18", "2000-07-30", "2001-02-07",
      "2015-04-07", "2015-04-12", "2015-06-11",
      "1993-02-21", "1995-05-11", "1996-09-02", "2000-09-10",
      "2012-04-19", "2012-04-27", "2012-06-05", "2012-06-09", "2012-06-12", "2012-09-06",
      "2011-04-24", "2011-07-08", "2011-08-15", "2011-12-12", "2012-11-07",
      "2014-03-11", "2014-03-12", "2014-03-14", "2014-03-21",
      "1990-12-05", "1997-10-28",
      "1988-07-26", "1989-06-08", "1989-06-20", "1989-08-08", "1989-10-13", "1990-05-30", "1993-09-27", "1999-04-22",
      "2019-11-24", "2019-11-25"
    ))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort1" = cohort_1)) |>
    copyCdm()

  # 1 to inf will leave the cohort table unchanged
  cdm$cohort1_a <- requireIsEntry(
    cohort = cdm$cohort1,
    entryRange = c(1, Inf),
    cohortId = 1,
    name = "cohort1_a")

  expect_equal(
    cdm$cohort1 |> dplyr::collect(),
    cdm$cohort1_a |> dplyr::collect(),
    ignore_attr = TRUE
  )

  cdm$cohort1_b <- requireIsEntry(
    cohort = cdm$cohort1,
    entryRange = c(1, 1),
    cohortId = 1,
    name = "cohort1_b")
  cdm$cohort1_c <- requireIsFirstEntry(
    cohort = cdm$cohort1,
    cohortId = 1,
    name = "cohort1_c")
  expect_equal(
    cdm$cohort1 |> dplyr::collect() |> dplyr::arrange(),
    cdm$cohort1_a |> dplyr::collect() |> dplyr::arrange(),
    ignore_attr = TRUE
  )

  # won't have any records
  cdm$cohort1_d <- requireIsEntry(
    cohort = cdm$cohort1,
    entryRange = c(50, Inf),
    cohortId = 1,
    name = "cohort1_d")
  expect_true(nrow(cdm$cohort1_d |> dplyr::collect()) == 0)

  # errors
  expect_error(cdm$cohort1 |> requireIsEntry(entryRange = c(1,2,3)))
  expect_error(cdm$cohort1 |> requireIsEntry(entryRange = c(1,NA)))
  expect_error(cdm$cohort1 |> requireIsEntry(entryRange = "a"))
  expect_error(cdm$cohort1 |> requireIsEntry(entryRange = c(1, 1), name = 1))
  expect_error(cdm$cohort1 <- cdm$cohort1 |> requireIsEntry(entryRange = c(1, 1), name = "cohort2"))
  expect_warning(cdm$cohort1 |> requireIsEntry(entryRange = c(1, 1), cohortId = Inf))
  expect_error(cdm$cohort1 |> dplyr::collect() |> requireIsEntry(entryRange = c(1, 1)))
  expect_warning(cdm$cohort1 |> requireIsEntry(entryRange = c(1, 1), cohortId = c(1, 5)))

  # mock cohort
  cdm <- omock::mockCdmFromTables(tables = list("my_cohort" = dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = as.integer(c(1,2, 2, 2, 2)),
    cohort_start_date  = as.Date(c(
      "2010-01-01", "2010-10-01", "2010-05-01", "2010-06-01", "2010-07-01"
    )),
    cohort_end_date = as.Date(c(
      "2010-01-01", "2010-10-01", "2010-05-01", "2010-06-01", "2010-07-01"
    ))
  ))) |>
    omopgenerics::insertTable(name = "observation_period", table = data.frame(
      observation_period_id = as.integer(c(1,2)),
      person_id = as.integer(c(1,2)),
      observation_period_start_date  = as.Date("2010-01-01"),
      observation_period_end_date  = as.Date("2011-01-01"),
      period_type_concept_id = NA_integer_
    )) |>
    omopgenerics::insertTable(name = "my_cohort", table = data.frame(
      cohort_definition_id = 1L,
      subject_id = as.integer(c(1,2, 2, 2, 2)),
      cohort_start_date  = c(as.Date("2010-01-01"),
                             as.Date("2010-10-01"),
                             as.Date("2010-05-01"),
                             as.Date("2010-06-01"),
                             as.Date("2010-07-01")),
      cohort_end_date  = c(as.Date("2010-01-01"),
                           as.Date("2010-10-01"),
                           as.Date("2010-05-01"),
                           as.Date("2010-06-01"),
                           as.Date("2010-07-01")))
    ) |>
    copyCdm()

  cdm$my_cohort <- cdm$my_cohort |> omopgenerics::newCohortTable(cohortSetRef = data.frame(cohort_definition_id = 1L,
                                                                                           cohort_name = "cohort"))
  cdm$my_cohort_1 <- requireIsEntry(
    cohort = cdm$my_cohort,
    entryRange = c(2, 3),
    name = "my_cohort_1")

  expect_equal(sort(cdm$my_cohort_1 |>
                      dplyr::pull("cohort_start_date")),
               as.Date(c("2010-06-01","2010-07-01")),
               ignore_attr = TRUE)

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("test indexes - postgres", {
  skip_on_cran()
  skip_if(!testIndexes)

  if (dbToTest == "postgres CDMConnector") {
    cdm <- omock::mockCdmFromTables(tables = list(
      my_cohort = dplyr::tibble(
        cohort_definition_id = 1L,
        subject_id = 1L,
        cohort_start_date = as.Date("2009-01-02"),
        cohort_end_date = as.Date("2009-01-03"),
        other_date = as.Date("2009-01-01")
      )
    )) |>
    copyCdm()

    con <- CDMConnector::cdmCon(cdm = cdm)

    cdm$my_cohort <- requireIsEntry(cdm$my_cohort, entryRange = c(0,2))
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    cdm$my_cohort <- requireIsEntry(cdm$my_cohort, entryRange = c(1,Inf))
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    cdm$my_cohort <- requireIsFirstEntry(cdm$my_cohort)
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    cdm$my_cohort <- requireIsLastEntry(cdm$my_cohort)
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

    dropCreatedTables(cdm = cdm)
  }
})
