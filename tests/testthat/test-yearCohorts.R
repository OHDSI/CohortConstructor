test_that("yearCohorts - change name", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c("2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25")),
    cohort_end_date = as.Date(c("2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30"))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort" = cohort_1)) |>
    copyCdm()

  # simple example
  cdm$cohort1 <- yearCohorts(cohort = cdm$cohort,
                             years = 1997:2002,
                             cohortId = NULL,
                             name = "cohort1")
  expect_identical(settings(cdm$cohort1) |> dplyr::arrange(.data$cohort_definition_id), dplyr::tibble(
                 cohort_definition_id = as.integer(1:6),
                 cohort_name = paste0("cohort_1_", 1997:2002),
                 target_cohort_definition_id = 1L,
                 year = 1997:2002,
                 target_cohort_name = "cohort_1"
               ))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1999-05-03", "2000-01-01", "2001-01-01")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1999-12-31", "2000-12-31", "2001-06-15")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") |> sort() == c(2,2,2)))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_definition_id") |> sort() == c(3,4,5)))
  expect_true(all(attrition(cdm$cohort1)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 1997-01-01 and 1997-12-31',
    'Initial qualifying events', 'Restrict to observations between: 1998-01-01 and 1998-12-31',
    'Initial qualifying events', 'Restrict to observations between: 1999-01-01 and 1999-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2000-01-01 and 2000-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2001-01-01 and 2001-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2002-01-01 and 2002-12-31'
  )))

 # more than 1 cohort
  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
    subject_id = c(1, 1, 2, 3, 1, 1, 1, 1, 1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13",
      "2001-08-30", "2002-01-09", "1999-06-04", "2015-01-22"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15",
      "2002-01-08", "2007-01-17", "2002-06-07", "2015-06-01"
    ))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort" = cohort_1)) |>
    copyCdm()

  # all cohorts
  cdm$cohort1 <- yearCohorts(cohort = cdm$cohort,
                            years = 2005:2008,
                            cohortId = NULL,
                            name = "cohort1")
  expect_identical(settings(cdm$cohort1) |> dplyr::arrange(.data$cohort_definition_id), dplyr::tibble(
                 cohort_definition_id = as.integer(1:12),
                 cohort_name = c(
                   "cohort_1_2005", "cohort_2_2005", "cohort_3_2005", "cohort_1_2006",
                   "cohort_2_2006", "cohort_3_2006", "cohort_1_2007", "cohort_2_2007",
                   "cohort_3_2007", "cohort_1_2008", "cohort_2_2008", "cohort_3_2008"
                 ),
                 target_cohort_definition_id = as.integer(c(1:3, 1:3, 1:3, 1:3)),
                 year = as.integer(c(rep(2005, 3), rep(2006, 3), rep(2007, 3), rep(2008, 3))),
                 target_cohort_name = rep(paste0("cohort_", 1:3), 4)
               ))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2005-01-01", "2005-01-01", "2005-01-01", "2006-01-01", "2007-01-01")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("2005-01-15", "2005-07-19", "2005-12-31", "2006-12-31", "2007-01-17")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 1, 1, 1)))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 2, 3, 6, 9)))
  expect_true(all(attrition(cdm$cohort1)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31'
  )))
  expect_true(all(cohortCount(cdm$cohort1)$number_records == c(1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0)))

  # just 1 cohort
  cdm$cohort1 <- yearCohorts(cohort = cdm$cohort,
                             years = 2005:2008,
                             cohortId = 1,
                             name = "cohort1")
  expect_identical(settings(cdm$cohort1) |> dplyr::arrange(.data$cohort_definition_id), dplyr::tibble(
                 cohort_definition_id = as.integer(1:4),
                 cohort_name = c(
                   paste0("cohort_1_", 2005:2008)
                 ),
                 target_cohort_definition_id = as.integer(c(1, 1, 1, 1)),
                 year = c(2005:2008),
                 target_cohort_name = c(rep("cohort_1", 4))
               ))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2005-01-01")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("2005-07-19")))
  expect_true(all(cdm$cohort1 |> dplyr::pull("subject_id") |> sort() == 1))
  expect_true(all(cdm$cohort1 |> dplyr::pull("cohort_definition_id") |> sort() == 1))
  expect_true(all(attrition(cdm$cohort1)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31'
  )))
  expect_true(all(cohortCount(cdm$cohort1)$number_records == c(1, 0, 0, 0)))

  # no years in:
  cdm$cohort1 <- yearCohorts(cohort = cdm$cohort,
                             years = numeric(),
                             cohortId = 1,
                             name = "cohort1")
  expect_identical(cdm$cohort1 |> dplyr::collect(), cdm$cohort |> dplyr::collect())

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("yearCohorts - keep name", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c("2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25")),
    cohort_end_date = as.Date(c("2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30"))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort" = cohort_1)) |>
    copyCdm()

  # simple example
  cdm$cohort <- yearCohorts(cohort = cdm$cohort,
                             years = 1997:2002,
                             cohortId = settings(cdm$cohort)$cohort_name)
  expect_identical(settings(cdm$cohort) |> dplyr::arrange(.data$cohort_definition_id), dplyr::tibble(
                 cohort_definition_id = as.integer(1:6),
                 cohort_name = paste0("cohort_1_", 1997:2002),
                 target_cohort_definition_id = 1L,
                 year = 1997:2002,
                 target_cohort_name = "cohort_1"
               ))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("1999-05-03", "2000-01-01", "2001-01-01")))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("1999-12-31", "2000-12-31", "2001-06-15")))
  expect_true(all(cdm$cohort |> dplyr::pull("subject_id") |> sort() == c(2, 2, 2)))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_definition_id") |> sort() == c(3, 4, 5)))
  expect_true(all(attrition(cdm$cohort)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 1997-01-01 and 1997-12-31',
    'Initial qualifying events', 'Restrict to observations between: 1998-01-01 and 1998-12-31',
    'Initial qualifying events', 'Restrict to observations between: 1999-01-01 and 1999-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2000-01-01 and 2000-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2001-01-01 and 2001-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2002-01-01 and 2002-12-31'
  )))

  # more than 1 cohort
  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
    subject_id = c(1, 1, 2, 3, 1, 1, 1, 1, 1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13",
      "2001-08-30", "2002-01-09", "1999-06-04", "2015-01-22"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15",
      "2002-01-08", "2007-01-17", "2002-06-07", "2015-06-01"
    ))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort" = cohort_1)) |>
    copyCdm()

  # just 1 cohort
  cdm$cohort <- yearCohorts(cohort = cdm$cohort,
                             years = 2005:2008,
                             cohortId = 1)
  expect_identical(settings(cdm$cohort) |> dplyr::arrange(.data$cohort_definition_id), dplyr::tibble(
                 cohort_definition_id = as.integer(1:4),
                 cohort_name = c(
                   paste0("cohort_1_", 2005:2008)
                 ),
                 target_cohort_definition_id = as.integer(c(1, 1, 1, 1)),
                 year = c(2005:2008),
                 target_cohort_name = c(rep("cohort_1", 4))
               ))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
                    c("2005-01-01")))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
                    c("2005-07-19")))
  expect_true(all(cdm$cohort |> dplyr::pull("subject_id") |> sort() == c(1)))
  expect_true(all(cdm$cohort |> dplyr::pull("cohort_definition_id") |> sort() == c(1)))
  expect_true(all(attrition(cdm$cohort)$reason == c(
    'Initial qualifying events', 'Restrict to observations between: 2005-01-01 and 2005-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2006-01-01 and 2006-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2007-01-01 and 2007-12-31',
    'Initial qualifying events', 'Restrict to observations between: 2008-01-01 and 2008-12-31'
  )))
  expect_true(all(cohortCount(cdm$cohort)$number_records == c(1, 0, 0, 0)))

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

    cdm$my_cohort <- yearCohorts(cdm$my_cohort, years = 2008:2010)
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

    dropCreatedTables(cdm = cdm)
  }
})
