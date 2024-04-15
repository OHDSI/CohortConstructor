test_that("getIdentifier", {
  x <- dplyr::tibble(a = 1)

  expect_no_error(id <- getIdentifier(x))

  expect_true(getIdentifier(x, len = 5) |> length() == 5)
  expect_true(getIdentifier(x, nchar = 2) |> nchar() == 2)
  expect_true(getIdentifier(x, prefix = "id_", nchar = 2) |> nchar() == 5)
  expect_true(getIdentifier(x, prefix = "id_", nchar = 8) |> substr(1, 3) == "id_")
})

test_that("joinOverlap", {
  x <- dplyr::tibble(
    start_date = as.Date(c(
      "2020-01-01", "2020-03-01", "2020-06-01", "2020-02-01", "2020-05-02",
      "2020-03-01", "2020-06-01", "2020-04-01"
    )),
    end_date = as.Date(c(
      "2020-04-01", "2020-06-01", "2020-08-01", "2020-05-01", "2020-07-01",
      "2020-05-01", "2020-08-01", "2020-07-01"
    )),
    pid = c(1, 1, 1, 1, 1, 2, 2, 2),
    def_id = c(1, 1, 1, 2, 2, 1, 1, 2)
  )
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(db, "x", x)
  x <- dplyr::tbl(db, "x")

  # gap = 0
  expect_no_error(
    res <- joinOverlap(
      x, start = "start_date", end = "end_date", by = c("pid", "def_id")
    ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  )
  expect_true(nrow(res) == 6)
  expect_identical(
    res,
    dplyr::tibble(
      pid = c(1, 1, 1, 2, 2, 2),
      def_id = c(1, 2, 2, 1, 1, 2),
      start_date = as.Date(c(
        "2020-01-01", "2020-02-01", "2020-05-02", "2020-03-01", "2020-06-01",
        "2020-04-01"
      )),
      end_date = as.Date(c(
        "2020-08-01", "2020-05-01", "2020-07-01", "2020-05-01", "2020-08-01",
        "2020-07-01"
      ))
    )
  )

  # gap = 1
  expect_no_error(
    res <- joinOverlap(
      x, start = "start_date", end = "end_date", by = c("pid", "def_id"), gap = 1
    ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  )
  expect_true(nrow(res) == 5)
  expect_identical(
    res,
    dplyr::tibble(
      pid = c(1, 1, 2, 2, 2),
      def_id = c(1, 2, 1, 1, 2),
      start_date = as.Date(c(
        "2020-01-01", "2020-02-01", "2020-03-01", "2020-06-01", "2020-04-01"
      )),
      end_date = as.Date(c(
        "2020-08-01", "2020-07-01", "2020-05-01", "2020-08-01", "2020-07-01"
      ))
    )
  )

  DBI::dbDisconnect(db, shutdown = TRUE)
})

test_that("splitOverlap", {
  x <- dplyr::tibble(
    start_date = as.Date(c(
      "2020-01-01", "2020-03-01", "2020-06-01", "2020-02-01", "2020-05-02",
      "2020-03-01", "2020-06-01", "2020-04-01"
    )),
    end_date = as.Date(c(
      "2020-04-01", "2020-06-01", "2020-08-01", "2020-05-01", "2020-07-01",
      "2020-05-01", "2020-08-01", "2020-07-01"
    )),
    pid = c(1, 1, 1, 1, 1, 2, 2, 2),
    def_id = c(1, 1, 1, 2, 2, 1, 2, 1)
  )
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(db, "x", x)
  x <- dplyr::tbl(db, "x")

  expect_no_error(
    res <- splitOverlap(
      x, start = "start_date", end = "end_date", by = c("pid", "def_id")
    ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  )
  expect_true(nrow(res) == 11)
  expect_identical(
    res,
    dplyr::tibble(
      pid = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
      def_id = c(1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2),
      start_date = as.Date(c(
        "2020-01-01", "2020-03-01", "2020-04-02", "2020-06-01", "2020-06-02",
        "2020-02-01", "2020-05-02", "2020-03-01", "2020-04-01", "2020-05-02",
        "2020-06-01"
      )),
      end_date = as.Date(c(
        "2020-02-29", "2020-04-01", "2020-05-31", "2020-06-01", "2020-08-01",
        "2020-05-01", "2020-07-01", "2020-03-31", "2020-05-01", "2020-07-01",
        "2020-08-01"
      ))
    )
  )

  DBI::dbDisconnect(db, shutdown = TRUE)
})

test_that("intersectCohort", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  # mutually exclusive
  expect_no_error(cdm$cohort2 <- intersectCohort(
    cohort = cdm$cohort1, name = "cohort2",
    mutuallyExclusive = TRUE
  ))
  expect_true(all(omopgenerics::settings(cdm$cohort2)$mutually_exclusive == TRUE))
  expect_true(cdm$cohort2 %>% dplyr::tally() %>% dplyr::pull() == 10)
  expect_true(all(
    CDMConnector::cohortCount(cdm$cohort2) %>%
      dplyr::arrange(.data$cohort_definition_id) %>%
      dplyr::pull("number_records") == c(1, 4, 5)
  ))
  expect_true(nrow(omopgenerics::settings(cdm$cohort2)) == 3)
  expect_true(all(
    cdm$cohort2 %>%
      dplyr::pull("cohort_start_date") %>%
      sort() ==
      c("1997-10-22", "2000-06-23", "2001-03-30" ,"2001-07-16", "2001-12-04",
        "2003-06-15", "2005-11-24", "2015-03-05", "2015-03-25", "2015-04-15")
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$reason ==
      c("Initial qualifying events", "Exclusive in cohort_1", "Initial qualifying events",
        "Exclusive in cohort_2", "[cohort_1] Initial qualifying events", "[cohort_2] Initial qualifying events",
        "Cohort intersect: cohort_1 and cohort_2")
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$reason_id ==  c(1, 2, 1, 2, 1, 2, 3)
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$excluded_records ==  c(0, 3, 0, 0, 0, 0,3)
  ))

  # not mutually exclusive and gap
  expect_no_error(cdm$cohort3 <- intersectCohort(
    cohort = cdm$cohort1, name = "cohort3",
    mutuallyExclusive = FALSE, gap = 1
  ))
  expect_true(all(CDMConnector::settings(cdm$cohort3)$mutually_exclusive == FALSE))
  expect_true(cdm$cohort3 %>% dplyr::tally() %>% dplyr::pull() == 7)
  expect_true(all(
    omopgenerics::cohortCount(cdm$cohort3) %>%
      dplyr::arrange(.data$cohort_definition_id) %>%
      dplyr::pull("number_records") == c(4, 4, 2)
  ))
  expect_true(all(
    omopgenerics::cohortCount(cdm$cohort3) %>%
      dplyr::arrange(.data$cohort_definition_id) %>%
      dplyr::pull("number_subjects") == c(3, 2, 2)
  ))
  expect_true(nrow(omopgenerics::settings(cdm$cohort3)) == 3)
  expect_true(all(
    cdm$cohort3 %>%
      dplyr::pull("cohort_start_date") %>%
      sort() ==
      c("1997-10-22", "2000-06-23", "2001-03-30", "2001-03-30", "2015-03-05", "2015-03-25", "2015-03-25")
  ))
  expect_true(all(
    cdm$cohort3 %>%
      dplyr::pull("cohort_end_date") %>%
      sort() ==
      c("1999-05-28", "2005-11-23", "2005-11-23", "2006-09-27", "2015-04-14", "2015-04-14", "2015-07-06")
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort3)$reason ==
      c("Initial qualifying events", "Initial qualifying events", "[cohort_1] Initial qualifying events",
        "[cohort_2] Initial qualifying events", "Cohort intersect: cohort_1 and cohort_2")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason_id ==  c(1, 1, 1, 2, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_records ==  c(4, 4, 4, 4, 2)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_subjects ==  c(3, 2, 3, 2, 2)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_records ==  c(0, 0, 0, 0, 6)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_subjects ==  c(0, 0, 0, 0, 3)))

  # not enough cohorts provided
  expect_warning(cdm$cohort4 <- intersectCohort(
    cohort = cdm$cohort1, name = "cohort4",
    cohortId = 1
  ), "At least 2 cohort id must be provided to do the combination")
  expect_equal(cdm$cohort1 %>%
                 omopgenerics::settings() %>%
                 dplyr::filter(cohort_definition_id == 1),
               cdm$cohort4 %>%
                 omopgenerics::settings())
  CDMConnector::cdmDisconnect(cdm)
})

test_that("only return comb", {
  # combination null
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 2, seed = 2)
  cdm_local$cohort1 <- cdm_local$cohort1 |>
    dplyr::filter(cohort_end_date != as.Date("2015-04-17"))
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort2 <- intersectCohort(
    cohort = cdm$cohort1, name = "cohort2",
    mutuallyExclusive = FALSE, returnOnlyComb = TRUE
  )
  expect_true(nrow(dplyr::collect(cdm$cohort2)) == 0)
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$reason ==
      c("[cohort_1] Initial qualifying events", "[cohort_2] Initial qualifying events",
        "Cohort intersect: cohort_1 and cohort_2")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$reason_id ==  c(1, 2, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$number_records ==  c(4, 4, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$number_subjects ==  c(3, 2, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$excluded_records ==  c(0, 0, 8)))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$excluded_subjects ==  c(0, 0, 5)))

  # not null combination
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 3)
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")
  cdm$cohort3 <- intersectCohort(
    cohort = cdm$cohort1, name = "cohort3",
    mutuallyExclusive = FALSE, returnOnlyComb = TRUE, gap = 1
  )
  expect_equal(
    cdm$cohort3 |>
      dplyr::collect() %>%
      dplyr::arrange(.data$cohort_start_date) %>%
      dplyr::pull(.data$cohort_start_date),
    as.Date(c("1997-10-22", "2001-03-30", "2015-03-05", "2015-03-25", "2015-03-25", "2015-03-25"))
  )
  expect_equal(
    cdm$cohort3 |>
      dplyr::collect() %>%
      dplyr::arrange(cohort_end_date) %>%
      dplyr::pull(cohort_end_date),
    as.Date(c("1999-05-28", "2005-11-23", "2015-04-14", "2015-04-14", "2015-04-14", "2015-07-06"))
  )
  expect_true(nrow(omopgenerics::settings(cdm$cohort3)) == 4)
  expect_true(all(omopgenerics::settings(cdm$cohort3)$cohort_1 == c(1, 1, 1, 0)))
  expect_true(all(omopgenerics::settings(cdm$cohort3)$cohort_2 == c(1, 1, 0, 1)))
  expect_true(all(omopgenerics::settings(cdm$cohort3)$cohort_3 == c(0, 1, 1, 1)))
  expect_false(any(omopgenerics::settings(cdm$cohort3)$mutually_exclusive))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort3)$reason ==
      c('[cohort_1] Initial qualifying events', '[cohort_2] Initial qualifying events',
      'Cohort intersect: cohort_1 and cohort_2', '[cohort_1] Initial qualifying events',
      '[cohort_2] Initial qualifying events', '[cohort_3] Initial qualifying events',
      'Cohort intersect: cohort_1, cohort_2 and cohort_3', '[cohort_1] Initial qualifying events',
      '[cohort_3] Initial qualifying events', 'Cohort intersect: cohort_1 and cohort_3',
      '[cohort_2] Initial qualifying events', '[cohort_3] Initial qualifying events',
      'Cohort intersect: cohort_2 and cohort_3')
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason_id ==
                    c(1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 1, 2, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_records ==
                    c(4, 4, 2, 4, 4, 4, 1, 4, 4, 2, 4, 4, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_subjects ==
                    c(3, 2, 2, 3, 2, 3, 1, 3, 3, 2, 2, 3, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_records ==
                    c(0, 0, 6, 0, 0, 0, 11, 0, 0, 6, 0, 0, 7)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_subjects ==
                    c(0, 0, 3, 0, 0, 0, 7, 0, 0, 4, 0, 0, 4)))

  cdm$cohort4 <- intersectCohort(
    cohort = cdm$cohort1, name = "cohort4",
    mutuallyExclusive = TRUE, returnOnlyComb = TRUE, gap = 1
  )

  expect_equal(
    cdm$cohort4 |>
      dplyr::collect() %>%
      dplyr::arrange(.data$cohort_start_date) %>%
      dplyr::pull(.data$cohort_start_date),
    as.Date(c("1997-10-22", "2001-03-30", "2015-03-05", "2015-03-25", "2015-04-15"))
  )
  expect_equal(
    cdm$cohort4 |>
      dplyr::collect() %>%
      dplyr::arrange(cohort_end_date) %>%
      dplyr::pull(cohort_end_date),
    as.Date(c("1999-05-28", "2005-11-23", "2015-03-24", "2015-04-14", "2015-07-06"))
  )
  expect_true(nrow(omopgenerics::settings(cdm$cohort4)) == 4)
  expect_true(all(omopgenerics::settings(cdm$cohort4)$cohort_1 == c(1, 1, 1, 0)))
  expect_true(all(omopgenerics::settings(cdm$cohort4)$cohort_2 == c(1, 1, 0, 1)))
  expect_true(all(omopgenerics::settings(cdm$cohort4)$cohort_3 == c(0, 1, 1, 1)))
  expect_true(all(omopgenerics::settings(cdm$cohort4)$mutually_exclusive))
})

test_that("attrition and cohortId", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(tableName = c("cohort1"), numberCohorts = 4, seed = 2)
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- CDMConnector::copy_cdm_to(con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                   cdm = cdm_local,
                                   schema = "main")

  cdm$cohort1 <- cdm$cohort1 |>
    requireInDateRange(dateRange = as.Date(c("1990-01-01", "2025-01-01"))) |>
    requireSex(sex = "Female") |>
    requireAge(ageRange = list(c(0,40)))

  cdm$cohort1 <- intersectCohort(
    cohort = cdm$cohort1, cohortId = 1:2,
    name = "cohort1", mutuallyExclusive = TRUE
  )
  expect_true(all(
    omopgenerics::attrition(cdm$cohort1)$reason ==
      c("Initial qualifying events", "cohort_start_date between 1990-01-01 & 2025-01-01",
        "Sex requirement: Female", "Age requirement: 0 to 40",
        "Exclusive in cohort_1", "Initial qualifying events",
        "cohort_start_date between 1990-01-01 & 2025-01-01", "Sex requirement: Female",
        "Age requirement: 0 to 40", "Exclusive in cohort_2",
        "[cohort_1] Initial qualifying events", "[cohort_1] cohort_start_date between 1990-01-01 & 2025-01-01",
        "[cohort_1] Sex requirement: Female", "[cohort_1] Age requirement: 0 to 40",
        "[cohort_2] Initial qualifying events", "[cohort_2] cohort_start_date between 1990-01-01 & 2025-01-01",
        "[cohort_2] Sex requirement: Female","[cohort_2] Age requirement: 0 to 40",
        "Cohort intersect: cohort_1 and cohort_2")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason_id ==  c(1:5, 1:5, 1:9)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$number_records ==
                    c(4, 4, 1, 1, 1, 4, 4, 0, 0, 0, 4, 4, 1, 1, 4, 4, 0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$number_subjects ==
                    c(3, 3, 1, 1, 1, 2, 2, 0, 0, 0, 3, 3, 1, 1, 2, 2, 0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$excluded_records ==
                    c(0, 0, 3, 0, 0, 0, 0, 4, 0, 0, 0, 0, 3, 0, 0, 0, 4, 0, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$excluded_subjects ==
                    c(0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, 1)))
  expect_true(all(omopgenerics::settings(cdm$cohort1)$cohort_name == c("cohort_1", "cohort_2", "cohort_1_cohort_2")))
})
