test_that("gteIdentifier", {
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

test_that("generateIntersectCohortSet", {
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3, 1, 2, 3, 1, 2),
    subject_id = c(1, 1, 1, 2, 3, 3, 4, 4),
    cohort_start_date = as.Date(c(
      "2020-03-01", "2020-04-01", "2020-01-01", "2020-02-01", "2020-03-01",
      "2020-04-01", "2020-02-01", "2020-06-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-05-01", "2020-06-01", "2020-05-01", "2020-05-01", "2020-05-01",
      "2020-07-01", "2020-02-04", "2020-06-08"
    ))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4),
    gender_concept_id = c(8507, 8532, 8507, 8532),
    year_of_birth = 2000,
    month_of_birth = 1,
    day_of_birth = 1,
    race_concept_id = NA_character_,
    ethnicity_concept_id = NA_character_

  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2020-12-31"),
    period_type_concept_id = 32880
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    observation_period = observation_period, person = person, cohort1 = cohort
  )

  # mutually exclusive
  expect_no_error(cdm <- generateIntersectCohortSet(
    cdm = cdm, name = "cohort2", targetCohortName = "cohort1",
    mutuallyExclusive = TRUE
  ))
  expect_true(all(CDMConnector::settings(cdm$cohort2)$mutually_exclusive == TRUE))
  expect_true(cdm$cohort2 %>% dplyr::tally() %>% dplyr::pull() == 10)
  expect_true(all(
    CDMConnector::cohortCount(cdm$cohort2) %>%
      dplyr::arrange(.data$cohort_definition_id) %>%
      dplyr::pull("number_records") == c(2, 3, 0, 2, 1, 1, 1)
  ))

  # not mutually exclusive
  expect_no_error(cdm <- generateIntersectCohortSet(
    cdm = cdm, name = "cohort3", targetCohortName = "cohort1",
    mutuallyExclusive = FALSE
  ))
  expect_true(all(CDMConnector::settings(cdm$cohort3)$mutually_exclusive == FALSE))
  expect_true(cdm$cohort3 %>% dplyr::tally() %>% dplyr::pull() == 13)
  expect_true(all(
    CDMConnector::cohortCount(cdm$cohort3) %>%
      dplyr::arrange(.data$cohort_definition_id) %>%
      dplyr::pull("number_records") == c(3, 3, 1, 2, 1, 2, 1)
  ))

  # not enough cohorts provided
  expect_warning(cdm <- generateIntersectCohortSet(
    cdm = cdm, name = "cohort4", targetCohortName = "cohort1",
    targetCohortId = 1
  ), "At least 2 cohort id must be provided to do the combination")
  expect_equal(cdm$cohort1 %>%
                 omopgenerics::settings() %>%
                 dplyr::filter(cohort_definition_id == 1),
               cdm$cohort4 %>%
                 omopgenerics::settings())
  CDMConnector::cdmDisconnect(cdm)
})

test_that("only return comb", {
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3),
    subject_id = c(1, 1, 1),
    cohort_start_date = as.Date(c(
      "2020-03-01", "2020-01-01", "2020-03-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-05-01", "2020-05-02", "2020-04-01"
    ))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4),
    gender_concept_id = c(8507, 8532, 8507, 8532),
    year_of_birth = 2000,
    month_of_birth = 1,
    day_of_birth = 1,
    race_concept_id = NA_character_,
    ethnicity_concept_id = NA_character_
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2022-12-31"),
    period_type_concept_id = 32880
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    observation_period = observation_period, person = person, cohort1 = cohort
  )

  cdm <- generateIntersectCohortSet(
    cdm = cdm, name = "cohort2", targetCohortName = "cohort1",
    mutuallyExclusive = FALSE, returnOnlyComb = TRUE
  )

  expect_equal(cdm$cohort2 %>% dplyr::arrange(cohort_start_date) %>% dplyr::pull(cohort_start_date),
               as.Date(c("2020-03-01", "2020-03-01", "2020-03-01", "2020-03-01")))

  expect_equal(cdm$cohort2 %>% dplyr::arrange(cohort_end_date) %>% dplyr::pull(cohort_end_date),
               as.Date(c("2020-04-01", "2020-04-01", "2020-04-01", "2020-05-01")))

  cdm <- generateIntersectCohortSet(
    cdm = cdm, name = "cohort3", targetCohortName = "cohort1",
    mutuallyExclusive = TRUE, returnOnlyComb = TRUE
  )

  expect_equal(cdm$cohort3 %>% omopgenerics::cohortCount() %>% dplyr::pull(number_records),
              c(1, 1, 0, 0))
})
