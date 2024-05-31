test_that("getIdentifier", {
  x <- dplyr::tibble(a = 1)

  expect_no_error(id <- getIdentifier(x))

  expect_true(getIdentifier(x, len = 5) |> length() == 5)
  expect_true(getIdentifier(x, nchar = 2) |> nchar() == 2)
  expect_true(getIdentifier(x, prefix = "id_", nchar = 2) |> nchar() == 5)
  expect_true(getIdentifier(x, prefix = "id_", nchar = 8) |> substr(1, 3) == "id_")
})

test_that("joinOverlap", {
  testthat::skip_on_cran()
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

  cdm <- mockCohortConstructor(otherTables = list(x = x),
                               con = connection(),
                               writeSchema = writeSchema())
  # gap = 0
  res <- joinOverlap(
    cdm$x, startDate = "start_date", endDate = "end_date", by = c("pid", "def_id")
  ) |>
    dplyr::collect() |>
    dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  expect_true(nrow(res) == 6)
  expect_equal(
    res |> dplyr::arrange(.data$start_date, .data$end_date) |> dplyr::select("pid", "def_id", "start_date", "end_date"),
    dplyr::tibble(
      pid = c(1, 1, 2, 2, 1, 2),
      def_id = c(1, 2, 1, 2, 2, 1),
      start_date = as.Date(c(
        "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-02", "2020-06-01"
      )),
      end_date = as.Date(c(
        "2020-08-01", "2020-05-01", "2020-05-01", "2020-07-01", "2020-07-01", "2020-08-01"
      ))
    )
  )

  # gap = 1
  res <- joinOverlap(
    cdm$x, start = "start_date", end = "end_date", by = c("pid", "def_id"), gap = 1
  ) |>
    dplyr::collect() |>
    dplyr::arrange(.data$pid, .data$def_id, .data$start_date)

  expect_true(nrow(res) == 5)
  expect_equal(
    res |> dplyr::arrange(.data$start_date, .data$end_date) |> dplyr::select("pid", "def_id", "start_date", "end_date"),
    dplyr::tibble(
      pid = c(1, 1, 2, 2, 2),
      def_id = c(1, 2, 1, 2, 1),
      start_date = as.Date(c(
        "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-06-01"
      )),
      end_date = as.Date(c(
        "2020-08-01", "2020-07-01", "2020-05-01", "2020-07-01", "2020-08-01"
      ))
    )
  )

  PatientProfiles::mockDisconnect(cdm)
})

test_that("splitOverlap", {
  testthat::skip_on_cran()
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
  cdm <- mockCohortConstructor(otherTables = list(x = x), con = connection(), writeSchema = writeSchema())

  expect_no_error(
    res <- splitOverlap(
      cdm$x, start = "start_date", end = "end_date", by = c("pid", "def_id")
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

  PatientProfiles::mockDisconnect(cdm)
})

test_that("intersectCohorts", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2)
  cdm <- cdm_local |> copyCdm()

  # mutually exclusive
  expect_no_error(cdm$cohort2 <- intersectCohorts(
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
      c("Initial qualifying events", "Mutually exclusive cohorts",
        "Initial qualifying events", "Mutually exclusive cohorts",
        "Initial qualifying events")
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$reason_id ==  c(1, 2, 1, 2, 1)
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$excluded_records ==  c(0, 3, 0, 0, 0)
  ))

  # not mutually exclusive and gap
  expect_no_error(cdm$cohort3 <- intersectCohorts(
    cohort = cdm$cohort1, name = "cohort3",
    mutuallyExclusive = FALSE, gap = 1
  ))
  expect_true(all(omopgenerics::settings(cdm$cohort3)$mutually_exclusive == FALSE))
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
      c("Initial qualifying events", "Initial qualifying events", "Initial qualifying events")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason_id ==  c(1, 1, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_records ==  c(4, 4, 2)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_subjects ==  c(3, 2, 2)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_records ==  c(0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_subjects ==  c(0, 0, 0)))

  # not enough cohorts provided
  expect_warning(
    cdm$cohort4 <- intersectCohorts(
      cohort = cdm$cohort1, name = "cohort4",
      cohortId = 1
    ), "At least 2 cohort id must be provided to do the intersection.")
  expect_equal(cdm$cohort1 %>%
                 omopgenerics::settings() %>%
                 dplyr::filter(cohort_definition_id == 1),
               cdm$cohort4 %>%
                 omopgenerics::settings())

  # all cohorts
  expect_no_error(cohort <- cdm$cohort1 |> intersectCohorts())

  PatientProfiles::mockDisconnect(cdm)
})

test_that("only return comb", {
  testthat::skip_on_cran()
  # combination = null
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2, seed = 2)
  cdm_local$cohort1 <- cdm_local$cohort1 |>
    dplyr::filter(cohort_end_date != as.Date("2015-04-17"))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort2 <- intersectCohorts(
    cohort = cdm$cohort1, name = "cohort2",
    mutuallyExclusive = FALSE, returnOnlyComb = TRUE
  )
  expect_true(nrow(dplyr::collect(cdm$cohort2)) == 0)
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$reason ==
      c("Initial qualifying events")
  ))
  expect_true(omopgenerics::attrition(cdm$cohort2)$reason_id == 1)
  expect_true(omopgenerics::attrition(cdm$cohort2)$number_records == 0)
  expect_true(omopgenerics::attrition(cdm$cohort2)$number_subjects == 0)
  expect_true(omopgenerics::attrition(cdm$cohort2)$excluded_records == 0)
  expect_true(omopgenerics::attrition(cdm$cohort2)$excluded_subjects == 0)

  # nUll combination, return individuals
  cdm$cohort4 <- intersectCohorts(
    cohort = cdm$cohort1, name = "cohort4",
    mutuallyExclusive = FALSE, returnOnlyComb = FALSE
  )
  expect_true(nrow(dplyr::collect(cdm$cohort4)) == nrow(dplyr::collect(cdm$cohort1)))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort4)$reason ==
      c("Initial qualifying events", "Initial qualifying events", "Initial qualifying events")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason_id == c(1, 1, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$number_records == c(4, 4, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$number_subjects == c(3, 2, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$excluded_records == c(0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$excluded_subjects == c(0, 0, 0)))
  expect_true(nrow(cdm$cohort1 |> dplyr::anti_join(cdm$cohort4, by = colnames(cdm$cohort4)) |> dplyr::collect()) == 0)

  # not null combination
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 3)
  cdm <- cdm_local |> copyCdm()
  cdm$cohort3 <- intersectCohorts(
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
      c("Initial qualifying events", "Initial qualifying events",
        "Initial qualifying events", "Initial qualifying events")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason_id == c(1, 1, 1, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_records == c(2, 1, 2, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_subjects ==  c(2, 1, 2, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_records == c(0, 0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_subjects == c(0, 0, 0, 0)))

  cdm$cohort4 <- intersectCohorts(
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

  PatientProfiles::mockDisconnect(cdm)
})

test_that("attrition and cohortId", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 4, seed = 2)
  cdm_local$person <- cdm_local$person |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("of_birth"), ~ as.numeric(.x)))
  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- cdm$cohort1 |>
    requireInDateRange(dateRange = as.Date(c("1990-01-01", "2025-01-01"))) |>
    requireSex(sex = "Female") |>
    requireAge(ageRange = list(c(0,40)))

  cdm$cohort1 <- intersectCohorts(
    cohort = cdm$cohort1, cohortId = 1:2,
    name = "cohort1", mutuallyExclusive = TRUE
  )
  expect_true(all(
    omopgenerics::attrition(cdm$cohort1)$reason ==
      c("Initial qualifying events",  "cohort_start_date after 1990-01-01",  "cohort_start_date before 2025-01-01",
        "Sex requirement: Female", "Age requirement: 0 to 40", "Mutually exclusive cohorts",
        "Initial qualifying events",  "cohort_start_date after 1990-01-01",  "cohort_start_date before 2025-01-01",
        "Sex requirement: Female", "Age requirement: 0 to 40", "Mutually exclusive cohorts",
        "Initial qualifying events" )
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason_id ==  c(1:6, 1:6, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$number_records ==
                    c(4, 4, 4, 1, 1, 1, 4, 4, 4, 0, 0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$number_subjects ==
                    c(3, 3, 3, 1, 1, 1, 2, 2, 2, 0, 0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$excluded_records ==
                    c(0, 0, 0, 3, 0, 0, 0, 0, 0, 4, 0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$excluded_subjects ==
                    c(0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0)))
  expect_true(all(omopgenerics::settings(cdm$cohort1)$cohort_name == c("cohort_1", "cohort_2", "cohort_1_cohort_2")))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("codelist", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 3) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort()
  cdm_local$concept <- dplyr::tibble(
    "concept_id" = c(1, 2, 3),
    "concept_name" = c("my concept 1", "my concept 2", "my concept 3"),
    "domain_id" = "Drug",
    "vocabulary_id" = NA,
    "concept_class_id" = NA,
    "concept_code" = NA,
    "valid_start_date" = NA,
    "valid_end_date" = NA
  )
  cdm_local$drug_exposure <- dplyr::tibble(
    "drug_exposure_id" = 1:17,
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 3, 3, 1, 2, 3, 3),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, 430, -10, 100, 123, -10, 1000),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, 400, -100, NA, 190, 123, 1500),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )
  cdm_local$observation_period <- cdm_local$observation_period|>
    dplyr::mutate(observation_period_start_date = as.Date("1990-01-01"), observation_period_end_date = as.Date("2020-01-01"))

  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- conceptCohort(cdm, conceptSet = list(c1 = c(1,3), c2 = c(2)), name = "cohort1")

  # intersect concept generated cohort
  cdm$cohort2 <- intersectCohorts(cdm$cohort1, name = "cohort2")
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("2009-12-22", "2010-01-01", "2010-01-11", "2010-05-31", "2012-01-21",
        "2012-01-21", "2012-09-27", "2014-02-09", "2014-02-09", "2014-12-06")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("2010-05-04", "2011-08-24", "2012-03-11", "2012-03-11", "2014-02-09",
        "2014-03-31", "2014-03-31", "2014-05-20", "2014-12-10", "2015-06-24")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("subject_id") %>% sort() == c(1, 1, 1, 1, 1, 1, 2, 3, 3, 3)
  ))
  codes <- attr(cdm$cohort2, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 4), rep("c2", 2))))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 1, 2, 2, 3, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort()== rep("index event", 6)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 2, 3, 3, 3)))

  # mutually esclusive
  cdm$cohort3 <- intersectCohorts(cdm$cohort1, mutuallyExclusive = TRUE, name = "cohort3")
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("2009-12-22", "2010-01-01", "2010-01-11", "2010-05-31", "2012-01-21",
        "2012-03-12", "2012-09-27", "2014-02-09", "2014-04-01", "2014-12-06")
  ))
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("2010-05-04", "2011-08-24", "2012-01-20", "2012-03-11", "2014-02-08",
        "2014-02-09", "2014-03-31", "2014-05-20", "2014-12-10", "2015-06-24")
  ))
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("subject_id") %>% sort() == c(1, 1, 1, 1, 1, 1, 2, 3, 3, 3)
  ))
  codes <- attr(cdm$cohort2, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 4), rep("c2", 2))))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 1, 2, 2, 3, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort()== rep("index event", 6)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 2, 3, 3, 3)))

  # only comb
  cdm$cohort4 <- intersectCohorts(cdm$cohort1, returnOnlyComb = TRUE, name = "cohort4")
  expect_true(all(
    cdm$cohort4 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c("2012-01-21", "2014-02-09")
  ))
  expect_true(all(
    cdm$cohort4 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("2012-03-11", "2014-03-31")
  ))
  expect_true(all(
    cdm$cohort4 %>% dplyr::pull("subject_id") %>% sort() == c(1, 1)
  ))
  codes <- attr(cdm$cohort4, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  # union concept + non concept cohorts
  cdm <- omopgenerics::bind(cdm$cohort, cdm$cohort1, name = "cohort5")
  cdm$cohort6 <- intersectCohorts(cdm$cohort5, name = "cohort6")
  codes <- attr(cdm$cohort6, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 8), rep("c2", 4))))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(rep(1, 4), rep(2, 4), rep(3, 4))))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 12)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(2, 2, 3, 3, 4, 5, 6, 6, 6, 7, 7, 7)))

  PatientProfiles::mockDisconnect(cdm)
})
