test_that("getIdentifier", {
  x <- dplyr::tibble(a = 1)

  expect_no_error(id <- getIdentifier(x))

  expect_true(getIdentifier(x, len = 5) |> length() == 5)
  expect_true(getIdentifier(x, nchar = 2) |> nchar() == 2)
  expect_true(getIdentifier(x, prefix = "id_", nchar = 2) |> nchar() == 5)
  expect_true(getIdentifier(x, prefix = "id_", nchar = 8) |> substr(1, 3) == "id_")
})

test_that("joinOverlap", {
  # testthat::skip_on_cran()
  # x <- dplyr::tibble(
  #   start_date = as.Date(c(
  #     "2020-01-01", "2020-03-01", "2020-06-01", "2020-02-01", "2020-05-02",
  #     "2020-03-01", "2020-06-01", "2020-04-01"
  #   )),
  #   end_date = as.Date(c(
  #     "2020-04-01", "2020-06-01", "2020-08-01", "2020-05-01", "2020-07-01",
  #     "2020-05-01", "2020-08-01", "2020-07-01"
  #   )),
  #   pid = c(1, 1, 1, 1, 1, 2, 2, 2),
  #   def_id = c(1, 1, 1, 2, 2, 1, 1, 2)
  # )
  #
  # cdm <- mockCohortConstructor(otherTables = list(x = x),
  #                              con = connection(),
  #                              writeSchema = writeSchema())
  # # gap = 0
  # res <- joinOverlap(
  #   cdm$x, startDate = "start_date", endDate = "end_date", by = c("pid", "def_id")
  # ) |>
  #   dplyr::collect() |>
  #   dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  # expect_true(nrow(res) == 6)
  # expect_equal(
  #   res |> dplyr::arrange(.data$start_date, .data$end_date) |> dplyr::select("pid", "def_id", "start_date", "end_date"),
  #   dplyr::tibble(
  #     pid = c(1, 1, 2, 2, 1, 2),
  #     def_id = c(1, 2, 1, 2, 2, 1),
  #     start_date = as.Date(c(
  #       "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-02", "2020-06-01"
  #     )),
  #     end_date = as.Date(c(
  #       "2020-08-01", "2020-05-01", "2020-05-01", "2020-07-01", "2020-07-01", "2020-08-01"
  #     ))
  #   )
  # )
  #
  # # gap = 1
  # res <- joinOverlap(
  #   cdm$x, start = "start_date", end = "end_date", by = c("pid", "def_id"), gap = 1
  # ) |>
  #   dplyr::collect() |>
  #   dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  #
  # expect_true(nrow(res) == 5)
  # expect_equal(
  #   res |> dplyr::arrange(.data$start_date, .data$end_date) |> dplyr::select("pid", "def_id", "start_date", "end_date"),
  #   dplyr::tibble(
  #     pid = c(1, 1, 2, 2, 2),
  #     def_id = c(1, 2, 1, 2, 1),
  #     start_date = as.Date(c(
  #       "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-06-01"
  #     )),
  #     end_date = as.Date(c(
  #       "2020-08-01", "2020-07-01", "2020-05-01", "2020-07-01", "2020-08-01"
  #     ))
  #   )
  # )
  #
  # PatientProfiles::mockDisconnect(cdm)
})

test_that("splitOverlap", {
  # testthat::skip_on_cran()
  # x <- dplyr::tibble(
  #   start_date = as.Date(c(
  #     "2020-01-01", "2020-03-01", "2020-06-01", "2020-02-01", "2020-05-02",
  #     "2020-03-01", "2020-06-01", "2020-04-01"
  #   )),
  #   end_date = as.Date(c(
  #     "2020-04-01", "2020-06-01", "2020-08-01", "2020-05-01", "2020-07-01",
  #     "2020-05-01", "2020-08-01", "2020-07-01"
  #   )),
  #   pid = c(1, 1, 1, 1, 1, 2, 2, 2),
  #   def_id = c(1, 1, 1, 2, 2, 1, 2, 1)
  # )
  # cdm <- mockCohortConstructor(otherTables = list(x = x), con = connection(), writeSchema = writeSchema())
  #
  # expect_no_error(
  #   res <- splitOverlap(
  #     cdm$x, start = "start_date", end = "end_date", by = c("pid", "def_id")
  #   ) |>
  #     dplyr::collect() |>
  #     dplyr::arrange(.data$pid, .data$def_id, .data$start_date)
  # )
  # expect_true(nrow(res) == 11)
  # expect_identical(
  #   res,
  #   dplyr::tibble(
  #     pid = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
  #     def_id = c(1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2),
  #     start_date = as.Date(c(
  #       "2020-01-01", "2020-03-01", "2020-04-02", "2020-06-01", "2020-06-02",
  #       "2020-02-01", "2020-05-02", "2020-03-01", "2020-04-01", "2020-05-02",
  #       "2020-06-01"
  #     )),
  #     end_date = as.Date(c(
  #       "2020-02-29", "2020-04-01", "2020-05-31", "2020-06-01", "2020-08-01",
  #       "2020-05-01", "2020-07-01", "2020-03-31", "2020-05-01", "2020-07-01",
  #       "2020-08-01"
  #     ))
  #   )
  # )
  #
  # PatientProfiles::mockDisconnect(cdm)
})

test_that("intersectCohorts", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 2)
  cdm <- cdm_local |> copyCdm()

  # returnNonOverlappingCohorts
  expect_no_error(cdm$cohort2 <- intersectCohorts(
    cohort = cdm$cohort1, name = "cohort2",
    returnNonOverlappingCohorts = TRUE
  ))
  expect_equal(omopgenerics::settings(cdm$cohort2)$non_overlapping,c(NA, TRUE, TRUE))
  expect_true(cdm$cohort2 %>% dplyr::tally() %>% dplyr::pull() == 10)
  expect_true(all(
    CDMConnector::cohortCount(cdm$cohort2) %>%
      dplyr::arrange(.data$number_records) %>%
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
    omopgenerics::attrition(cdm$cohort2)$reason |> sort() ==
      c("Initial qualifying events", "Initial qualifying events",
        "Initial qualifying events", "Trim to non overlapping entries",
        "Trim to non overlapping entries")
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$reason_id |> sort() ==  c(1, 1, 1, 2, 2)
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$excluded_records |> sort() ==  c(0, 0, 0, 0, 3)
  ))

  # not overlap, keep original and gap
  expect_no_error(cdm$cohort3 <- intersectCohorts(
    cohort = cdm$cohort1, name = "cohort3",
    returnNonOverlappingCohorts = FALSE, gap = 1,
    keepOriginalCohorts = TRUE
  ))
  expect_false("non_overlapping" %in% colnames(settings(cdm$cohort3)))
  expect_true(cdm$cohort3 %>% dplyr::tally() %>% dplyr::pull() == 10)
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort3, 2))
  expect_equal(collectCohort(cdm$cohort1, 2), collectCohort(cdm$cohort3, 3))
  expect_true(all(
    omopgenerics::cohortCount(cdm$cohort3) %>%
      dplyr::pull("number_records") |> sort() == c(2, 4, 4)
  ))
  expect_true(all(
    omopgenerics::cohortCount(cdm$cohort3) %>%
      dplyr::pull("number_subjects") |>
      sort() == c(2, 2, 3)
  ))
  expect_true(nrow(omopgenerics::settings(cdm$cohort3)) == 3)
  expect_true(all(
    cdm$cohort3 %>%
      dplyr::pull("cohort_start_date") %>%
      sort() ==
      c("1997-10-22", "2000-06-23", "2001-03-30", "2001-03-30", "2001-07-16",
        "2001-12-04", "2003-06-15", "2015-03-05", "2015-03-25", "2015-03-25")
  ))
  expect_true(all(
    cdm$cohort3 %>%
      dplyr::pull("cohort_end_date") %>%
      sort() ==
      c("1999-05-28", "2001-07-15", "2001-12-03", "2003-06-14", "2005-11-23",
        "2005-11-23", "2006-09-27", "2015-04-14", "2015-04-14", "2015-07-06")
  ))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort3)$reason ==
      c("Initial qualifying events", "Initial qualifying events", "Initial qualifying events")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason_id ==  c(1, 1, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_records ==  c(2, 4, 4)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_subjects ==  c(2, 3, 2)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_records ==  c(0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_subjects ==  c(0, 0, 0)))

  # not enough cohorts provided
  expect_error(
    cdm$cohort4 <- intersectCohorts(
      cohort = cdm$cohort1, name = "cohort4",
      cohortId = 1
    ))

  # all cohorts
  expect_no_error(cohort <- cdm$cohort1 |> intersectCohorts())

  PatientProfiles::mockDisconnect(cdm)
})

test_that("keepOriginalCohorts", {
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
    returnNonOverlappingCohorts = FALSE, keepOriginalCohorts = TRUE
  )
  expect_true(nrow(dplyr::collect(cdm$cohort2)) == nrow(dplyr::collect(cdm$cohort1)))
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort2, 2))
  expect_equal(collectCohort(cdm$cohort1, 2), collectCohort(cdm$cohort2, 3))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort2)$reason ==
      rep("Initial qualifying events", 3)
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$reason_id == rep(1, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$number_records == c(0, 4, 4)))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$number_subjects == c(0, 3, 2)))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$excluded_records == rep(0, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort2)$excluded_subjects == rep(0, 3)))

  # nUll combination, return individuals
  cdm$cohort4 <- intersectCohorts(
    cohort = cdm$cohort1, name = "cohort4",
    returnNonOverlappingCohorts = FALSE, keepOriginalCohorts = FALSE
  )
  expect_true(nrow(dplyr::collect(cdm$cohort4)) == 0)
  expect_true(omopgenerics::attrition(cdm$cohort4)$reason == "Initial qualifying events")
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason_id == 1))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$number_records == 0))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$number_subjects == 0))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$excluded_records == 0))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$excluded_subjects == 0))
  expect_true(settings(cdm$cohort4)$cohort_name == "cohort_1_cohort_2")


  # not null combination
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 3)
  cdm <- cdm_local |> copyCdm()
  cdm$cohort3 <- intersectCohorts(
    cohort = cdm$cohort1, name = "cohort3",
    returnNonOverlappingCohorts = FALSE, keepOriginalCohorts = TRUE, gap = 1
  )
  expect_true(all(settings(cdm$cohort3)$cohort_name |> sort() == c(
    "cohort_1", "cohort_1_cohort_2_cohort_3", "cohort_2", "cohort_3"
  )))
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort3, 2))
  expect_equal(collectCohort(cdm$cohort1, 2), collectCohort(cdm$cohort3, 3))
  expect_equal(collectCohort(cdm$cohort1, 3), collectCohort(cdm$cohort3, 4))
  expect_equal(
    cdm$cohort3 |>
      dplyr::filter(.data$cohort_definition_id == 1) |>
      dplyr::collect() |>
      dplyr::pull(.data$cohort_start_date),
    as.Date("2015-03-25")
  )
  expect_equal(
    cdm$cohort3 |>
      dplyr::filter(.data$cohort_definition_id == 1) |>
      dplyr::collect() |>
      dplyr::pull(cohort_end_date),
    as.Date("2015-04-14")
  )
  expect_true(nrow(omopgenerics::settings(cdm$cohort3)) == 4)
  expect_equal(omopgenerics::settings(cdm$cohort3)$cohort_1, c(1, NA, NA, NA))
  expect_equal(omopgenerics::settings(cdm$cohort3)$cohort_2, c(1, NA, NA, NA))
  expect_equal(omopgenerics::settings(cdm$cohort3)$cohort_3, c(1, NA, NA, NA))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort3)$reason ==
      c("Initial qualifying events", "Initial qualifying events",
        "Initial qualifying events", "Initial qualifying events")
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$reason_id == c(1, 1, 1, 1)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_records == c(1, 4, 4, 4)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$number_subjects ==  c(1, 3, 2, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_records == c(0, 0, 0, 0)))
  expect_true(all(omopgenerics::attrition(cdm$cohort3)$excluded_subjects == c(0, 0, 0, 0)))

  cdm$cohort4 <- intersectCohorts(
    cohort = cdm$cohort1, name = "cohort4",
    returnNonOverlappingCohorts = TRUE, keepOriginalCohorts = TRUE, gap = 1
  )
  expect_true(nrow(settings(cdm$cohort4)) == 7)
  expect_equal(settings(cdm$cohort4)$non_overlapping, c(NA, TRUE, TRUE, TRUE, NA, NA, NA))
  expect_equal(collectCohort(cdm$cohort1, 1), collectCohort(cdm$cohort4, 5))
  expect_equal(collectCohort(cdm$cohort1, 2), collectCohort(cdm$cohort4, 6))
  expect_equal(collectCohort(cdm$cohort1, 3), collectCohort(cdm$cohort4, 7))
  expect_equal(
    cdm$cohort4 |>
      dplyr::filter(.data$cohort_definition_id %in% 1:4) |>
      dplyr::collect() |>
      dplyr::pull(.data$cohort_start_date),
    as.Date(c('2015-01-19', '2015-03-25', '2000-06-23', '2005-11-24',
              '1999-12-19', '1994-06-17', '1999-05-29', '2015-07-07'))
  )
  expect_equal(
    cdm$cohort4 |>
      dplyr::filter(.data$cohort_definition_id %in% 1:4) |>
      dplyr::collect() |>
      dplyr::pull(cohort_end_date),
    as.Date(c('2015-03-04', '2015-04-14', '2001-03-29', '2006-09-27',
              '2001-08-26', '1997-10-21', '2007-08-06', '2015-09-14'))
  )
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$reason_id |> sort() == c(rep(1, 7), rep(2, 2))))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$number_records |> sort() == c(1, 2, rep(4, 6), 5)))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$number_subjects |> sort() ==  c(1, 1, 2, 2, rep(3, 5))))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$excluded_records |> sort() == c(-1, rep(0, 7), 2)))
  expect_true(all(omopgenerics::attrition(cdm$cohort4)$excluded_subjects |> sort() == c(rep(0, 8), 1)))

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
    cohort = cdm$cohort1, cohortId = c("cohort_1", "cohort_2"),
    name = "cohort1", returnNonOverlappingCohorts = TRUE,
    keepOriginalCohorts = FALSE
  )
  expect_true(nrow(settings(cdm$cohort1)) == 3)
  expect_equal(settings(cdm$cohort1)$non_overlapping, c(NA, TRUE, TRUE))
  expect_true(all(
    omopgenerics::attrition(cdm$cohort1)$reason |> sort() ==
      c(rep("Age requirement: 0 to 40", 2), rep("cohort_start_date after 1990-01-01", 2),
        rep("cohort_start_date before 2025-01-01", 2), rep("Initial qualifying events", 3),
        rep("Sex requirement: Female", 2), rep("Trim to non overlapping entries", 2))
  ))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$reason_id ==  c(1, 1:6, 1:6)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$number_records |> sort() ==
                    c(0, 0, 0, 0, 1, 1, 1, 4, 4, 4, 4, 4, 4)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$number_subjects |> sort() ==
                    c(0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$excluded_records |> sort() ==
                    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 4)))
  expect_true(all(omopgenerics::attrition(cdm$cohort1)$excluded_subjects |> sort() ==
                    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2)))
  expect_true(all(omopgenerics::settings(cdm$cohort1)$cohort_name |> sort() ==
                    c("cohort_1_cohort_2", "only_in_cohort_1", "only_in_cohort_2")))

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
      c("2012-01-21", "2014-02-09")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c("2012-03-11", "2014-03-31")
  ))
  expect_true(all(
    cdm$cohort2 %>% dplyr::pull("subject_id") %>% sort() == c(1, 1)
  ))
  codes <- attr(cdm$cohort2, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  # mutually esclusive
  cdm$cohort3 <- intersectCohorts(cdm$cohort1, returnNonOverlappingCohorts = TRUE, name = "cohort3")
  expect_equal(collectCohort(cdm$cohort3, 1), collectCohort(cdm$cohort2, 1))
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("cohort_start_date") %>% sort() ==
      c('2009-12-22', '2010-01-01', '2010-01-11', '2010-05-31', '2012-01-21',
        '2012-03-12', '2012-09-27', '2014-02-09', '2014-04-01', '2014-12-06')
  ))
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("cohort_end_date") %>% sort() ==
      c('2010-05-04', '2011-08-24', '2012-01-20', '2012-03-11', '2014-02-08',
        '2014-02-09', '2014-03-31', '2014-05-20', '2014-12-10', '2015-06-24')
  ))
  expect_true(all(
    cdm$cohort3 %>% dplyr::pull("subject_id") %>% sort() == c(1, 1, 1, 1, 1, 1, 2, 3, 3, 3)
  ))
  codes <- attr(cdm$cohort3, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 4), rep("c2", 2))))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 1, 2, 2, 3, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort()== rep("index event", 6)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1, 2, 2, 3)))

  # only comb
  cdm$cohort4 <- intersectCohorts(cdm$cohort1, keepOriginalCohorts = FALSE, name = "cohort4")
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
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  PatientProfiles::mockDisconnect(cdm)
})
