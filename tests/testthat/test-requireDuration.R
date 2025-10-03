test_that("require certain range of days for cohort entries", {
  skip_on_cran()

  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 2),
    subject_id = c(1, 1, 1, 2, 3),
    cohort_start_date = as.Date(c("2001-05-03",
                                  "2002-10-24",
                                  "2004-01-08",
                                  "1999-07-30",
                                  "2015-02-17")),
    cohort_end_date = as.Date(c("2001-05-04", # one day cohort
                                "2003-10-24",
                                "2006-01-08",
                                "2010-07-30",
                                "2015-02-17")) # zero day cohort
  )

  cdm <- omock::mockCdmFromTables(
    tables = list(
      "cohort" = cohort
    ),
    seed = 1
  )

  cdm <- copyCdm(cdm)

  # 0 to Inf shouldn't do anything except add a row to attrition
  expect_identical(cdm$cohort |>
                     dplyr::pull("cohort_start_date") |>
                     sort(),
                   cdm$cohort |>
                     requireDuration(daysInCohort = c(1, Inf),
                                     name = "new_cohort") |>
                     dplyr::pull("cohort_start_date") |>
                     sort())

  expect_true("Keep records with duration 1 to Inf days" %in%
                (cdm$cohort |>
                   requireDuration(daysInCohort = c(1, Inf),
                                   name = "new_cohort") |>
                   attrition() |>
                   dplyr::pull("reason") |>
                   unique()))

  # drop last record
  expect_true(!"2015-02-17" %in% (cdm$cohort |>
                                    requireDuration(daysInCohort = c(2, Inf),
                                                    name = "new_cohort") |>
                                    dplyr::pull("cohort_start_date")))
  expect_true("Keep records with duration 2 to Inf days" %in%
                (cdm$cohort |>
                   requireDuration(daysInCohort = c(2, Inf),
                                   name = "new_cohort") |>
                   attrition() |>
                   dplyr::pull("reason") |>
                   unique()))

  # keep if not cohort of interest
  expect_true("2015-02-17" %in% (cdm$cohort |>
                                   requireDuration(daysInCohort = c(2, Inf),
                                                   cohortId = 1,
                                                   name = "new_cohort") |>
                                   dplyr::pull("cohort_start_date")))

  # keep only one day cohort
  expect_true(cdm$cohort |>
                requireDuration(daysInCohort = c(2, 2),
                                name = "new_cohort") |>
                dplyr::tally() |>
                dplyr::pull("n") == 1)

  expect_true("2001-05-03" %in%  (cdm$cohort |>
                                    requireDuration(daysInCohort = c(2, 2),
                                                    name = "new_cohort") |>
                                    dplyr::pull("cohort_start_date")))

  expect_true(cdm$cohort |>
                requireDuration(daysInCohort = c(2, 2),
                                cohortId = 1,
                                name = "new_cohort") |>
                dplyr::tally() |>
                dplyr::pull("n") == 2)

  # expect error
  expect_error(cdm$cohort |>
                 requireDuration(daysInCohort = c(20, 1)))
  expect_error(cdm$cohort |>
                 requireDuration(daysInCohort = c(1, 2, 3)))
  expect_error(cdm$cohort |>
                 requireDuration(daysInCohort = c(NA, 2)))
  expect_error(cdm$cohort |>
                 requireDuration(daysInCohort = c(2, NA)))
  expect_error(cdm$cohort |>
                 requireDuration(daysInCohort = c(-1, 2)))
  expect_error(cdm$cohort |>
                 requireDuration(daysInCohort = c(-1)))
  expect_error(cdm$cohort |>
                 requireDuration(daysInCohort = c(Inf)))

})

test_that("trim duration", {
  skip_on_cran()

  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 2),
    subject_id = c(1, 1, 1, 2, 3),
    cohort_start_date = as.Date(c("2001-05-03",
                                  "2002-10-24",
                                  "2004-01-08",
                                  "1999-07-30",
                                  "2015-02-17")),
    cohort_end_date = as.Date(c("2001-05-04", # one day cohort
                                "2003-10-24",
                                "2006-01-08",
                                "2010-07-30",
                                "2015-02-17")) # zero day cohort
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort" = cohort)) |>
    copyCdm()

  # 0 to Inf shouldn't do anything except add a row to attrition
  expect_identical(cdm$cohort |>
                     dplyr::pull("cohort_start_date") |>
                     sort(),
                   cdm$cohort |>
                     trimDuration(daysInCohort = c(1, Inf),
                                  name = "new_cohort") |>
                     dplyr::pull("cohort_start_date") |>
                     sort())

  expect_true("Trim records to 1 to Inf days following entry" %in%
                (cdm$cohort |>
                   trimDuration(daysInCohort = c(1, Inf),
                                name = "new_cohort") |>
                   attrition() |>
                   dplyr::pull("reason") |>
                   unique()))

  # drop last record
  # other records get a day added to cohort start
  expect_true(!"2015-02-17" %in% (cdm$cohort |>
                                    trimDuration(daysInCohort = c(2, Inf),
                                                 name = "new_cohort") |>
                                    dplyr::pull("cohort_start_date")))
  expect_true("2001-05-04" %in% (cdm$cohort |>
                                   trimDuration(daysInCohort = c(2, Inf),
                                                name = "new_cohort") |>
                                   dplyr::pull("cohort_start_date")))

  expect_true("Trim records to 2 to Inf days following entry" %in%
                (cdm$cohort |>
                   trimDuration(daysInCohort = c(2, Inf),
                                name = "new_cohort") |>
                   attrition() |>
                   dplyr::pull("reason") |>
                   unique()))

  # keep if not cohort of interest
  expect_true("2015-02-17" %in% (cdm$cohort |>
                                   trimDuration(daysInCohort = c(2, Inf),
                                                cohortId = 1,
                                                name = "new_cohort") |>
                                   dplyr::pull("cohort_start_date")))

  # subtracting days only from end
  # only if cohort end is currently more
  cdm$new_cohort <- cdm$cohort |>
    trimDuration(daysInCohort = c(1, 4),
                 name = "new_cohort")
  expect_true(cdm$new_cohort |>
                dplyr::collect() |>
                nrow() == 5)
  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_start_date") |>
                     sort(),
                   as.Date(c(
                     "1999-07-30",
                     "2001-05-03",
                     "2002-10-24",
                     "2004-01-08",
                     "2015-02-17"
                   )))
  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_end_date") |>
                     sort(),
                   as.Date(c(
                     "1999-08-02",
                     "2001-05-04",
                     "2002-10-27",
                     "2004-01-11",
                     "2015-02-17"
                   )))

  # only cohort 1
  cdm$new_cohort <- cdm$cohort |>
    trimDuration(daysInCohort = c(1, 4),
                 cohortId = 1,
                 name = "new_cohort")
  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_start_date") |>
                     sort(),
                   as.Date(c(
                     "1999-07-30",
                     "2001-05-03",
                     "2002-10-24",
                     "2004-01-08",
                     "2015-02-17"
                   )))
  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_end_date") |>
                     sort(),
                   as.Date(c(
                     "1999-08-02",
                     "2001-05-04",
                     "2002-10-27",
                     "2004-01-11",
                     "2015-02-17"
                   )))

  cdm$new_cohort <- cdm$cohort |>
    trimDuration(daysInCohort = c(1, 4),
                 cohortId = 2,
                 name = "new_cohort")
  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_end_date") |>
                     sort(),
                   as.Date(c(
                     "2001-05-04",
                     "2003-10-24",
                     "2006-01-08",
                     "2010-07-30",
                     "2015-02-17 "
                   )))

  # adding days to start and taking days off end
  cdm$new_cohort <- cdm$cohort |>
    trimDuration(daysInCohort = c(4, 6),
                 name = "new_cohort")
  expect_true(cdm$new_cohort |>
                dplyr::collect() |>
                nrow() == 3)

  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_start_date") |>
                     sort(),
                   as.Date(c(
                     "1999-08-02",
                     "2002-10-27",
                     "2004-01-11"
                   )) )
  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_end_date") |>
                     sort(),
                   as.Date(c(
                     "1999-08-04",
                     "2002-10-29",
                     "2004-01-13"
                   )))


  cdm$new_cohort <- cdm$cohort |>
    trimDuration(daysInCohort = c(4, 4),
                 cohortId = 1,
                 name = "new_cohort")
  expect_true(cdm$new_cohort |>
                dplyr::collect() |>
                nrow() == 4)

  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_start_date") |>
                     sort(),
                   as.Date(c(
                     "1999-08-02",
                     "2002-10-27",
                     "2004-01-11",
                     "2015-02-17"
                   )) )
  expect_identical(cdm$new_cohort |>
                     dplyr::pull("cohort_end_date") |>
                     sort(),
                   as.Date(c(
                     "1999-08-02",
                     "2002-10-27",
                     "2004-01-11",
                     "2015-02-17"
                   )))



  # expect error
  expect_error(cdm$cohort |>
                 trimDuration(daysInCohort = c(20, 1)))
  expect_error(cdm$cohort |>
                 trimDuration(daysInCohort = c(1, 2, 3)))
  expect_error(cdm$cohort |>
                 trimDuration(daysInCohort = c(NA, 2)))
  expect_error(cdm$cohort |>
                 trimDuration(daysInCohort = c(2, NA)))
  expect_error(cdm$cohort |>
                 trimDuration(daysInCohort = c(-1, 2)))
  expect_error(cdm$cohort |>
                 trimDuration(daysInCohort = c(-1)))
  expect_error(cdm$cohort |>
                 trimDuration(daysInCohort = c(Inf)))

})
