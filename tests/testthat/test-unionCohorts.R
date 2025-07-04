test_that("unionCohorts works", {
  skip_on_cran()

  obs <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4),
    person_id = c(1, 2, 3, 4),
    observation_period_start_date = as.Date(c(
      "2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09"
    )),
    observation_period_end_date = as.Date(c(
      "2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31"
    )),
    "period_type_concept_id" = NA_integer_
  )

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1,
                             2, 2, 2, 2,
                             3, 3, 3, 3,
                             4, 4, 4, 4),
    subject_id = c(1, 1, 2, 3,
                   1, 1, 1, 1,
                   1, 1, 2, 3,
                   1, 2, 3, 3),
    cohort_start_date = as.Date(c(
      "2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25",
      "2001-03-24", "2001-11-28", "2002-01-30", "2002-06-13",
      "2001-08-30", "2002-01-09", "1999-06-04", "2015-01-22",
      "2002-11-30", "2000-06-07", "2015-04-05", "2015-04-11"
    )),
    cohort_end_date = as.Date(c(
      "2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30",
      "2001-11-27", "2002-01-29", "2002-06-12", "2005-01-15",
      "2002-01-08", "2007-01-17", "2002-06-07", "2015-06-01",
      "2009-06-12", "2002-02-01", "2015-04-10", "2015-06-22"
    ))
  )


  cdm_local <- omock::mockCdmFromTables(
    tables = list(
      "cohort1" = cohort_1
    ),
    seed = 1
  )

  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local, name = "observation_period", table = obs)

  cdm <- cdm_local |> copyCdm()
  # simple example
  cdm$cohort2 <- unionCohorts(cdm$cohort1, name = "cohort2")
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1999-05-03", "2001-03-24", "2015-01-22")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2002-06-07", "2009-06-12", "2015-06-22")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("subject_id") |> sort() == 1:3
  ))
  expect_true(all(attrition(cdm$cohort2) ==
                    dplyr::tibble(
                      cohort_definition_id = 1,
                      number_records = 3,
                      number_subjects = 3,
                      reason_id = 1,
                      reason = "Initial qualifying events",
                      excluded_records = 0,
                      excluded_subjects = 0
                    )))
  expect_true(settings(cdm$cohort2)$cohort_name == "cohort_1_cohort_2_cohort_3_cohort_4")

  # choose cohort Id
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

  cdm$cohort4 <- unionCohorts(cdm$cohort1, cohortId = c("cohort_1", "cohort_2"), name = "cohort4")
  expect_identical(collectCohort(cdm$cohort3, 1), collectCohort(cdm$cohort4, 1))
  expect_identical(collectCohort(cdm$cohort3, 2), collectCohort(cdm$cohort4, 2))

  # union 2 empty cohorts
  cdm$cohort5 <- conceptCohort(cdm = cdm, conceptSet = list("a"= 1L, "b" = 2L), name = "cohort5")
  cdm$cohort6 <- cdm$cohort5 |> unionCohorts(name = "cohort6")
  expect_true(nrow(attrition(cdm$cohort6)) == 1)
  expect_true(attrition(cdm$cohort6)$number_records == 0)

  cdm$cohort5 <- cdm$cohort5 |> unionCohorts(name = "cohort5")

  # TEST same name and keep cohorts
  cdm$cohort1 <- cdm$cohort1 |>
    CohortConstructor::unionCohorts(name = "cohort1", keepOriginalCohorts = TRUE)
  expect_true(all(settings(cdm$cohort1)$cohort_name %in% c("cohort_1", "cohort_2", "cohort_3", "cohort_4", "cohort_1_cohort_2_cohort_3_cohort_4")))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("gap and name works", {
  skip_on_cran()

  obs <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4),
    person_id = c(1, 2, 3, 4),
    observation_period_start_date = as.Date(c(
      "2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09"
    )),
    observation_period_end_date = as.Date(c(
      "2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31"
    )),
    "period_type_concept_id" = NA_integer_
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4),
    gender_concept_id = c(8532, 8507, 8507, 8507),
    year_of_birth = c(1997, 1963, 1986, 1978),
    month_of_birth = c(8, 1, 3, 11),
    day_of_birth = c(22, 27, 10, 8),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(
      1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4, 4
    ),
    subject_id = c(
      1, 1, 1, 2, 2, 2, 4, 4,
      1, 1, 3, 3, 4, 4, 4, 4,
      2, 3, 3, 3, 3, 3, 3, 4,
      1, 1, 2, 2, 3, 3, 3, 4
    ),
    cohort_start_date = as.Date(c(
      "2002-08-29", "2002-12-30", "2004-09-01", "1999-07-26", "1999-08-16", "1999-12-04", "1994-12-29", "1995-01-27",
      "2003-05-08", "2003-10-04", "2015-03-11", "2015-03-20", "1991-04-29", "1991-07-14", "1991-08-15", "1991-10-21",
      "2000-06-17", "2015-02-04", "2015-02-13", "2015-02-16", "2015-02-27", "2015-03-07", "2015-03-26", "1993-08-15",
      "2002-05-31", "2003-02-07", "1999-08-22", "2000-06-06", "2015-03-08", "2015-03-14", "2015-03-18", "1993-03-10"
    )),
    cohort_end_date = as.Date(c(
      "2002-12-29", "2004-08-31", "2007-03-24", "1999-08-15", "1999-12-03", "2001-10-20", "1995-01-26", "1995-09-11",
      "2003-10-03", "2007-06-11", "2015-03-19", "2015-05-16", "1991-07-13", "1991-08-14", "1991-10-20", "1993-11-13",
      "2001-03-23", "2015-02-12", "2015-02-15", "2015-02-26", "2015-03-06", "2015-03-25", "2015-07-28", "1994-05-05",
      "2003-02-06", "2005-01-06", "2000-06-05", "2001-07-23", "2015-03-13", "2015-03-17", "2015-06-24", "1993-04-15"
    ))
  )


  cdm_local <- omock::mockCdmFromTables(
    tables = list(
      "cohort" = cohort_1
    ),
    seed = 1
  )

  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local, name = "observation_period", table = obs)

  cdm_local <- omopgenerics::insertTable(
    cdm = cdm_local, name = "person", table = person)


  cdm_local$cohort <- cdm_local$cohort |>
    dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::filter(id %% 2 == 0) |>
    dplyr::select(-id)
  cdm_local$cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2),
    subject_id = c(1, 1, 1),
    cohort_start_date = as.Date(c("2000-07-01", "2000-07-10", "2000-07-22")),
    cohort_end_date = as.Date(c("2000-07-02", "2000-07-20", "2000-08-22"))
  )
  cdm <- cdm_local |> copyCdm()
  cdm$cohort1 <- cdm$cohort1 |> omopgenerics::newCohortTable()

  # gap
  cdm$cohort2 <- unionCohorts(cdm$cohort1, gap = 2,  name = "cohort2")
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("2000-07-01", "2000-07-10")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2000-07-02", "2000-08-22")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("subject_id") |> sort() == 1
  ))
  expect_true(all(attrition(cdm$cohort2) ==
                    dplyr::tibble(
                      cohort_definition_id = 1,
                      number_records = 2,
                      number_subjects = 1,
                      reason_id = 1,
                      reason = "Initial qualifying events",
                      excluded_records = 0,
                      excluded_subjects = 0
                    )))
  expect_true(settings(cdm$cohort2)$cohort_name == "cohort_1_cohort_2")


  # names
  cdm$cohort <- unionCohorts(cdm$cohort, gap = 2,  cohortName = "test")
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1991-07-14", "1991-10-21", "1995-01-27", "1999-07-26", "1999-08-22",
        "2002-08-29", "2003-02-07", "2015-02-04", "2015-02-16", "2015-03-07")
  ))
  expect_true(all(
    cdm$cohort |> dplyr::pull("cohort_end_date") |> sort() ==
      c("1991-08-14", "1994-05-05", "1995-09-11", "1999-08-15", "2001-07-23",
        "2002-12-29", "2007-06-11", "2015-02-12", "2015-02-26", "2015-07-28")
  ))
  expect_true(all(
    cdm$cohort |> dplyr::pull("subject_id") |> sort() == c(1, 1, rep(2, 2), rep(3, 3), rep(4, 3))
  ))
  expect_true(all(attrition(cdm$cohort) ==
                    dplyr::tibble(
                      cohort_definition_id = 1,
                      number_records = 10,
                      number_subjects = 4,
                      reason_id = 1,
                      reason = "Initial qualifying events",
                      excluded_records = 0,
                      excluded_subjects = 0
                    )))
  expect_true(settings(cdm$cohort)$cohort_name == "test")

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("Expected behaviour", {
  testthat::skip_on_cran()
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort"), numberCohorts = 4, seed = 8, recordPerson = 2)
  cdm <- cdm_local |> copyCdm()
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                           cohortId = 1,
                           gap = 0,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                           cohortId = NULL,
                           gap = -1,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                           cohortId = NULL,
                           gap = NA,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                           cohortId = NULL,
                           gap = Inf,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_error(
    cohort <- unionCohorts(cdm$cohort,
                           cohortId = "1",
                           gap = 1,
                           cohortName = NULL,
                           name = "cohort1")
  )
  expect_warning(
    cohort <- unionCohorts(cdm$cohort,
                           cohortId = NULL,
                           gap = 1,
                           cohortName = "hOLA",
                           name = "cohort1")
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("test codelist", {
  testthat::skip_on_cran()

  obs <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4),
    person_id = c(1, 2, 3, 4),
    observation_period_start_date = as.Date(c(
      "2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09"
    )),
    observation_period_end_date = as.Date(c(
      "2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31"
    )),
    "period_type_concept_id" = NA_integer_
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4),
    gender_concept_id = c(8532, 8507, 8507, 8507),
    year_of_birth = c(1997, 1963, 1986, 1978),
    month_of_birth = c(8, 1, 3, 11),
    day_of_birth = c(22, 27, 10, 8),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c("2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25")),
    cohort_end_date = as.Date(c("2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30"))
  )

  cdm_local <- omock::mockCdmFromTables(
    tables = list(
      "person" = person,
      "cohort" = cohort_1
    ),
    seed = 1
  )

  cdm_local <- omopgenerics::insertTable(cdm = cdm_local,
                                         name = "observation_period", table = obs)

  cdm_local <- omopgenerics::insertTable(cdm = cdm_local,
                                         name = "person", table = person)

  # cdm_local <- omock::mockCdmReference() |>
  #   omock::mockPerson(n = 4,seed = 1) |>
  #   omock::mockObservationPeriod(seed = 1) |>
  #   omock::mockCohort(seed = 1)
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
    "person_id" = c(1, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 4, 4, 1, 2, 3, 4),
    "drug_concept_id" = c(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3),
    "drug_exposure_start_date" = c(0, 300, 1500, 750, 10, 800, 150, 1800, 1801, 1802, 1803, 430, -10, 100, 123, -10, 1000),
    "drug_exposure_end_date" = c(400, 800, 1600, 1550, 2000, 1000, 600, 1801, 1802, 1803, 1804, 400, -100, NA, 190, 123, 1500),
    "drug_type_concept_id" = 1
  ) |>
    dplyr::mutate(
      "drug_exposure_start_date" = as.Date(.data$drug_exposure_start_date, origin = "2010-01-01"),
      "drug_exposure_end_date" = as.Date(.data$drug_exposure_end_date, origin = "2010-01-01")
    )
  cdm_local$observation_period <- cdm_local$observation_period |>
    dplyr::mutate(observation_period_start_date = as.Date("1990-01-01"), observation_period_end_date = as.Date("2020-01-01"))

  cdm <- cdm_local |> copyCdm()

  cdm$cohort1 <- conceptCohort(cdm, conceptSet = list(c1 = c(1L,3L), c2 = c(2L)), name = "cohort1")

  # Union concept generated cohort
  cdm$cohort2 <- unionCohorts(cdm$cohort1, name = "cohort2")
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("2009-12-22", "2010-01-01", "2010-01-11", "2010-05-31", "2012-09-27", "2014-12-06")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2010-05-04", "2011-08-24", "2014-02-09", "2014-05-20", "2014-12-10", "2015-06-24")
  ))
  expect_true(all(
    cdm$cohort2 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 2, 3, 3, 4)
  ))
  codes <- attr(cdm$cohort2, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("codelist_type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  # union concept + non concept cohorts
  cdm <- omopgenerics::bind(cdm$cohort, cdm$cohort1, name = "cohort3")
  cdm$cohort4 <- unionCohorts(cdm$cohort3, name = "cohort4")
  expect_true(all(
    cdm$cohort4 |> dplyr::pull("cohort_start_date") |> sort() ==
      c("1999-05-03", "2003-05-17", "2004-03-11", "2009-12-22", "2010-01-01", "2010-01-11", "2010-05-31", "2012-09-27", "2014-12-06", "2015-02-25")
  ))
  expect_true(all(
    cdm$cohort4 |> dplyr::pull("cohort_end_date") |> sort() ==
      c("2001-06-15", "2004-03-10", "2005-07-19", "2010-05-04", "2011-08-24", "2014-02-09", "2014-05-20", "2014-12-10", "2015-04-30", "2015-06-24")
  ))
  expect_true(all(
    cdm$cohort4 |> dplyr::pull("subject_id") |> sort() == c(1, 1, 1, 1, 2, 2, 3, 3, 3, 4)
  ))
  codes <- attr(cdm$cohort4, "cohort_codelist")
  expect_true(all(codes |> dplyr::pull("codelist_name") |> sort() == c(rep("c1", 2), "c2")))
  expect_true(all(codes |> dplyr::pull("concept_id") |> sort() == c(1, 2, 3)))
  expect_true(all(codes |> dplyr::pull("codelist_type") |> sort() == rep("index event", 3)))
  expect_true(all(codes |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 1)))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("keep original cohorts", {
  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4, seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 4,seed = 1)

  cdm <- cdm_local |> copyCdm()

  start_settings <- settings(cdm$cohort1)

  cdm$cohort2 <- unionCohorts(cdm$cohort1,
                              name = "cohort2",
                              keepOriginalCohorts = FALSE)
  expect_true(nrow(settings(cdm$cohort2)) == 1)

  cdm$cohort3 <- unionCohorts(cdm$cohort1,
                              name = "cohort3",
                              keepOriginalCohorts = TRUE)
  expect_true(nrow(settings(cdm$cohort3)) == nrow(start_settings) + 1)

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("multiple observation periods", {
  skip_on_cran()

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4),
    gender_concept_id = c(8532, 8507, 8507, 8507),
    year_of_birth = c(1997, 1963, 1986, 1978),
    month_of_birth = c(8, 1, 3, 11),
    day_of_birth = c(22, 27, 10, 8),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )


  cdm_local <- omock::mockCdmReference()

  cdm_local <- omopgenerics::insertTable(cdm = cdm_local, name = "person", table = person)

  cdm_local$observation_period <- dplyr::tibble(
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
  )
  cdm_local$cohort <- dplyr::tibble(
    "cohort_definition_id" = as.integer(c(1, 1, 1, 1, 2, 2)),
    "subject_id" = as.integer(c(1, 1, 1, 2, 2, 1)),
    "cohort_start_date" = as.Date(c(
      "2000-01-01", "2000-12-01", "2001-01-01", "2001-01-01", "2002-01-01", "2003-01-01"
    )),
    "cohort_end_date" =as.Date(c(
      "2000-05-20", "2000-12-20", "2001-04-01", "2001-12-30", "2003-01-01", "2004-01-01"
    ))
  )
  cdm <- cdm_local |> copyCdm()
  cdm$cohort <- cdm$cohort |> omopgenerics::newCohortTable() |> unionCohorts(gap = 99999)
  expect_identical(collectCohort(cdm$cohort, 1), dplyr::tibble(
      "subject_id" = as.integer(c(1, 1, 1, 2, 2)),
      "cohort_start_date" = as.Date(c("2000-01-01", "2001-01-01", "2003-01-01", "2001-01-01", "2002-01-01")),
      "cohort_end_date" = as.Date(c("2000-12-20", "2001-04-01", "2004-01-01", "2001-12-30", "2003-01-01"))
    ))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("test indexes - postgres", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")
  skip_if(!testIndexes)

  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                       host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                       password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    writeSchema = Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
    writePrefix = "cc_",
    achillesSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  )

  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "my_cohort",
                                   table = data.frame(cohort_definition_id = 1:2L,
                                                      subject_id = 1L,
                                                      cohort_start_date = as.Date("2009-01-02"),
                                                      cohort_end_date = as.Date("2009-01-03"),
                                                      sex = "Female"))
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- unionCohorts(cdm$my_cohort)
  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
