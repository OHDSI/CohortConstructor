test_that("simple duckdb checks", {
  # create test mock ----
  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      "person" = dplyr::tibble(
        "person_id" = c(1, 2, 3),
        "gender_concept_id" = c(8507, 8532, 8532),
        "year_of_birth" = c(1993, 2000, 2005),
        "month_of_birth" = c(4, 1, 8),
        "day_of_birth" = c(19, 15, 20),
        "race_concept_id" = 0,
        "ethnicity_concept_id" = 0
      ),
      "observation_period" = dplyr::tibble(
        "observation_period_id" = 1:4,
        "person_id" = c(1, 2, 2, 3),
        "observation_period_start_date" = as.Date(c(
          "1993-04-19", "2010-03-12", "2031-08-23", "2020-10-06"
        )),
        "observation_period_end_date" = as.Date(c(
          "2033-10-11", "2017-01-01", "2045-03-12", "2100-12-31"
        )),
        "period_type_concept_id" = 0
      )
    ),
    cdmName = "test cohortconstructor",
    cohortTables = list(
      "cohort1" = dplyr::tibble(
        "cohort_definition_id" = c(1, 1, 1, 2),
        "subject_id" = c(1, 2, 3, 1),
        "cohort_start_date" = as.Date(c(
          "2032-01-19", "2039-11-12", "2036-03-16", "2003-12-15"
        )),
        "cohort_end_date" = as.Date(c(
          "2033-10-11", "2045-01-12", "2074-05-18", "2010-05-25"
        ))
      )
    )
  )
  cdm <- CDMConnector::copyCdmTo(
    con = duckdb::dbConnect(duckdb::duckdb()), cdm = cdm, schema = "main"
  )

  # test with cohort1 ----
  expect_no_error(
    cdm$cohort2 <- cdm$cohort1 |>
      trimDemographics(
        ageRange = list(
          c(0, Inf), c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)
        ),
        sex = c("Female", "Male", "Both"),
        minPriorObservation = c(0, 365),
        minFutureObservation = c(0, 365),
        name = "cohort2"
      )
  )

  expect_true(nrow(settings(cdm$cohort2)) == 6*3*2*2*2)

  id <- settings(cdm$cohort2) |>
    dplyr::filter(
      sex == "Both" & age_range == "40_59" &
        min_prior_observation == 0 &
        min_future_observation == 0 &
        grepl("cohort_1", cohort_name)
    ) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$cohort2, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date(c(
        "2033-04-19", "2040-01-15", "2045-08-20"
      )),
      "cohort_end_date" = as.Date(c(
        "2033-10-11", "2045-01-12", "2065-08-19"
      ))
    )
  )
  id <- settings(cdm$cohort2) |>
    dplyr::filter(
      sex == "Both" & age_range == "40_59" &
        min_prior_observation == 0 &
        min_future_observation == 365 &
        grepl("cohort_1", cohort_name)
    ) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$cohort2, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = c(2, 3),
      "cohort_start_date" = as.Date(c("2040-01-15", "2045-08-20")),
      "cohort_end_date" = as.Date(c("2045-01-12", "2065-08-19"))
    )
  )

  # test with observation period cohort ----
  cdm$obs <- demographicsCohort(cdm = cdm, name = "obs")

  # observation period -----
  expect_identical(
    cdm$observation_period |>
      dplyr::inner_join(
        cdm$obs |>
          dplyr::select(
            "person_id" = "subject_id",
            "observation_period_start_date" = "cohort_start_date",
            "observation_period_end_date" = "cohort_end_date"
          ),
        by = c("person_id", "observation_period_start_date", "observation_period_end_date")
      ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$observation_period_id),
    cdm$observation_period |> dplyr::collect() |>
      dplyr::arrange(.data$observation_period_id)
  )
  expect_true(
    cdm$observation_period |>
      dplyr::anti_join(
        cdm$obs |>
          dplyr::select(
            "person_id" = "subject_id",
            "observation_period_start_date" = "cohort_start_date",
            "observation_period_end_date" = "cohort_end_date"
          ),
        by = c("person_id", "observation_period_start_date", "observation_period_end_date")
      ) |>
      dplyr::tally() |>
      dplyr::pull() == 0
  )

  # with everything it works ----
  expect_no_error(
    cdm$obs1 <- cdm$obs |>
      trimDemographics(
        ageRange = list(
          c(0, Inf), c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)
        ),
        sex = c("Female", "Male", "Both"),
        minPriorObservation = c(0, 365),
        minFutureObservation = c(0, 365),
        name = "obs1"
      )
  )

  # check few examples ----
  id <- settings(cdm$obs1) |>
    dplyr::filter(
      sex == "Both" & age_range == "0_19" &
        min_prior_observation == 0 &
        min_future_observation == 0
    ) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$obs1, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date(c("1993-04-19", "2010-03-12", "2020-10-06")),
      "cohort_end_date" = as.Date(c("2013-04-18", "2017-01-01", "2025-08-19"))
    )
  )
  id <- settings(cdm$obs1) |>
    dplyr::filter(
      sex == "Both" & age_range == "0_19" &
        min_prior_observation == 365 &
        min_future_observation == 0
    ) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$obs1, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date(c("1994-04-19", "2011-03-12", "2021-10-06")),
      "cohort_end_date" = as.Date(c("2013-04-18", "2017-01-01", "2025-08-19"))
    )
  )


  # check sex is consistent ----
  expect_no_error(
    cdm$obs2 <- cdm$obs |>
      trimDemographics(
        sex = c("Female", "Male", "Both"),
        name = "obs2"
      )
  )
  expect_true(settings(cdm$obs2) |> nrow() == 3)
  values <- c("Male", "Female")
  for (val in values) {
    id1 <- settings(cdm$obs1) |>
      dplyr::filter(
        sex == val & age_range == "0_Inf" & min_prior_observation == 0 &
          min_future_observation == 0
      ) |>
      dplyr::pull(cohort_definition_id)
    id2 <- settings(cdm$obs2) |>
      dplyr::filter(sex == val) |>
      dplyr::pull(cohort_definition_id)
    expect_true(compareCohort(cdm$obs1, id1, cdm$obs2, id2))
  }

  # check age is consistent ----
  expect_no_error(
    cdm$obs3 <- cdm$obs |>
      trimDemographics(
        ageRange = list(
          c(0, Inf), c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)
        ),
        name = "obs3"
      )
  )
  expect_true(cdm$obs3 |> settings() |> nrow() == 6)
  val = c("0_Inf", "0_19", "20_39", "40_59", "60_79", "80_Inf")
  for (k in seq_along(val)) {
    id1 <- settings(cdm$obs1) |>
      dplyr::filter(
        sex == "Both" & age_range == val[k] & min_prior_observation == 0 &
          min_future_observation == 0
      ) |>
      dplyr::pull(cohort_definition_id)
    id2 <- settings(cdm$obs3) |>
      dplyr::filter(
        age_range == val[k]
      ) |>
      dplyr::pull(cohort_definition_id)
    expect_true(compareCohort(cdm$obs1, id1, cdm$obs3, id2))
  }

  # check prior observation is consistent ----
  expect_no_error(
    cdm$obs4 <- cdm$obs |>
      trimDemographics(
        minPriorObservation = c(0, 365),
        name = "obs4"
      )
  )
  expect_true(cdm$obs4 |> settings() |> nrow() == 2)
  values <- c(0, 365)
  for (val in values) {
    id1 <- settings(cdm$obs1) |>
      dplyr::filter(
        sex == "Both" & age_range == "0_Inf" & min_prior_observation == val &
          min_future_observation == 0
      ) |>
      dplyr::pull(cohort_definition_id)
    id2 <- settings(cdm$obs4) |>
      dplyr::filter(min_prior_observation == val) |>
      dplyr::pull(cohort_definition_id)
    expect_true(compareCohort(cdm$obs1, id1, cdm$obs4, id2))
  }

  # check future observation is consistent ----
  expect_no_error(
    cdm$obs5 <- cdm$obs |>
      trimDemographics(
        minFutureObservation = c(0, 365),
        name = "obs5"
      )
  )
  expect_true(cdm$obs5 |> settings() |> nrow() == 2)
  values <- c(0, 365)
  for (val in values) {
    id1 <- settings(cdm$obs1) |>
      dplyr::filter(
        sex == "Both" & age_range == "0_Inf" & min_prior_observation == 0 &
          min_future_observation == val
      ) |>
      dplyr::pull(cohort_definition_id)
    id2 <- settings(cdm$obs5) |>
      dplyr::filter(min_future_observation == val) |>
      dplyr::pull(cohort_definition_id)
    expect_true(compareCohort(cdm$obs1, id1, cdm$obs5, id2))
  }

  # long prior observation ----
  expect_no_error(
    cdm$obs_new <- cdm$obs |>
      trimDemographics(
        ageRange = list(c(0, 19)),
        minPriorObservation = c(0, 1825, 3000),
        name = "obs_new"
      )
  )
  id <- settings(cdm$obs_new) |>
    dplyr::filter(min_prior_observation == 0) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$obs_new, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = c(1, 2, 3),
      "cohort_start_date" = as.Date(c("1993-04-19", "2010-03-12", "2020-10-06")),
      "cohort_end_date" = as.Date(c("2013-04-18", "2017-01-01", "2025-08-19"))
    )
  )
  id <- settings(cdm$obs_new) |>
    dplyr::filter(min_prior_observation == 1825) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$obs_new, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = c(1, 2),
      "cohort_start_date" = as.Date(c("1998-04-18", "2015-03-11")),
      "cohort_end_date" = as.Date(c("2013-04-18", "2017-01-01"))
    )
  )
  id <- settings(cdm$obs_new) |>
    dplyr::filter(min_prior_observation == 3000) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$obs_new, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = 1,
      "cohort_start_date" = as.Date(c("2001-07-06")),
      "cohort_end_date" = as.Date(c("2013-04-18"))
    )
  )
})

test_that("cohort Id, name, additional columns", {
  cdm <- mockCohortConstructor(nPerson = 5)
  cdm$cohort2 <- cdm$cohort2 |>
    dplyr::mutate(
      col_extra1 = as.numeric(subject_id) + 1,
      col_extra2 = as.numeric(subject_id) + 2
    ) |>
    dplyr::compute(name = "cohort2", temporary = FALSE)

  cdm$cohort3 <- trimDemographics(cohort = cdm$cohort2,
                                  cohortId = 1,
                                  ageRange = NULL,
                                  sex = "Male",
                                  minPriorObservation = c(0, 400),
                                  minFutureObservation = NULL,
                                  name = "cohort3")
  expect_true(all(colnames(cdm$cohort3) == c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "col_extra1", "col_extra2"
  )))
  expect_true(compareCohort(cdm$cohort2, 2, cdm$cohort3, 2))
  x1 <- collectCohort(cdm$cohort3, 1)
  x3 <- collectCohort(cdm$cohort3, 3)
  expect_equal(
    x1,
    dplyr::tibble(
      subject_id = c(1, 1, 2, 3, 5),
      cohort_start_date = as.Date(c("2001-04-03", "2002-05-07", "1999-07-26", "2015-02-19", "2012-06-12")),
      cohort_end_date = as.Date(c("2002-05-06", "2005-11-07", "2002-09-17", "2015-06-27", "2012-09-09"))
    )
  )
  expect_equal(
    x3,
    dplyr::tibble(
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2001-07-08", "2002-05-07", "2000-05-09")),
      cohort_end_date = as.Date(c("2002-05-06", "2005-11-07", "2002-09-17"))
    )
  )
  expect_true(all(
    attrition(cdm$cohort3)$reason ==
      c('Initial qualifying events', 'Sex requirement: Male',
        'Prior observation requirement: 0 days', 'Initial qualifying events',
        'Initial qualifying events', 'Sex requirement: Male',
        'Prior observation requirement: 400 days')
  ))
  expect_equal(
    settings(cdm$cohort3),
    dplyr::tibble(
      cohort_definition_id = 1:3,
      cohort_name = c("cohort_1_1", "cohort_2", "cohort_1_2"),
      sex = c("Male", "Both", "Male"),
      min_prior_observation = c(0, 0, 400)
    )
  )

  expect_no_error(
    cohort <- trimDemographics(cohort = cdm$cohort2,
                               cohortId = 1,
                               ageRange = NULL,
                               sex = "Male",
                               minPriorObservation = c(0, 400),
                               minFutureObservation = NULL)
  )

  # test 29th frebruary
  cdm <- mockCohortConstructor(
    tables = list(
      "cohort" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = 1,
        cohort_start_date = as.Date("1972-02-29"),
        cohort_end_date = as.Date("2018-03-20")
      )
    )
  )
  cdm <- omopgenerics::insertTable(
    cdm, "observation_period",
    dplyr::tibble(
      person_id = 1,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2050-01-01"),
      observation_period_id = 1,
      period_type_concept_id = 0
    )
  )
  cdm$person <- cdm$person |> dplyr::mutate(
    birth_datetime = as.Date("1972-02-29"),
    year_of_birth = 1972,
    month_of_birth = 2,
    day_of_birth = 29
  )
  cohort <- trimDemographics(cdm$cohort, ageRange = c(18, 65))
  expect_true(cohort |> dplyr::pull(cohort_start_date) == "1990-03-01")
  CDMConnector::cdm_disconnect(cdm)
})
