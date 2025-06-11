test_that("simple duckdb checks", {
  testthat::skip_on_cran()
  # create test mock ----
  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      "person" = dplyr::tibble(
        "person_id" = as.integer(c(1, 2, 3)),
        "gender_concept_id" = as.integer(c(8507, 8532, 8532)),
        "year_of_birth" = as.integer(c(1993, 2000, 2005)),
        "month_of_birth" = as.integer(c(4, 1, 8)),
        "day_of_birth" = as.integer(c(19, 15, 20)),
        "race_concept_id" = 0L,
        "ethnicity_concept_id" = 0L
      ),
      "observation_period" = dplyr::tibble(
        "observation_period_id" = as.integer(1:4),
        "person_id" = as.integer(c(1, 2, 2, 3)),
        "observation_period_start_date" = as.Date(c(
          "1993-04-19", "2010-03-12", "2017-08-23", "2020-10-06"
        )),
        "observation_period_end_date" = as.Date(c(
          "2023-10-11", "2017-01-01", "2023-03-12", "2024-01-01"
        )),
        "period_type_concept_id" = 0L
      )
    ),
    cdmName = "test cohortconstructor",
    cohortTables = list(
      "cohort1" = dplyr::tibble(
        "cohort_definition_id" = as.integer(c(1, 1, 1, 2)),
        "subject_id" = as.integer(c(1, 2, 3, 1)),
        "cohort_start_date" = as.Date(c(
          "2012-01-19", "2010-11-12", "2021-03-16", "2003-12-15"
        )),
        "cohort_end_date" = as.Date(c(
          "2023-10-11", "2015-01-12", "2024-01-01", "2010-05-25"
        ))
      )
    )
  )
  cdm <- cdm |> copyCdm()

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
      "subject_id" = as.integer(c()),
      "cohort_start_date" = as.Date(c()),
      "cohort_end_date" = as.Date(c())
    )
  )
  id <- settings(cdm$cohort2) |>
    dplyr::filter(
      sex == "Both" & age_range == "0_19" &
        min_prior_observation == 0 &
        min_future_observation == 365 &
        grepl("cohort_1", cohort_name)
    ) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$cohort2, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date(c("2012-01-19", "2010-11-12", "2021-03-16")),
      "cohort_end_date" = as.Date(c("2013-04-18", "2015-01-12", "2024-01-01"))
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
      "subject_id" = as.integer(c(1, 2, 2, 3)),
      "cohort_start_date" = as.Date(c("1993-04-19", "2010-03-12", "2017-08-23", "2020-10-06")),
      "cohort_end_date" = as.Date(c("2013-04-18", "2017-01-01", "2020-01-14", "2024-01-01"))
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
      "subject_id" = c(1L, 2L, 2L, 3L),
      "cohort_start_date" = as.Date(c("1994-04-19", "2011-03-12", "2018-08-23", "2021-10-06")),
      "cohort_end_date" = as.Date(c("2013-04-18", "2017-01-01", "2020-01-14", "2024-01-01"))
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
      dplyr::pull("cohort_definition_id")
    id2 <- settings(cdm$obs2) |>
      dplyr::filter(sex == val) |>
      dplyr::pull("cohort_definition_id")
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
  val <- c("0_Inf", "0_19", "20_39", "40_59", "60_79", "80_Inf")
  for (k in seq_along(val)) {
    id1 <- settings(cdm$obs1) |>
      dplyr::filter(
        sex == "Both" & age_range == val[k] & min_prior_observation == 0 &
          min_future_observation == 0
      ) |>
      dplyr::pull("cohort_definition_id")
    id2 <- settings(cdm$obs3) |>
      dplyr::filter(
        age_range == val[k]
      ) |>
      dplyr::pull("cohort_definition_id")
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
      dplyr::pull("cohort_definition_id")
    id2 <- settings(cdm$obs4) |>
      dplyr::filter(min_prior_observation == val) |>
      dplyr::pull("cohort_definition_id")
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
      dplyr::pull("cohort_definition_id")
    id2 <- settings(cdm$obs5) |>
      dplyr::filter(min_future_observation == val) |>
      dplyr::pull("cohort_definition_id")
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
      "subject_id" = c(1L, 2L, 2L, 3L),
      "cohort_start_date" = as.Date(c("1993-04-19", "2010-03-12", "2017-08-23", "2020-10-06")),
      "cohort_end_date" = as.Date(c("2013-04-18", "2017-01-01", "2020-01-14", "2024-01-01"))
    )
  )
  id <- settings(cdm$obs_new) |>
    dplyr::filter(min_prior_observation == 1825) |>
    dplyr::pull("cohort_definition_id")
  x <- collectCohort(cdm$obs_new, id)
  expect_identical(
    x,
    dplyr::tibble(
      "subject_id" = c(1L, 2L),
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
      "subject_id" = 1L,
      "cohort_start_date" = as.Date(c("2001-07-06")),
      "cohort_end_date" = as.Date(c("2013-04-18"))
    )
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  PatientProfiles::mockDisconnect(cdm)
})

test_that("cohort Id, name, additional columns", {
  testthat::skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = c(1L, 1L, 2L, 3L, 4L),
    cohort_start_date = as.Date(c(
      "2001-04-03", "2002-05-07", "1999-07-26", "2015-02-19", "1990-09-07"
    )),
    cohort_end_date = as.Date(c(
      "2002-05-06", "2005-11-07", "2002-09-17", "2015-06-27", "2008-02-19"
    ))
  )

  cohort_2 <- dplyr::tibble(
    cohort_definition_id = c(rep(1L, 5), rep(2L, 5)),
    subject_id = c(1L, 1L, 2L, 3L, 4L, 1L, 1L, 2L, 3L, 4L),
    cohort_start_date = as.Date(c(
      # Cohort 1
      "2001-04-03", "2002-05-07", "1999-07-26", "2015-02-19", "1990-09-07",
      # Cohort 2
      "2004-04-07", "2004-05-03", "2000-02-27", "2015-03-10", "1995-05-15"
    )),
    cohort_end_date = as.Date(c(
      # Cohort 1
      "2002-05-06", "2005-11-07", "2002-09-17", "2015-06-27", "2008-02-19",
      # Cohort 2
      "2004-05-02", "2005-05-25", "2001-05-18", "2015-06-15", "1995-11-14"
    ))
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:5,
    person_id = 1:5,
    observation_period_start_date = as.Date(c(
      "2000-06-03", "1999-04-05", "2015-01-15", "1989-12-09", "2012-03-18"
    )),
    observation_period_end_date = as.Date(c(
      "2013-06-29", "2003-06-15", "2015-10-11", "2013-12-31", "2013-02-10"
    )),
    period_type_concept_id = NA_integer_
  )

  person <- dplyr::tibble(
    person_id = 1:5,
    gender_concept_id = c(8507L, 8507L, 8507L, 8532L, 8507L),
    year_of_birth = c(1997L, 1963L, 1986L, 1978L, 1973L),
    month_of_birth = c(8L, 1L, 3L, 11L, 3L),
    day_of_birth = c(22L, 27L, 10L, 8L, 2L),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_
  )

  cdm_local <- omock::mockCdmFromTables(
    tables = list(
      "cohort1" = cohort_1,
      "cohort2" = cohort_2
    ),
    seed = 1
  )

  cdm_local <- omopgenerics::insertTable(cdm = cdm_local, name = "observation_period", table = obs)
  cdm_local <- omopgenerics::insertTable(cdm = cdm_local, name = "person", table = person)
  cdm <- cdm_local |> copyCdm()

  cdm$cohort2 <- cdm$cohort2 |>
    dplyr::mutate(
      col_extra1 = as.numeric(subject_id) + 1,
      col_extra2 = as.numeric(subject_id) + 2
    ) |>
    dplyr::compute(name = "cohort2", temporary = FALSE)

  cdm$cohort3 <- trimDemographics(cohort = cdm$cohort2,
                                  cohortId = "cohort_1",
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
  expect_identical(x1, dplyr::tibble(
    subject_id = c(1, 1, 2, 3) |> as.integer(),
    cohort_start_date = as.Date(c("2001-04-03", "2002-05-07", "1999-07-26", "2015-02-19")),
    cohort_end_date = as.Date(c("2002-05-06", "2005-11-07", "2002-09-17", "2015-06-27"))
  ))
  expect_identical(x3, dplyr::tibble(
    subject_id = c(1, 1, 2) |> as.integer(),
    cohort_start_date = as.Date(c("2001-07-08", "2002-05-07", "2000-05-09")),
    cohort_end_date = as.Date(c("2002-05-06", "2005-11-07", "2002-09-17"))
  ))
  expect_true(all(
    attrition(cdm$cohort3)$reason ==
      c('Initial qualifying events', 'Sex requirement: Male',
        'Prior observation requirement: 0 days', 'Initial qualifying events',
        'Initial qualifying events', 'Sex requirement: Male',
        'Prior observation requirement: 400 days')
  ))
  expect_identical(settings(cdm$cohort3), dplyr::tibble(
    cohort_definition_id = as.integer(1:3),
    cohort_name = c("cohort_1_1", "cohort_2", "cohort_1_2"),
    sex = c("Male", "Both", "Male"),
    min_prior_observation = c(0, 0, 400)
  ))

  expect_no_error(
    cohort <- trimDemographics(cohort = cdm$cohort2,
                               cohortId = 1,
                               ageRange = NULL,
                               sex = "Male",
                               minPriorObservation = c(0, 400),
                               minFutureObservation = NULL)
  )

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

  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "my_cohort",
    table = data.frame(cohort_definition_id = 1L,
                       subject_id = 1L,
                       cohort_start_date = as.Date("2009-01-01"),
                       cohort_end_date = as.Date("2009-01-02"))
  )
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort)
  cdm$my_cohort <- trimDemographics(cdm$my_cohort, ageRange = list(c(0, 50)))

  expect_true(
    DBI::dbGetQuery(db, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with("my_cohort"))
  CDMConnector::cdmDisconnect(cdm = cdm)
})
