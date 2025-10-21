test_that("basic example", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4, 5,6) |> as.integer(),
    person_id = c(1, 2, 3, 4, 5,6)|> as.integer(),
    observation_period_start_date = c(
      rep(as.Date("1980-07-20"),6)
    ),
    observation_period_end_date = c(
      rep(as.Date("2023-05-20"),6)
    ),
    period_type_concept_id = c(rep(0,6)) |> as.integer()
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2,3)|> as.integer(),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5) |> as.integer(),
    year_of_birth = c(rep(1990, 5)) |> as.integer(),
    month_of_birth = c(rep(02, 5)) |> as.integer(),
    day_of_birth = c(rep(11, 5)) |> as.integer(),
    gender_concept_id = c(rep(0,5)) |> as.integer(),
    ethnicity_concept_id = c(rep(0,5)) |> as.integer(),
    race_concept_id = c(rep(0,5)) |> as.integer()
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock"
  ) |>
    copyCdm()

  cdm$my_death_cohort <- deathCohort(cdm=cdm,
                                     name = "my_death_cohort")

  expect_true(all(c("cohort_definition_id", "subject_id",
                    "cohort_start_date", "cohort_end_date") %in%
                    colnames(cdm$my_death_cohort)))

  expect_identical(
    omopgenerics::attrition(cdm$my_death_cohort) |>
    dplyr::pull("reason"),
  c("Initial qualifying events",
    "Record in observation",
    "Not missing record date",
    "First death record"))

  dropCreatedTables(cdm = cdm)
})

test_that("first death record per person", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4, 5,6) |> as.integer(),
    person_id = c(1, 2, 3, 4, 5,6) |> as.integer(),
    observation_period_start_date = c(
      rep(as.Date("1980-07-20"),6)
    ),
    observation_period_end_date = c(
      rep(as.Date("2023-05-20"),6)
    ),
    period_type_concept_id = c(rep(0,6)) |> as.integer()
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2,2) |> as.integer(),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-31")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5) |> as.integer(),
    year_of_birth = c(rep(1990, 5)) |> as.integer(),
    month_of_birth = c(rep(02, 5)) |> as.integer(),
    day_of_birth = c(rep(11, 5)) |> as.integer(),
    gender_concept_id = c(rep(0,5)) |> as.integer(),
    ethnicity_concept_id = c(rep(0,5)) |> as.integer(),
    race_concept_id = c(rep(0,5)) |> as.integer()
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm2 <- cdm |> copyCdm()

  cdm2$death_cohort <- deathCohort(cdm=cdm2,
                       name = "death_cohort")

  expect_true(nrow(cdm2$death_cohort |>
                     dplyr::filter(subject_id == "2") |>
                     dplyr::collect()) == 1)


  expect_true(cdm2$death_cohort |>
                dplyr::filter(subject_id == "2") |>
                dplyr::select(cohort_start_date) |>
                dplyr::pull() == as.Date("2020-01-02"))

  expect_identical(omopgenerics::attrition(cdm2$death_cohort) |>
    dplyr::filter(reason == "First death record") |>
    dplyr::pull("excluded_records"),
    1L)

  expect_identical(omopgenerics::attrition(cdm2$death_cohort) |>
                     dplyr::filter(reason == "First death record") |>
                     dplyr::pull("excluded_subjects"),
                   0L)

  dropCreatedTables(cdm = cdm)
})

test_that("test death in observation criteria", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2) |> as.integer(),
    person_id = c(1,2) |> as.integer(),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-01-01"),
      as.Date("2021-01-01")
    ),
    period_type_concept_id = c(rep(0,2)) |> as.integer()
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2) |> as.integer(),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5) |> as.integer(),
    year_of_birth = c(rep(1990, 5)) |> as.integer(),
    month_of_birth = c(rep(02, 5)) |> as.integer(),
    day_of_birth = c(rep(11, 5)) |> as.integer(),
    gender_concept_id = c(rep(0,5)) |> as.integer(),
    ethnicity_concept_id = c(rep(0,5)) |> as.integer(),
    race_concept_id = c(rep(0,5)) |> as.integer()
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm2 <- cdm |> copyCdm()

  cdm2$death_cohort <- deathCohort(cdm=cdm2,
                                 name = "death_cohort")

  expect_true(nrow(cdm2$death_cohort |> dplyr::collect()) == 1)

  expect_true(cdm2$death_cohort |>
                dplyr::select(subject_id) |>
                dplyr::pull() == 2)

  expect_true(cdm2$death_cohort |>
                dplyr::select(cohort_start_date) |>
                dplyr::pull() == as.Date("2020-01-02"))

  dropCreatedTables(cdm = cdm)
})

test_that("test different cohort table name", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3) |> as.integer(),
    person_id = c(1,2, 3) |> as.integer(),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2022-01-01")
    ),
    period_type_concept_id = c(rep(0,3)) |> as.integer()
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2,3) |> as.integer(),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5) |> as.integer(),
    year_of_birth = c(rep(1990, 5)) |> as.integer(),
    month_of_birth = c(rep(02, 5)) |> as.integer(),
    day_of_birth = c(rep(11, 5)) |> as.integer(),
    gender_concept_id = c(rep(0,5)) |> as.integer(),
    ethnicity_concept_id = c(rep(0,5)) |> as.integer(),
    race_concept_id = c(rep(0,5)) |> as.integer()
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm2 <-  cdm |> copyCdm()

  cdm2$my_cohort_death <- deathCohort(cdm=cdm2,
                                 name = "my_cohort_death")

  expect_true("my_cohort_death" %in% names(cdm2))
  expect_true(all(c("cohort_definition_id", "subject_id",
                    "cohort_start_date", "cohort_end_date") %in%
                    colnames(cdm2$my_cohort_death)))

  dropCreatedTables(cdm = cdm)
})

test_that("test subsetting death table by a cohort table", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3) |> as.integer(),
    person_id = c(1,2, 3) |> as.integer(),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2022-01-01")
    ),
    period_type_concept_id = c(rep(0,3)) |> as.integer()
  )

  deathTable <- dplyr::tibble(
    person_id = seq(1,5) |> as.integer(),
    death_date = c(as.Date("2020-01-01")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5) |> as.integer(),
    year_of_birth = c(rep(1990, 5)) |> as.integer(),
    month_of_birth = c(rep(02, 5)) |> as.integer(),
    day_of_birth = c(rep(11, 5)) |> as.integer(),
    gender_concept_id = c(rep(0,5)) |> as.integer(),
    ethnicity_concept_id = c(rep(0,5)) |> as.integer(),
    race_concept_id = c(rep(0,5)) |> as.integer()
  )

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1,1,2) |> as.integer(),
    subject_id = c(1,2,3) |> as.integer(),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cohortTables = list(
      cohort1 = cohort1
    ),
    cdmName = "mock_es"
  )

  cdm2 <-  cdm |> copyCdm()

  cdm2$death_cohort <- deathCohort(cdm=cdm2,
                                 name = "death_cohort",
                                 subsetCohort = "cohort1")

  expect_true(nrow(cdm2$death_cohort |> dplyr::collect()) == 3)

  expect_true(all(cdm2$death_cohort |>
                    dplyr::select(subject_id) |>
                    dplyr::pull() %in%  c(1,2,3)
  ))
  # with subsetCohortId
  cdm2$death_cohort <-  deathCohort(cdm=cdm2,
                       name = "death_cohort",
                       subsetCohort = "cohort1",
                       subsetCohortId = 1)

  expect_true(nrow(cdm2$death_cohort |> dplyr::collect()) == 2)

  expect_true(all(cdm2$death_cohort |>
                    dplyr::select(subject_id) |>
                    dplyr::pull() %in%  c(1,2)
  ))

  dropCreatedTables(cdm = cdm)
})

test_that("test expected errors", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3) |> as.integer(),
    person_id = c(1,2, 3) |> as.integer(),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2022-01-01")
    ),
    period_type_concept_id = c(rep(0,3)) |> as.integer()
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5) |> as.integer(),
    year_of_birth = c(rep(1990, 5)) |> as.integer(),
    month_of_birth = c(rep(02, 5)) |> as.integer(),
    day_of_birth = c(rep(11, 5)) |> as.integer(),
    gender_concept_id = c(rep(0,5)) |> as.integer(),
    ethnicity_concept_id = c(rep(0,5)) |> as.integer(),
    race_concept_id = c(rep(0,5)) |> as.integer()
  )

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1,1,1) |> as.integer(),
    subject_id = c(1,2,3) |> as.integer(),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort1
    ),
    cdmName = "mock_es"
  )

  cdm = copyCdm(cdm)

  # no death table in CDM
  expect_error(deathCohort(cdm=cdm,
                           name = "death_cohort"))

  # cohortTable & cohortId
  deathTable <- dplyr::tibble(
    person_id = c(1,2,3) |> as.integer(),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))


  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cohortTables = list(
      cohort1 = cohort1
    ),
    cdmName = "mock_es"
  )

  cdm = copyCdm(cdm)

  # cohortTable not exist
  expect_error( deathCohort(cdm=cdm,
                            name = "death_cohort",
                            subsetCohort = "non_exist_cohort"))

  # wrong cohortId input
  expect_error(deathCohort(cdm=cdm,
                           name = "death_cohort",
                           subsetCohort = "cohort1",
                           subsetCohortId = "1"))

  dropCreatedTables(cdm = cdm)
})

test_that("test single permanent table created", {
  skip_on_cran()

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3) |> as.integer(),
    person_id = c(1,2, 3) |> as.integer(),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2021-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2022-01-01")
    ),
    period_type_concept_id = c(rep(0,3)) |> as.integer()
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2,3) |> as.integer(),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2021-01-01")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5) |> as.integer(),
    year_of_birth = c(rep(1990, 5)) |> as.integer(),
    month_of_birth = c(rep(02, 5)) |> as.integer(),
    day_of_birth = c(rep(11, 5)) |> as.integer(),
    gender_concept_id = c(rep(0,5)) |> as.integer(),
    ethnicity_concept_id = c(rep(0,5)) |> as.integer(),
    race_concept_id = c(rep(0,5)) |> as.integer()
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm = copyCdm(cdm)

  start_tables <- names(cdm)

  cdm$my_death_cohort <- deathCohort(cdm=cdm, name = "my_death_cohort")

  end_tables <- names(cdm)

  expect_equal(
    sort(end_tables),
    sort(c(start_tables, "my_death_cohort")))

  dropCreatedTables(cdm = cdm)
})
