test_that("subsetCohort works", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(
      1, 1, 1, 1,
      2, 2, 2, 2,
      3, 3, 3, 3,
      4, 4, 4, 4,
      5, 5, 5, 5
    ),
    subject_id = c(
      1, 2, 2, 3,
      1, 1, 2, 3,
      2, 2, 3, 3,
      1, 2, 2, 3,
      1, 1, 1, 1
    ),
    cohort_start_date = as.Date(c(
      "2003-05-08", "2000-01-11", "2000-05-28", "2015-01-25",
      "2000-06-17", "2004-12-12", "1999-07-11", "2015-02-02",
      "2000-03-29", "2000-07-12", "2015-02-04", "2015-03-17",
      "2004-11-07", "1999-08-12", "2000-03-25", "2015-02-17",
      "2001-11-22", "2002-10-22", "2003-11-14", "2004-06-09"
    )),
    cohort_end_date = as.Date(c(
      "2005-04-08", "2000-05-27", "2001-09-08", "2015-04-26",
      "2004-12-11", "2007-09-06", "2002-03-26", "2015-08-12",
      "2000-07-11", "2001-06-22", "2015-03-16", "2015-06-11",
      "2005-10-03", "2000-03-24", "2001-04-29", "2015-03-08",
      "2002-10-21", "2003-11-13", "2004-06-08", "2006-06-02"
    ))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort1" = cohort_1)) |>
    copyCdm()

  # Subset 1 cohort
  cdm$cohort2 <- subsetCohorts(cdm$cohort1, cohortId = 1, name = "cohort2")
  expect_true(unique(cdm$cohort2 |> dplyr::pull("cohort_definition_id")) == 1)
  expect_true(settings(cdm$cohort2) |> dplyr::pull("cohort_definition_id") == 1)
  expect_true(unique(attrition(cdm$cohort2) |> dplyr::pull("cohort_definition_id")) == 1)
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(.data$cohort_definition_id == 1) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort2 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_identical(attrition(cdm$cohort1) |> dplyr::filter(.data$cohort_definition_id == 1), attrition(cdm$cohort2))

  # subset more than 1 cohort
  cdm$cohort3 <- subsetCohorts(cdm$cohort1, c(3,4,5), name = "cohort3")
  expect_true(all(unique(cdm$cohort3|> dplyr::pull("cohort_definition_id")) == 3:5))
  expect_true(all(settings(cdm$cohort3) |> dplyr::pull("cohort_definition_id") == 3:5))
  expect_true(all(unique(attrition(cdm$cohort3) |> dplyr::pull("cohort_definition_id")) == 3:5))
  expect_true(all(
    cdm$cohort1 |> dplyr::filter(.data$cohort_definition_id %in% 3:5) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_identical(attrition(cdm$cohort1) |> dplyr::filter(.data$cohort_definition_id %in% 3:5), attrition(cdm$cohort3))

  # same name
  cohort <- cdm$cohort1
  cdm$cohort1 <- subsetCohorts(cdm$cohort1, c(3,4,5))
  expect_true(all(unique(cdm$cohort1|> dplyr::pull("cohort_definition_id")) == 3:5))
  expect_true(all(settings(cdm$cohort1) |> dplyr::pull("cohort_definition_id") == 3:5))
  expect_true(all(unique(attrition(cdm$cohort1) |> dplyr::pull("cohort_definition_id")) == 3:5))
  expect_true(all(
    cohort |> dplyr::filter(.data$cohort_definition_id %in% 3:5) |> dplyr::pull("cohort_start_date") |> sort() ==
      cdm$cohort1 |> dplyr::pull("cohort_start_date") |> sort()
  ))
  expect_identical(attrition(cohort) |> dplyr::filter(.data$cohort_definition_id %in% 3:5), attrition(cdm$cohort1))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("codelist works", {
  skip_on_cran()

  cohort_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c("2003-05-17", "2004-03-11", "1999-05-03", "2015-02-25")),
    cohort_end_date = as.Date(c("2004-03-10", "2005-07-19", "2001-06-15", "2015-04-30"))
  )

  cdm <- omock::mockCdmFromTables(tables = list("cohort1" = cohort_1)) |>
    omock::mockVocabularyTables(concept = dplyr::tibble(
      "concept_id" = c(1, 2, 3),
      "concept_name" = c("my concept 1", "my concept 2", "my concept 3"),
      "domain_id" = "Drug",
      "vocabulary_id" = NA,
      "concept_class_id" = NA,
      "concept_code" = NA,
      "valid_start_date" = NA,
      "valid_end_date" = NA
    )) |>
    omopgenerics::insertTable(name = "drug_exposure", table = dplyr::tibble(
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
      ))
  cdm$observation_period <- cdm$observation_period|>
    dplyr::mutate(observation_period_start_date = as.Date("1990-01-01"), observation_period_end_date = as.Date("2020-01-01"))
  cdm <- copyCdm(cdm = cdm)

  cdm$cohort1 <- conceptCohort(cdm, conceptSet = list(c1 = c(1L,3L), c2 = c(2L)), name = "cohort1")

  # Subset 1 cohort
  cdm$cohort2 <- subsetCohorts(cdm$cohort1, 1, name = "cohort2")
  expect_identical(attr(cdm$cohort2, "cohort_codelist") |> dplyr::collect(), attr(cdm$cohort1, "cohort_codelist") |> dplyr::filter(cohort_definition_id == 1) |> dplyr::collect())

  # same name
  cohort <- cdm$cohort1
  cdm$cohort1 <- subsetCohorts(cdm$cohort1, 2)
  expect_identical(attr(cdm$cohort1, "cohort_codelist") |> dplyr::collect(), attr(cohort, "cohort_codelist") |> dplyr::filter(cohort_definition_id == 2) |> dplyr::collect())

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("Expected behaviour", {
  skip_on_cran()

  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(n = 4,seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockCohort(name = c("cohort1"), numberCohorts = 5, seed = 2) |>
    copyCdm()

  # Subset 1 cohort
  expect_error(cdm$cohort2 <- subsetCohorts("cohort1", 1, name = "cohort2"))
  expect_warning(cdm$cohort2 <- subsetCohorts(cdm$cohort1, "1", name = "cohort2"))
  expect_error(cdm$cohort2 <- subsetCohorts(cdm$cohort1, 2, name = "cohort3"))
  expect_warning(cdm$cohort2 <- subsetCohorts(cdm$cohort1, 10, name = "cohort2"))
  expect_no_error(cdm$cohort3 <- subsetCohorts(cdm$cohort1, NULL, name = "cohort3"))
  expect_identical(cdm$cohort3 |>
                     dplyr::collect(),
                   cdm$cohort1 |>
                     dplyr::collect())

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

    cdm$my_cohort <- subsetCohorts(cdm$my_cohort, cohortId = 1)
    expect_true(
      DBI::dbGetQuery(con, paste0("SELECT * FROM pg_indexes WHERE tablename = 'cc_my_cohort';")) |> dplyr::pull("indexdef") ==
        "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )

    expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

    dropCreatedTables(cdm = cdm)
  }

})
