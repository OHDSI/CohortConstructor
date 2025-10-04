test_that("simple example", {
  skip_on_cran()
  cdm <- omock::mockCdmFromTables(tables = list(
    observation_period = dplyr::tibble(
      "observation_period_id" = c(1L, 2L, 3L),
      "person_id" = c(1L, 2L, 3L),
      "observation_period_start_date" = as.Date("2000-01-01"),
      "observation_period_end_date" = as.Date("2024-01-01"),
      "period_type_concept_id" = 1L
    ),
    cohort = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 1L, 2L, 3L),
      "cohort_start_date" = as.Date(c(
        "2020-01-01", "2020-01-12", "2021-01-01", "2022-01-01"
      )),
      "cohort_end_date" = as.Date(c(
        "2020-01-10", "2020-01-15", "2021-01-01", "2022-01-01"
      ))
    )
  )) |>
    copyCdm()

  expect_no_error(sameCohort <- cdm$cohort |>
                    collapseCohorts(gap = 0,
                                    name = "new_cohort"))
  expect_identical(settings(sameCohort), settings(cdm$cohort))
  expect_identical(cohortCount(sameCohort), cohortCount(cdm$cohort))

  # test character id works
  cohort_name <- settings(cdm$cohort) |> dplyr::pull("cohort_name")
  expect_no_error(sameCohort2 <- cdm$cohort |>
                    collapseCohorts(gap = 0,
                                    name = "new_cohort",
                                    cohortId = cohort_name))
  expect_identical(settings(sameCohort2), settings(cdm$cohort))
  expect_identical(cohortCount(sameCohort2), cohortCount(cdm$cohort))
  expect_identical(
    attrition(sameCohort),
    attrition(cdm$cohort) |>
      dplyr::union_all(dplyr::tibble(
        "cohort_definition_id" = 1L,
        "number_records" = 4L,
        "number_subjects" = 3L,
        "reason_id" = 2L,
        "reason" = "Collapse cohort with a gap of 0 days.",
        "excluded_records" = 0L,
        "excluded_subjects" = 0L
      ))
  )
  expect_true(tableName(sameCohort) == "new_cohort")
  expect_identical(
    omopgenerics::tableSource(sameCohort), omopgenerics::tableSource(cdm$cohort)
  )

  expect_no_error(newCohort <- cdm$cohort |>
                    collapseCohorts(gap = 5, name = "my_cohort"))
  expect_identical(settings(newCohort), settings(cdm$cohort))
  expect_identical(cohortCount(newCohort), dplyr::tibble(
    "cohort_definition_id" = 1L,
    "number_records" = 3L,
    "number_subjects" = 3L
  ))
  expect_identical(
    attrition(newCohort),
    attrition(cdm$cohort) |>
      dplyr::union_all(dplyr::tibble(
        "cohort_definition_id" = 1L,
        "number_records" = 3L,
        "number_subjects" = 3L,
        "reason_id" = 2L,
        "reason" = "Collapse cohort with a gap of 5 days.",
        "excluded_records" = 1L,
        "excluded_subjects" = 0L
      ))
  )
  expect_true(tableName(newCohort) == "my_cohort")
  expect_identical(
    omopgenerics::tableSource(newCohort), omopgenerics::tableSource(cdm$cohort)
  )

  # expected behaviour
  expect_warning(cdm$cohort |> collapseCohorts(cohortId = c("a", "n")))
  cdm$cohort <- cdm$cohort |> dplyr::mutate(extra_column_1 = 1,
                                            extra_column_2 = 2)
  expect_warning(cdm$cohort |> collapseCohorts())
  expect_error(cdm$cohort |> collapseCohorts(gap = NA))
  expect_error(cdm$cohort |> collapseCohorts(gap = NULL))
  expect_error(cdm$cohort |> collapseCohorts(gap = -1))
  expect_error(cdm$cohort |> collapseCohorts(gap = -Inf))
  expect_error(cdm$cohort |> collapseCohorts(gap = "not a number"))

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("infitine", {
  skip_on_cran()

  cdm <- omock::mockCdmFromTables(tables = list(
    person = dplyr::tibble(
      "person_id" = c(1L, 2L, 3L),
      "gender_concept_id" = 1L,
      "year_of_birth" = 1990L,
      "race_concept_id" = 1L,
      "ethnicity_concept_id" = 1L
    ),
    observation_period = dplyr::tibble(
      "observation_period_id" = c(1L, 2L, 3L),
      "person_id" = c(1L, 2L, 3L),
      "observation_period_start_date" = as.Date("2000-01-01"),
      "observation_period_end_date" = as.Date("2024-01-01"),
      "period_type_concept_id" = 1L
    ),
    cohort = dplyr::tibble(
      "cohort_definition_id" = c(1L, 1L, 1L, 2L),
      "subject_id" = c(1L, 2L, 3L, 3L),
      "cohort_start_date" = as.Date(c("2020-01-01", "2020-01-01",
                                      "2020-01-01", "2021-01-01")),
      "cohort_end_date" = as.Date(c("2022-01-01", "2022-01-01",
                                    "2022-01-01", "2023-01-01"))
    )
  )) |>
    copyCdm()

  # for each person and each cohort id we should go from
  # first cohort start to last cohort entry
  cdm$cohort_collapsed <- cdm$cohort |>
                  collapseCohorts(gap = Inf,
                                  name = "cohort_collapsed")
  expect_true(nrow(cdm$cohort_collapsed |>
    dplyr::collect()) == 4)
  expect_true(all(cdm$cohort_collapsed |>
                     dplyr::filter(cohort_definition_id == 1) |>
                     dplyr::pull("cohort_start_date") ==
                as.Date("2020-01-01")))
  expect_true(all(cdm$cohort_collapsed |>
                    dplyr::filter(cohort_definition_id == 2) |>
                    dplyr::pull("cohort_start_date") ==
                    as.Date("2021-01-01")))

  expect_true(all(cdm$cohort_collapsed |>
                    dplyr::filter(cohort_definition_id == 1) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2022-01-01")))
  expect_true(all(cdm$cohort_collapsed |>
                    dplyr::filter(cohort_definition_id == 2) |>
                    dplyr::pull("cohort_end_date") ==
                    as.Date("2023-01-01")))

  # test Id
  cdm$cohort_collapsed2 <- cdm$cohort |>
    dplyr::mutate("extra_col" = 1) |>
    collapseCohorts(gap = Inf,
                    name = "cohort_collapsed2",
                    cohortId = 2)
  expect_equal(collectCohort(cdm$cohort, 1), collectCohort(cdm$cohort_collapsed2, 1))
  expect_true(
    cdm$cohort_collapsed2 |>
      attrition() |>
      dplyr::filter(reason == "Collapse cohort with a gap of Inf days.") |>
      dplyr::pull("cohort_definition_id") == 2
  )

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  dropCreatedTables(cdm = cdm)
})

test_that("multiple observation periods", {
# collapse should respect observation end dates
  skip_on_cran()

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = dplyr::tibble(
        person_id = 1L,
        gender_concept_id = 8507L,
        year_of_birth = 1950L,
        race_concept_id = 0L,
        ethnicity_concept_id = 0L
      ),
      concept = dplyr::tibble(
        "concept_id" = 1L,
        "concept_name" = "my concept",
        "domain_id" = "drug",
        "vocabulary_id" = NA_character_,
        "concept_class_id" = NA_character_,
        "concept_code" = NA_character_,
        "valid_start_date" = as.Date("1900-01-01"),
        "valid_end_date" = as.Date("2100-01-01")
      ),
      vocabulary = omock:::mockVocabulary,
      drug_exposure = dplyr::tibble(
        "drug_exposure_id" = c(1L, 2L),
        "person_id" = 1L,
        "drug_concept_id" = 1L,
        "drug_exposure_start_date" = as.Date(c("2020-01-01", "2021-01-01")),
        "drug_exposure_end_date" =  as.Date(c("2020-01-15", "2021-01-15")),
        "drug_type_concept_id" = 1L,
        verbatim_end_date = drug_exposure_end_date
      ),
      observation_period = dplyr::tibble(
        "observation_period_id" = c(1L, 2L),
        "person_id" = 1L,
        "period_type_concept_id" = 1L,
        "observation_period_start_date" = as.Date(c("2020-01-01", "2021-01-01")),
        "observation_period_end_date" =  as.Date(c("2020-06-01", "2021-06-01"))
      )
    ),
    cdmName = "mock", cohortTables = list(
      cohort = dplyr::tibble(
        "cohort_definition_id" = 1L,
        "subject_id" = 1L,
        "cohort_start_date" = as.Date("2020-01-01"),
        "cohort_end_date" = as.Date("2020-01-01")
      )
    )
  ) |>
    copyCdm()

  expect_no_error(cdm$cohort_1 <- conceptCohort(cdm = cdm,
                                          conceptSet = list(a = 1L),
                                          name = "cohort_1"))

  # should not have been combined as they are in different observation periods
  expect_no_error(cdm$cohort_1  <- cdm$cohort_1  |>
                    collapseCohorts(gap = 500, name = "cohort_1"))
  expect_true(nrow(cdm$cohort_1 |>
         dplyr::collect()) == 2)

  expect_no_error(cdm$cohort_1 <- conceptCohort(cdm = cdm,
                                                conceptSet = list(a = 1L),
                                                name = "cohort_1"))
  expect_no_error(cdm$cohort_1  <- cdm$cohort_1  |>
                    collapseCohorts(gap = Inf, name = "cohort_1"))
  expect_true(nrow(cdm$cohort_1 |>
                     dplyr::collect()) == 2)

  expect_true(sum(grepl("og", omopgenerics::listSourceTables(cdm))) == 0)

  if (dbToTest == "postgres CDMConnector") {
    con <- CDMConnector::cdmCon(cdm = cdm)

    # check indexes are still there
    indexdef <- dplyr::tbl(con, "pg_indexes") |>
      dplyr::filter(.data$table_name == "coco_test_cohort_1") |>
      dplyr::pull("indexdef")
    expect_indentical(
      indexdef,
      "CREATE INDEX cc_my_cohort_subject_id_cohort_start_date_idx ON public.cc_my_cohort USING btree (subject_id, cohort_start_date)"
    )
  }

  dropCreatedTables(cdm = cdm)
})

test_that("overlapping input cohort", {
  skip_on_cran()
  cdm <- omock::mockCdmFromTables(tables = list(
    observation_period = dplyr::tibble(
      "observation_period_id" = c(1L, 2L, 3L),
      "person_id" = c(1L, 2L, 3L),
      "observation_period_start_date" = as.Date("2000-01-01"),
      "observation_period_end_date" = as.Date("2024-01-01"),
      "period_type_concept_id" = 1L
    ))) |>
    copyCdm()

  # overlap should still work if input cohort has overlaps
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "cohort",
    table = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 1L, 2L, 3L),
      "cohort_start_date" = as.Date(c(
        "2020-01-01", "2020-01-08", "2021-01-01", "2022-01-01"
      )),
      "cohort_end_date" = as.Date(c(
        "2020-01-10", "2020-01-15", "2021-01-01", "2022-01-01"
      ))
    ))
  cdm$cohort <- omopgenerics::newCohortTable(cdm$cohort, .softValidation = TRUE)

  expect_no_error(cdm$collapsed_cohort <- cdm$cohort |>
                    collapseCohorts(gap = 0,
                                    name = "collapsed_cohort"))
  expect_true(cdm$collapsed_cohort |>
    dplyr::filter(subject_id == 1) |>
    dplyr::pull(cohort_start_date) == "2020-01-01")
  expect_true(cdm$collapsed_cohort |>
    dplyr::filter(subject_id == 1) |>
    dplyr::pull(cohort_end_date) == "2020-01-15")

  expect_no_error(cdm$collapsed_cohort <- cdm$cohort |>
                    collapseCohorts(gap = 1,
                                    name = "collapsed_cohort"))
  expect_true(cdm$collapsed_cohort |>
                dplyr::filter(subject_id == 1) |>
                dplyr::pull(cohort_start_date) == "2020-01-01")
  expect_true(cdm$collapsed_cohort |>
                dplyr::filter(subject_id == 1) |>
                dplyr::pull(cohort_end_date) == "2020-01-15")

  expect_no_error(cdm$collapsed_cohort <- cdm$cohort |>
                    collapseCohorts(gap = Inf,
                                    name = "collapsed_cohort"))
  expect_true(cdm$collapsed_cohort |>
                dplyr::filter(subject_id == 1) |>
                dplyr::pull(cohort_start_date) == "2020-01-01")
  expect_true(cdm$collapsed_cohort |>
                dplyr::filter(subject_id == 1) |>
                dplyr::pull(cohort_end_date) == "2020-01-15")

 dropCreatedTables(cdm = cdm)
})
