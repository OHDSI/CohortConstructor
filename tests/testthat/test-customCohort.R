test_that("initial tests", {
  skip_on_cran()
  cdm <- omock::mockPerson(nPerson = 3)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = dplyr::tibble(
      "observation_period_id" = c(1L, 2L, 3L),
      "person_id" = c(1L, 2L, 3L),
      "observation_period_start_date" = as.Date(c("2000-01-01")),
      "observation_period_end_date" = as.Date(c("2025-01-01")),
      "period_type_concept_id" = NA_integer_
    ))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = dplyr::tibble(
      "concept_id" = c(1L, 2L, 4L, 5L, 99L, 1177480L, 43157344L),
      "concept_name" = c("concept 1", "concept 2", "concept 3", "concept 4", "my_non_standard_concept", "ibuprofen", "Acetaminophen Oral Tablet [PARACETAMOL ALMUS]"),
      "domain_id" = c("drug", "drug", "observation", "condition", "drug", "drug", "drug"),
      "vocabulary_id" = NA_character_,
      "concept_class_id" = NA_character_,
      "concept_code" = NA_character_,
      "valid_start_date" = as.Date(NA),
      "valid_end_date" = as.Date(NA)
    )
  )
  # person 1 - 2 overlapping records and another
  # person 2 - 2 non-overlapping records
  # person 3 one record
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "drug_exposure",
    table = dplyr::tibble(
      "drug_exposure_id" = 1:17 |> as.integer(),
      "person_id" = c(1, 1, 1, 2, 2, 3, 1, 2, 3, 1, 1, 1, 1, 1, 2, 2, 2) |> as.integer(),
      "drug_concept_id" = c(1, 1,  1, 1, 1, 2, 3, 3, 5, 1177480L, 1177480L, 1177480L, 43157344L, 43157344L, 43157344L, 43157344L, 43157344L) |> as.integer(),
      "drug_exposure_start_date" = as.Date(c(
        "2020-07-11", "2020-10-27", "2024-02-09", "2022-01-20", "2023-01-11",
        "2022-03-11", "2020-01-01", "2020-01-04", "2005-01-04", "2000-01-01",
        "2001-08-01", "2000-03-01", "2000-11-01", "2000-02-01", "1999-11-01",
        "2000-02-01", "2000-02-02"
      )),
      "drug_exposure_end_date" = as.Date(c(
        "2020-11-11", "2020-10-29", "2024-02-10", "2022-01-21", "2023-01-12",
        "2022-03-12", "2020-01-10", "2020-01-14", "2005-01-04", "2000-10-02",
        "2001-08-03", "2000-03-01", "2000-11-01", "2001-02-01", "2001-02-01",
        "2000-03-01", "2000-04-02"
      )),
      "drug_type_concept_id" = 1L,
      "drug_source_concept_id" = c(rep(NA_integer_, 7), 99L, rep(NA_integer_, 9)),
      "sig" = letters[1:17]
    )
  )
  # end date < start date
  # end date = NA
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "condition_occurrence",
    table = dplyr::tibble(
      "condition_occurrence_id" = 1:3 |> as.integer(),
      "person_id" = c(1, 1, 1) |> as.integer(),
      "condition_concept_id" = 5L,
      "condition_start_date" = as.Date(c("2019-09-11",
                                         "2018-10-27",
                                         "2016-02-09")),
      "condition_end_date" = as.Date(c("2019-10-11",
                                       "2017-10-29",
                                       NA)),
      "condition_type_concept_id" = 1L,
      "condition_source_concept_id" = NA_integer_
    )
  )
  # subset cohort
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "subset_cohort",
    table = dplyr::tibble(
      "cohort_definition_id" = c(1L, 2L),
      "subject_id" = c(1L, 2L),
      "cohort_start_date" = as.Date(c("2019-09-11", "2018-10-27")),
      "cohort_end_date" = as.Date(c("2019-10-11", "2018-10-29"))
    )
  )
  cdm <- cdm |> copyCdm()
  cdm$subset_cohort <- cdm$subset_cohort |> omopgenerics::newCohortTable()

  # expected error
  expect_error(customCohort(cdm = cdm,
               table = "drug_exposure",
               field = "sig",
               values = "a",
               name = "cohort_a"))
  expect_error(customCohort(cdm = cdm,
               table = "x",
               field = "sig",
               values = list(a = "a"),
               name = "cohort_a"))
  expect_error(customCohort(cdm = cdm,
               table = "drug_exposure",
               field = "not_a_column",
               values = list(a = "a"),
               name = "cohort_a"))
  expect_error(customCohort(cdm = cdm,
               table = "drug_exposure",
               field = c("sig", "drug_concept_id"),
               values = list(a = "a"),
               name = "cohort_a"))



  cdm$cohort_a <- conceptCohort(cdm = cdm,
                                conceptSet = list(a = 1177480L),
                                name = "cohort_a")
  cdm$cohort_b <- customCohort(cdm = cdm,
                               table = "drug_exposure",
                               field = "drug_concept_id",
                               values = list(b = 1177480L),
                               name = "cohort_b")
  expect_identical(cdm$cohort_a |> collectCohort(id = 1),
                   cdm$cohort_b |> collectCohort(id = 1))

  # simple examples
  expect_no_error(customCohort(cdm = cdm,
               table = "drug_exposure",
               field = "sig",
               values = list(a = "a",
                             b = c("c", "d")),
               name = "custom_cohort"))
  expect_no_error(customCohort(cdm = cdm,
               table = "drug_exposure",
               field = "sig",
               values = list(a = "a",
                             b = c("c", "d")),
               name = "custom_cohort", exit = "event_start_date"))


  cdm$cohort_sub <- conceptCohort(cdm = cdm,
                                conceptSet = list(a = 43157344L),
                                name = "cohort_sub") |>
    dplyr::filter(subject_id == "2")
  expect_no_error(cdm$custom_cohort_sub <- customCohort(cdm = cdm,
                               table = "drug_exposure",
                               field = "sig",
                               values = list(a = "a",
                                             b = c("c", "d")),
                               name = "custom_cohort_sub",
                               subsetCohort = "cohort_sub"))
  expect_identical(cdm$custom_cohort_sub |>
    dplyr::pull("subject_id") |>
    unique(),
  2L)

})

