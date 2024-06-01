test_that("mearurementCohorts works", {
  cdm <- mockCohortConstructor(con = NULL)
  cdm$concept <- cdm$concept |>
    dplyr::union_all(
      dplyr::tibble(
        concept_id = c(4326744, 4298393, 45770407, 8876, 4124457, 999999, 123456),
        concept_name = c("Blood pressure", "Systemic blood pressure",
                         "Baseline blood pressure", "millimeter mercury column",
                         "Normal range", "Normal", "outObs"),
        domain_id = "Measurement",
        vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "UCUM", "SNOMED", "SNOMED", "hi"),
        standard_concept = "S",
        concept_class_id = c("Observable Entity", "Observable Entity",
                             "Observable Entity", "Unit", "Qualifier Value",
                             "Qualifier Value", "hi"),
        concept_code = NA,
        valid_start_date = NA,
        valid_end_date = NA,
        invalid_reason = NA
      )
    )
  cdm$measurement <- dplyr::tibble(
    measurement_id = 1:7,
    person_id = c(1, 1, 2, 3, 3, 1, 1),
    measurement_concept_id = c(4326744, 4298393, 4298393, 45770407, 45770407, 123456, 123456),
    measurement_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20", "1900-01-01", "2050-01-01")),
    measurement_type_concept_id = NA,
    value_as_number = c(100, 125, NA, NA, NA, NA, NA),
    value_as_concept_id = c(0, 0, 0, 4124457, 999999, 0, 0),
    unit_concept_id = c(8876, 8876, 0, 0, 0, 0, 0)
  )
  cdm <- cdm |> copyCdm()

  # simple example ----
  cdm$cohort <- measurementCohort(
    cdm = cdm,
    name = "cohort",
    conceptSet = list("normal_blood_pressure" = c(4326744, 4298393, 45770407)),
    valueAsConcept = c(4124457),
    valueAsNumber = list("8876" = c(70, 120))
  )
  expect_equal(
    collectCohort(cdm$cohort, 1),
    dplyr::tibble(
      subject_id = c(1, 3),
      cohort_start_date = as.Date(c("2000-07-01", "2015-02-19")),
      cohort_end_date = as.Date(c("2000-07-01", "2015-02-19"))
    ))
  expect_true(cdm$cohort |> attrition() |> dplyr::pull("reason") == "Initial qualifying events")
  expect_true(settings(cdm$cohort)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id |> sort() == c(4298393, 4326744, 45770407)))

  # non valid concept ----
  cdm$cohort3 <- measurementCohort(
    cdm = cdm,
    name = "cohort3",
    conceptSet = list("normal_blood_pressure" = c(4326744, 4298393, 45770407, 12345)),
    valueAsConcept = c(4124457),
    valueAsNumber = list("8876" = c(70, 120))
  )
  expect_equal(
    collectCohort(cdm$cohort3, 1),
    dplyr::tibble(
      subject_id = c(1, 3),
      cohort_start_date = as.Date(c("2000-07-01", "2015-02-19")),
      cohort_end_date = as.Date(c("2000-07-01", "2015-02-19"))
    ))
  expect_true(cdm$cohort3 |> attrition() |> dplyr::pull("reason") == "Initial qualifying events")
  expect_true(settings(cdm$cohort3)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort3, "cohort_codelist") |> dplyr::collect()
  expect_true(all(c(4298393, 4326744, 45770407) %in% codes$concept_id))

  # no values ----
  cdm$cohort4 <- measurementCohort(
    cdm = cdm,
    name = "cohort4",
    conceptSet = list("normal_blood_pressure" = c(4326744, 4298393, 45770407))
  )
  expect_equal(
    collectCohort(cdm$cohort4, 1),
    dplyr::tibble(
      subject_id = c(1, 1, 2, 3, 3),
      cohort_start_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20")),
      cohort_end_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20"))
    ))
  expect_true(cdm$cohort4 |> attrition() |> dplyr::pull("reason") == "Initial qualifying events")
  expect_true(settings(cdm$cohort4)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort4, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(4298393, 4326744, 45770407)))

  # number values ----
  cdm$cohort5 <- measurementCohort(
    cdm = cdm,
    name = "cohort5",
    conceptSet = list("normal_blood_pressure" = c(4326744, 4298393, 45770407)),
    valueAsNumber = list("8876" = c(70, 120), "908" = c(800, 900))
  )
  expect_equal(
    collectCohort(cdm$cohort5, 1),
    dplyr::tibble(
      subject_id = c(1),
      cohort_start_date = as.Date(c("2000-07-01")),
      cohort_end_date = as.Date(c("2000-07-01"))
    ))
  expect_true(cdm$cohort5 |> attrition() |> dplyr::pull("reason") == "Initial qualifying events")
  expect_true(settings(cdm$cohort5)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort5, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(4298393, 4326744, 45770407)))

  # concept values ----
  cdm$cohort6 <- measurementCohort(
    cdm = cdm,
    name = "cohort6",
    conceptSet = list("normal_blood_pressure" = c(4326744, 4298393, 45770407)),
    valueAsConcept = c(4124457, 999999, 12345)
  )
  expect_equal(
    collectCohort(cdm$cohort6, 1),
    dplyr::tibble(
      subject_id = c(3, 3),
      cohort_start_date = as.Date(c("2015-02-19", "2015-02-20")),
      cohort_end_date = as.Date(c("2015-02-19", "2015-02-20"))
    ))
  expect_true(cdm$cohort6 |> attrition() |> dplyr::pull("reason") == "Initial qualifying events")
  expect_true(settings(cdm$cohort6)$cohort_name == "normal_blood_pressure")
  codes <- attr(cdm$cohort6, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(4298393, 4326744, 45770407)))

  # more than 1 cohort ----
  cdm$cohort7 <- measurementCohort(
    cdm = cdm,
    name = "cohort7",
    conceptSet = list("c1" = c(4326744), "c2" = c(4298393, 45770407))
  )
  expect_equal(
    collectCohort(cdm$cohort7, 1),
    dplyr::tibble(
      subject_id = c(1),
      cohort_start_date = as.Date(c("2000-07-01")),
      cohort_end_date = as.Date(c("2000-07-01"))
    ))
  expect_equal(
    collectCohort(cdm$cohort7, 2),
    dplyr::tibble(
      subject_id = c(1, 2, 3, 3),
      cohort_start_date = as.Date(c("2000-12-11", "2002-09-08", "2015-02-19","2015-02-20")),
      cohort_end_date = as.Date(c("2000-12-11", "2002-09-08", "2015-02-19", "2015-02-20"))
    ))
  expect_true(all(cdm$cohort7 |> attrition() |> dplyr::pull("reason") == c("Initial qualifying events", "Initial qualifying events")))
  expect_true(all(all(settings(cdm$cohort7)$cohort_name == c("c1", "c2"))))
  codes <- attr(cdm$cohort7, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id[codes$codelist_name == "c1"] == c(4326744)))
  expect_true(all(codes$concept_id[codes$codelist_name == "c2"] == c(4298393, 45770407)))

  # out of obs ----
  cdm$cohort8 <- measurementCohort(
    cdm = cdm,
    name = "cohort8",
    conceptSet = list("c1" = c(123456))
  )
  expect_true(cdm$cohort8 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(cdm$cohort8 |> attrition() |> dplyr::pull("reason") == "Initial qualifying events")
  expect_true(settings(cdm$cohort8)$cohort_name == "c1")
  codes <- attr(cdm$cohort8, "cohort_codelist") |> dplyr::collect()
  expect_true(all(codes$concept_id  |> sort() == c(123456)))

  # empty cohort ----
  cdm$cohort9 <- measurementCohort(
    cdm = cdm,
    name = "cohort9",
    conceptSet = list("c1" = c(1234567))
  )
  expect_true(cdm$cohort9 |> dplyr::tally() |> dplyr::pull("n") == 0)
  expect_true(cdm$cohort9 |> attrition() |> dplyr::pull("reason") == "Initial qualifying events")
  expect_true(settings(cdm$cohort9)$cohort_name == "c1")
  # codes <- attr(cdm$cohort9, "cohort_codelist") |> dplyr::collect()
  # expect_true(nrow(codes) == 1)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("expected errors", {
  testthat::skip_on_cran()
  cdm <- mockCohortConstructor(con = NULL)
  cdm$concept <- cdm$concept |>
    dplyr::union_all(
      dplyr::tibble(
        concept_id = c(4326744, 4298393, 45770407, 8876, 4124457),
        concept_name = c("Blood pressure", "Systemic blood pressure",
                         "Baseline blood pressure", "millimeter mercury column",
                         "Normal range"),
        domain_id = "Measurement",
        vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "UCUM", "SNOMED"),
        standard_concept = "S",
        concept_class_id = c("Observable Entity", "Observable Entity",
                             "Observable Entity", "Unit", "Qualifier Value"),
        concept_code = NA,
        valid_start_date = NA,
        valid_end_date = NA,
        invalid_reason = NA
      )
    )
  cdm$measurement <- dplyr::tibble(
    measurement_id = 1:4,
    person_id = c(1, 1, 2, 3),
    measurement_concept_id = c(4326744, 4298393, 4298393, 45770407),
    measurement_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19")),
    measurement_type_concept_id = NA,
    value_as_number = c(100, 125, NA, NA),
    value_as_concept_id = c(0, 0, 0, 4124457),
    unit_concept_id = c(8876, 8876, 0, 0)
  )
  cdm <- cdm |> copyCdm()

  # simple example
  expect_error(
    measurementCohort(
      cdm = cdm,
      name = "cohort",
      conceptSet = list(c(4326744, 4298393, 45770407)),
      valueAsConcept = c(4124457),
      valueAsNumber = list("8876" = c(70, 120))
    )
  )
  expect_error(
    measurementCohort(
      cdm = cdm,
      name = "cohort",
      conceptSet = list("name " = c(4326744, 4298393, 45770407)),
      valueAsConcept = c(4124457),
      valueAsNumber = list("8876" = c(700, 120))
    )
  )

  PatientProfiles::mockDisconnect(cdm)
})
