test_that("simple duckdb checks", {
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
  cdm$cohort2 <- observationPeriodCohort(cdm = cdm, name = "cohort2")

  expect_no_error(
    cdm$cohort3 <- cdm$cohort1 |>
      trimDemographics(
        ageRange = list(
          c(0, Inf), c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)
        ),
        sex = c("FEMale", "malE", "bOTh"),
        minPriorObservation = c(0, 365),
        minFutureObservation = c(0, 365),
        name = "cohort3"
      )
  )

})
