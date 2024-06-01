test_that("matchCohorts runs without errors", {
  testthat::skip_on_cran()
  # Create cdm object
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(180, 180),
    overwrite = TRUE
  )

  expect_no_error(a <- matchCohorts(cohort = cdm$cases,
                                    name = "new_cohort",
                                    ratio = 2))

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009, other = 4141052, other1 = 432526),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(10,10),
    overwrite = TRUE)

  expect_no_error(matchCohorts(cohort = cdm$cases,
                               name = "new_cohort"))

  expect_no_error(matchCohorts(cohort = cdm$cases,
                               name = "new_cohort",
                               ratio = 3))

  expect_no_error(matchCohorts(cohort = cdm$cases,
                               name = "new_cohort",
                               ratio = Inf))

  expect_no_error(matchCohorts(cohort = cdm$cases,
                               name = "new_cohort",
                               matchSex = FALSE,
                               matchYearOfBirth = TRUE))

  expect_no_error(matchCohorts(cohort = cdm$cases,
                               name = "new_cohort",
                               matchSex = TRUE,
                               matchYearOfBirth = FALSE))

  expect_no_error(b <- matchCohorts(cohort = cdm$cases,
                                    name = "new_cohort",
                                    matchSex = FALSE,
                                    matchYearOfBirth = FALSE))

  expect_no_error(a <- matchCohorts(cohort = cdm$cases,
                                    name = "new_cohort",
                                    cohortId = c(1,2),
                                    matchSex = TRUE,
                                    matchYearOfBirth = TRUE,
                                    ratio = 2))

  # empty set
  cdm <- omopgenerics::emptyCohortTable(cdm, name = "cohort")
  expect_no_error(empty_cohort <- matchCohorts(cohort = cdm$cohort, name = "empty_cohort"))
  expect_equal(cdm$cohort |> dplyr::collect(), empty_cohort |> dplyr::collect())


  # expect errors
  expect_error(matchCohorts(cohort = dplyr::tibble()))
  expect_error(matchCohorts(cohort = cdm$cases, ratio = -0.5))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("matchCohorts, no duplicated people within a cohort", {
  testthat::skip_on_cran()
  followback <- 180

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009, other = 432526),
    name = "cohort",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  cdm$new_cohort <- matchCohorts(cohort = cdm$cohort,
                                 name = "new_cohort",
                                 matchSex = TRUE,
                                 matchYearOfBirth = TRUE,
                                 ratio = 1)

  p1 <- cdm$new_cohort %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(subject_id) %>%
    dplyr::pull() %>%
    length()
  expect_true(length(p1) == length(unique(p1)))


  cdm$new_cohort <- matchCohorts(cohort = cdm$cohort,
                                 name = "new_cohort",
                                 matchSex = TRUE,
                                 matchYearOfBirth = TRUE,
                                 ratio = 5)
  p1 <- cdm$new_cohort %>%
    dplyr::filter(cohort_definition_id == 2) %>%
    dplyr::select(subject_id) %>%
    dplyr::pull() %>%
    length()
  expect_true(length(p1) == length(unique(p1)))

})

test_that("check that we obtain expected result when ratio is 1", {
  testthat::skip_on_cran()
  followback <- 180

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(c_1 = 317009, c_2 = 432526, c_3 = 4141052),
    name = "cohort",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  # Number of counts for the initial cohorts are the same as in the matched cohorts
  matched_cohorts <- matchCohorts(cohort = cdm$cohort,
                                  name = "new_cohort",
                                  matchSex = TRUE,
                                  matchYearOfBirth = TRUE,
                                  ratio = 1)

  expect_true(nrow(omopgenerics::cohortCount(matched_cohorts) %>%
                     dplyr::left_join(omopgenerics::settings(matched_cohorts),
                                      by = "cohort_definition_id") %>%
                     dplyr::filter(stringr::str_detect(cohort_name, "c_1"))  %>%
                     dplyr::select("number_records") %>%
                     dplyr::distinct()) == 1)
  expect_true(nrow(omopgenerics::cohortCount(matched_cohorts) %>%
                     dplyr::left_join(omopgenerics::settings(matched_cohorts),
                                      by = "cohort_definition_id") %>%
                     dplyr::filter(stringr::str_detect(cohort_name, "c_2"))  %>%
                     dplyr::select("number_records") %>%
                     dplyr::distinct()) == 1)
  expect_true(nrow(omopgenerics::cohortCount(matched_cohorts) %>%
                     dplyr::left_join(omopgenerics::settings(matched_cohorts),
                                      by = "cohort_definition_id") %>%
                     dplyr::filter(stringr::str_detect(cohort_name, "c_3"))  %>%
                     dplyr::select("number_records") %>%
                     dplyr::distinct()) == 1)



  # Everybody has a match
  n <- matched_cohorts %>%
    dplyr::summarise(n = max(.data$cohort_definition_id, na.rm = TRUE)/2) %>%
    dplyr::pull()

  cohorts <- matched_cohorts %>%
    dplyr::select("person_id" = "subject_id", "cohort_definition_id") %>%
    dplyr::inner_join(
      cdm$person %>%
        dplyr::select("person_id", "gender_concept_id", "year_of_birth"),
      by = "person_id"
    )

  expect_true(is.na(nrow(cohorts %>%
                           dplyr::filter(.data$cohort_definition_id %in% c(1,2,3)) %>%
                           dplyr::left_join(
                             cohorts %>%
                               dplyr::filter(.data$cohort_definition_id %in% c(4,5,6)) %>%
                               dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id-n),
                             by = c("cohort_definition_id", "gender_concept_id", "year_of_birth")
                           ) %>%
                           dplyr::filter(
                             is.na(person_id.y)
                           ))))
})

test_that("test exactMatchingCohort works if there are no subjects", {
  testthat::skip_on_cran()
  followback  <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )
  cdm$cases <- cdm$cases %>% dplyr::filter(subject_id == 0)
  cdm$new_cohort <- matchCohorts(
    cohort = cdm$cases,
    name = "new_cohort"
  )
  expect_true(cdm$new_cohort %>% dplyr::tally() %>% dplyr::pull(n) == 0)
})

test_that("test exactMatchingCohort works if one of the cohorts does not have any people", {
  testthat::skip_on_cran()
  followback  <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(c_1 = 317009, c_2 = 8505),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  expect_no_error(
    cdm$new_cohort <- matchCohorts(cohort = cdm$cases,
                                   name = "new_cohort",
                                   matchSex = TRUE,
                                   matchYearOfBirth = TRUE,
                                   ratio = 1)
  )
})

test_that("test exactMatchingCohort with a ratio bigger than 1", {
  testthat::skip_on_cran()
  followback  <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 1000),
    conceptSet = list(c_1 = 317009, c_2 = 432526),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  expect_no_error(
    a <- matchCohorts(cohort = cdm$cases,
                      name = "new_cohort",
                      matchSex = TRUE,
                      matchYearOfBirth = TRUE,
                      ratio = 5)
  )
})

test_that("test exactMatchingCohort with a ratio bigger than 1", {
  testthat::skip_on_cran()
  # Generate mock data
  cdmMock <- DrugUtilisation::mockDrugUtilisation(
    numberIndividuals = 10,
    person = tibble::tibble("person_id" = seq(1,10,1),
                            "gender_concept_id" = rep(8532,10),
                            "year_of_birth" = rep(1980, 10),
                            "day_of_birth"  = rep(1, 10),
                            "birth_date_time" = as.Date(rep("1980-04-01",10)),
                            "month_of_birth"  = rep(4, 10),
                            "race_concept_id" = NA_character_,
                            "ethnicity_concept_id" = NA_character_),
    condition_occurrence = tibble::tibble("condition_occurrence_id" = seq(1,10,1),
                                          "person_id" = seq(1,10,1),
                                          "condition_concept_id" = c(317009,317009,4266367,4266367,rep(1,6)),
                                          "condition_start_date" = as.Date(c("2017-10-30","2003-01-04","2014-12-15","2010-09-09","2004-08-26","1985-03-31","1985-03-13","1985-07-11","1983-11-07","2020-01-13")),
                                          "condition_end_date"   = as.Date(c("2017-11-01","2003-01-05","2014-12-16","2010-09-10","2004-08-27","1985-04-01","1985-03-14","1985-07-12","1983-11-08","2020-01-14")),
                                          "condition_type_concept_id" = rep(32020,10)),
    observation_period = tibble::tibble("observation_period_id" = seq(1,10,1),
                                        "person_id" = seq(1,10,1),
                                        "observation_period_start_date" = as.Date(rep("1984-01-01",10)),
                                        "observation_period_end_date"   = as.Date(rep("2021-01-01",10)),
                                        "period_type_concept_id"        = 44814724)
  )

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = cdmMock,
    conceptSet = list(c_1 = 317009, c_2 = 4266367),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(0,0),
    overwrite = TRUE
  )

  cdm$new_cohort <- matchCohorts(cohort = cdm$cases,
                                 name = "new_cohort",
                                 matchSex = TRUE,
                                 matchYearOfBirth = TRUE,
                                 ratio = 4)

  expect_true(
    cdm[["new_cohort"]] %>%
      cohortCount() |>
      dplyr::filter(.data$cohort_definition_id %in% omopgenerics::getCohortId(
        cdm$new_cohort, "c_1"
      )) %>%
      dplyr::pull("number_subjects") |>
      sum() == 2
  )
  expect_true(
    cdm[["new_cohort"]] %>%
      cohortCount() |>
      dplyr::filter(.data$cohort_definition_id %in% omopgenerics::getCohortId(
        cdm$new_cohort, "c_1_matched"
      )) %>%
      dplyr::pull("number_subjects") |>
      sum() == 8
  )
  expect_true(
    cdm[["new_cohort"]] %>%
      cohortCount() |>
      dplyr::filter(.data$cohort_definition_id %in% omopgenerics::getCohortId(
        cdm$new_cohort, "c_2"
      )) %>%
      dplyr::pull("number_subjects") |>
      sum() == 2
  )
  expect_true(
    cdm[["new_cohort"]] %>%
      cohortCount() |>
      dplyr::filter(.data$cohort_definition_id %in% omopgenerics::getCohortId(
        cdm$new_cohort, "c_2_matched"
      )) %>%
      dplyr::pull("number_subjects") |>
      sum() == 8
  )

  outc <- cdm[["new_cohort"]] %>%
    dplyr::filter(subject_id == 5) %>% dplyr::summarise(cohort_start_date) %>%
    dplyr::pull() %in% as.Date(c("2017-10-30","2003-01-04","2014-12-15","2010-09-09"))
  expect_true(unique(outc) == TRUE)
})
