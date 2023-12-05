test_that("generateMatchedCohortSet runs without errors", {
  # Create cdm object
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(180, 180),
    overwrite = TRUE)

  expect_no_error(a <- generateMatchedCohortSet(cdm,
                                                name = "new_cohort",
                                                targetCohortName = "cases",
                                                ratio = 2))

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009, other = 4141052, other1 = 432526),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(10,10),
    overwrite = TRUE)

  expect_no_error(generateMatchedCohortSet(cdm,
                                           name = "new_cohort",
                                           targetCohortName = "cases"))

  expect_no_error(generateMatchedCohortSet(cdm,
                                           name = "new_cohort",
                                           targetCohortName = "cases",
                                           ratio = 3))

  expect_no_error(generateMatchedCohortSet(cdm,
                                           name = "new_cohort",
                                           targetCohortName = "cases",
                                           ratio = Inf))

  expect_no_error(generateMatchedCohortSet(cdm,
                                           name = "new_cohort",
                                           matchSex = FALSE,
                                           matchYearOfBirth = TRUE,
                                           targetCohortName = "cases"))

  expect_no_error(generateMatchedCohortSet(cdm,
                                           name = "new_cohort",
                                           matchSex = TRUE,
                                           matchYearOfBirth = FALSE,
                                           targetCohortName = "cases"))

  expect_no_error(b <- generateMatchedCohortSet(cdm,
                                                name = "new_cohort",
                                                matchSex = FALSE,
                                                matchYearOfBirth = FALSE,
                                                targetCohortName = "cases"))

  expect_no_error(a <- generateMatchedCohortSet(cdm,
                                                name = "new_cohort",
                                                targetCohortName = "cases",
                                                targetCohortId = c(1,2),
                                                matchSex = TRUE,
                                                matchYearOfBirth = TRUE,
                                                ratio = 2))

})


test_that("generateMatchedCohortSet, no duplicated people within a cohort", {
  followback <- 180

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009, other = 432526),
    name = "cohort",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  a <- generateMatchedCohortSet(cdm,
                                name = "new_cohort",
                                targetCohortName = "cohort",
                                targetCohortId = NULL,
                                matchSex = TRUE,
                                matchYearOfBirth = TRUE,
                                ratio = 1)

  p1 <- a$new_cohort %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(subject_id) %>%
    dplyr::pull() %>%
    length()
  expect_true(length(p1) == length(unique(p1)))


  a <- generateMatchedCohortSet(cdm,
                                name = "new_cohort",
                                targetCohortName = "cohort",
                                targetCohortId = NULL,
                                matchSex = TRUE,
                                matchYearOfBirth = TRUE,
                                ratio = 5)
  p1 <- a$new_cohort %>%
    dplyr::filter(cohort_definition_id == 2) %>%
    dplyr::select(subject_id) %>%
    dplyr::pull() %>%
    length()
  expect_true(length(p1) == length(unique(p1)))

})

test_that("check that we obtain expected result when ratio is 1", {
  followback <- 180

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(c1 = 317009, c2 = 432526, c3 = 4141052),
    name = "cohort",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  # Number of counts for the initial cohorts are the same as in the matched cohorts
  matched_cohorts <- generateMatchedCohortSet(cdm,
                                              name = "new_cohort",
                                              targetCohortName = "cohort",
                                              targetCohortId = NULL,
                                              matchSex = TRUE,
                                              matchYearOfBirth = TRUE,
                                              ratio = 1)
  expect_true(
    length(CDMConnector::cohort_count(matched_cohorts[["new_cohort"]]) %>%
             dplyr::select("number_records") %>%
             dplyr::pull() %>%
             unique()) == 3
  )

  # Everybody has a matched
  n <- matched_cohorts[["new_cohort"]] %>%
    dplyr::summarise(n = max(.data$cohort_definition_id)/2) %>%
    dplyr::pull()

  cohorts <- matched_cohorts[["new_cohort"]] %>%
    dplyr::select("person_id" = "subject_id", "cohort_definition_id") %>%
    dplyr::inner_join(
      matched_cohorts[["person"]] %>%
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
  expect_no_error(
    generateMatchedCohortSet(
      cdm,
      name = "new_cohort",
      targetCohortName = "cases",
    )
  )
})


test_that("test exactMatchingCohort works if one of the cohorts does not have any people", {
  followback  <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(c1 = 317009, c2 = 1),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )


  expect_no_error(
    generateMatchedCohortSet(cdm,
                             name = "new_cohort",
                             targetCohortName = "cases",
                             targetCohortId = NULL,
                             matchSex = TRUE,
                             matchYearOfBirth = TRUE,
                             ratio = 1)
  )
})



test_that("test exactMatchingCohort with a ratio bigger than 1", {
  followback  <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 1000),
    conceptSet = list(c1 = 317009, c2 = 432526),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )



  expect_no_error(
    a <- generateMatchedCohortSet(cdm,
                                  name = "new_cohort",
                                  targetCohortName = "cases",
                                  targetCohortId = NULL,
                                  matchSex = TRUE,
                                  matchYearOfBirth = TRUE,
                                  ratio = 5)
  )
})


test_that("test exactMatchingCohort with a ratio bigger than 1", {
  # Generate mock data
  cdmMock <- DrugUtilisation::mockDrugUtilisation(
    numberIndividuals = 10,
    person = tibble::tibble("person_id" = seq(1,10,1),
                            "gender_concept_id" = rep(8532,10),
                            "year_of_birth" = rep(1980, 10),
                            "day_of_birth"  = rep(1, 10),
                            "birth_date_time" = rep(as.Date(1980,04,01),10),
                            "month_of_birth"  = rep(4, 10)),
    condition_occurrence = tibble::tibble("condition_ocurrence_id" = seq(1,10,1),
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
    conceptSet = list(c1 = 317009, c2 = 4266367),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(0,0),
    overwrite = TRUE
  )

  a <- generateMatchedCohortSet(cdm,
                                name = "new_cohort",
                                targetCohortName = "cases",
                                targetCohortId = NULL,
                                matchSex = TRUE,
                                matchYearOfBirth = TRUE,
                                ratio = 4)

  expect_true(a[["new_cohort"]] %>%
                dplyr::filter(cohort_definition_id %in% c(1,3)) %>%
                dplyr::summarise(subject_id) %>%
                dplyr::distinct() %>% dplyr::pull() %>% length() == 10)
  expect_true(a[["new_cohort"]] %>%
                dplyr::filter(cohort_definition_id %in% c(2,4)) %>%
                dplyr::summarise(subject_id) %>%
                dplyr::distinct() %>% dplyr::pull() %>% length() == 10)
  expect_true(a[["new_cohort"]] %>%
                dplyr::filter(cohort_definition_id %in% c(1,3)) %>%
                dplyr::summarise(cohort_start_date) %>%
                dplyr::distinct() %>% dplyr::pull() %>% length() == 2)
  expect_true(a[["new_cohort"]] %>%
                dplyr::filter(cohort_definition_id %in% c(2,4)) %>%
                dplyr::summarise(cohort_start_date) %>%
                dplyr::distinct() %>% dplyr::pull() %>% length() == 2)


  outc <- a[["new_cohort"]] %>%
    dplyr::filter(subject_id == 5) %>% dplyr::summarise(cohort_start_date) %>% dplyr::pull() %in% c("2017-10-30","2003-01-04","2014-12-15","2010-09-09")
  expect_true(unique(outc) == TRUE)

})


