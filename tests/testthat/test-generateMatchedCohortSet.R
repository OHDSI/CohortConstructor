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
  # cdm[["person"]] <- tibble::tibble("person_id" )
  # # Generate mock data
  # cdm[["person"]] <- tibble::tibble("person_id" = c(1,2))
  # cdm <- DrugUtilisation::generateConceptCohortSet(
  #   cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 1000),
  #   conceptSet = list(c1 = 317009, c2 = 432526),
  #   name = "cases",
  #   end  = "observation_period_end_date",
  #   requiredObservation = c(followback,followback),
  #   overwrite = TRUE
  # )
  #
  #
  #
  # expect_no_error(
  #   a <- generateMatchedCohortSet(cdm,
  #                                 name = "new_cohort",
  #                                 targetCohortName = "cases",
  #                                 targetCohortId = NULL,
  #                                 matchSex = TRUE,
  #                                 matchYearOfBirth = TRUE,
  #                                 ratio = 5)
  # )
})

#
#
# a[["new_cohort"]] %>%
#   dplyr::inner_join(a[["person"]] %>%
#                       dplyr::select("subject_id" = "person_id", "gender_concept_id", "year_of_birth"),
#                     by = "subject_id") %>%
#   dplyr::filter(cohort_definition_id %in% c(1,3)) %>%
#   dplyr::group_by(gender_concept_id, year_of_birth) %>%
#   dplyr::mutate(n = dplyr::row_number()) %>% print(n = 100)
