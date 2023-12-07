test_that("requiring presence in another cohort", {
  cdm <- PatientProfiles::mockPatientProfiles(patient_size = 100,
                                              drug_exposure_size = 100)

  cdm$cohort3 <-  requireCohortIntersectFlag(x = cdm$cohort1,
                             targetCohortTable = "cohort2",
                             targetCohortId = 1,
                             window = c(-Inf, Inf))

 expect_true(all(cdm$cohort3  %>%
    dplyr::distinct(subject_id) %>%
    dplyr::pull() %in%
  intersect(cdm$cohort1 %>%
    dplyr::distinct(subject_id) %>%
    dplyr::pull(),
  cdm$cohort2 %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::distinct(subject_id) %>%
    dplyr::pull())))

 cdm$cohort4 <-  requireCohortIntersectFlag(x = cdm$cohort1,
                                            targetCohortTable = "cohort2",
                                            targetCohortId = 2,
                                            window = c(-Inf, Inf))
 expect_true(all(cdm$cohort4 %>%
                   dplyr::distinct(subject_id) %>%
                   dplyr::pull() %in%
                   intersect(cdm$cohort1 %>%
                               dplyr::distinct(subject_id) %>%
                               dplyr::pull(),
                             cdm$cohort2 %>%
                               dplyr::filter(cohort_definition_id == 2) %>%
                               dplyr::distinct(subject_id) %>%
                               dplyr::pull())))


 # expected errors
 # only support one target id at the moment
 expect_error(requireCohortIntersectFlag(x = cdm$cohort1,
                            targetCohortTable = "cohort2",
                            targetCohortId = c(1,2),
                            window = c(-Inf, Inf)))

 expect_error(requireCohortIntersectFlag(x = cdm$cohort1,
                                         targetCohortTable = "cohort22", # does not exist
                                         targetCohortId = 1,
                                         window = c(-Inf, Inf)))
 expect_error(requireCohortIntersectFlag(x = cdm$cohort1,
                                         targetCohortTable = "cohort2",
                                         targetCohortId = 10, # does not exist
                                         window = c(-Inf, Inf)))

 CDMConnector::cdm_disconnect(cdm)

  })


