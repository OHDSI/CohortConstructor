test_that("expected output", {
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(c_1 = 317009, c_2 = 432526, c_3 = 4141052),
    name = "cohort",
    end  = "observation_period_end_date"
  )

 overlap1 <- summariseCohortOverlap(cdm$cohort,
                                   restrictToFirstEntry = TRUE,
                                   timing = c("min", "q25",
                                              "median","q75",
                                              "max"))
 expect_equal(colnames(omopgenerics::emptySummarisedResult()),
              colnames(overlap1))

 overlap2 <- summariseCohortOverlap(cdm$cohort,
                                   restrictToFirstEntry = FALSE,
                                   timing = c("min", "q25",
                                              "median","q75",
                                              "max"))
 expect_equal(colnames(omopgenerics::emptySummarisedResult()),
              colnames(overlap2))


 overlap3 <- summariseCohortOverlap(cdm$cohort,
                                   restrictToFirstEntry = TRUE,
                                   timing = c("min",
                                              "max"))
 expect_equal(colnames(omopgenerics::emptySummarisedResult()),
              colnames(overlap3))

 overlap4 <- summariseCohortOverlap(cdm$cohort,
                                   restrictToFirstEntry = TRUE,
                                   timing = NULL)
 expect_equal(colnames(omopgenerics::emptySummarisedResult()),
              colnames(overlap4))

 CDMConnector::cdm_disconnect(cdm)

  })
