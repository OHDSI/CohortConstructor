test_that("require flag in concept", {
  cdm <- DrugUtilisation::mockDrugUtilisation()
  conceptSet <- CodelistGenerator::getCandidateCodes("Influenza")

  requireConceptIntersectFlag(cdm$cohort1, conceptSet)
})
