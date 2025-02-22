
# check if we will need to filter based on cohort ID
needsIdFilter <- function(cohort, cohortId){
  !identical(omopgenerics::cohortCount(cohort) |>
             dplyr::pull("cohort_definition_id"),
             cohortId)
}
