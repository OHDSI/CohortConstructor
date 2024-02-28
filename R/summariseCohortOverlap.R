#' Summarise cohort overlap
#'
#' @param cohort  A cohort table in a cdm reference
#' @param restrictToFirstEntry If TRUE only an individual's first entry per
#' cohort will be considered. If FALSE all entries per individual will be
#' considered
#' @param timing Summary statistics for timing. If NULL, timings between cohort
#' entries will not be considered
#'
#' @return A summarised result
#' @export
#'
#' @examples
summariseCohortOverlap <- function(cohort,
                          restrictToFirstEntry = TRUE,
                          timing = c("min", "q25",
                                     "median","q75",
                                     "max")){

  # validate inputs


  # add cohort names
  cdm <- omopgenerics::cdmReference(cohort)
  name <- attr(cohort, "tbl_name") # change to omopgenerics::getTableName(cohort)  when og is released

  cdm[[name]] <- PatientProfiles::addCohortName(cdm[[name]])

  if(isTRUE(restrictToFirstEntry)){
    cdm[[name]] <- cdm[[name]] %>%
      restrictToFirstEntry()
  }

  # should we use addCohortIntersectDate instead to avoid potentially large number of rows?
  cdm[[name]] <- cdm[[name]] %>%
    dplyr::inner_join(cdm[[name]],
               by = "subject_id") %>%
    dplyr::rename("cohort_start_date" = "cohort_start_date.x",
                  "cohort_end_date" = "cohort_end_date.x",
           "cohort_name" = "cohort_name.x",
           "cohort_definition_id" = "cohort_definition_id.x",
           "cohort_start_date_comparator" = "cohort_start_date.y",
           "cohort_end_date_comparator" = "cohort_end_date.y",
           "cohort_name_comparator" = "cohort_name.y",
           "cohort_definition_id_comparator" = "cohort_definition_id.y") %>%
    dplyr::mutate(comparison = as.character(paste0(as.character(.data$cohort_name),
                                                   as.character(" &&& "),
                                                   as.character(.data$cohort_name_comparator))))

  name_overlap <- paste0(omopgenerics::uniqueTableName(), "_", name, "_overlap")

  cdm[[name_overlap]] <- cdm[[name]] %>%
    dplyr::compute(temporary = FALSE,
                   name = name_overlap) %>%
    omopgenerics::newCohortTable(.softValidation = TRUE)

  if(is.null(timing)){
    cohort_timings <- cdm[[name_overlap]] %>%
      PatientProfiles::summariseCharacteristics(
        strata = list("comparison")) %>% # can we only get number subject and records?
      dplyr::filter(.data$variable_name %in% c("Number subjects",
                                         "Number records")) %>%
      dplyr::mutate(result_type = "cohort_overlap")

    return(cohort_timings)

  }

  cohort_timings <- cdm[[name_overlap]]  %>%
      dplyr::mutate(diff_days = !!CDMConnector::datediff("cohort_start_date",
                                                         "cohort_start_date_comparator",
                                                         interval = "day")) %>%
      dplyr::collect() %>%
      PatientProfiles::summariseResult(group=list("comparison"),
                                       variables = list(diff_days = "diff_days"),
                                       functions = list(diff_days = timing))%>%
      dplyr::mutate(result_type = "cohort_overlap")

    cohort_timings

}
