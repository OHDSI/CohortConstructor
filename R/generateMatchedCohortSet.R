#' Generate a new cohort matched cohort from a preexisting target cohort. The
#' new cohort will contain individuals not included in the target cohort with
#' same year of birth (matchYearOfBirth = TRUE) and same sex (matchSex = TRUE).
#'
#' @param cdm A cdm reference object.
#' @param name Name of the new generated cohort set.
#' @param targetCohortName Name of the target cohort to match.
#' @param targetCohortId Cohort definition id to match from the target cohort.
#' If NULL all the cohort definition id present in the target cohort will be
#' matched.
#' @param matchSex Whether to match in sex.
#' @param matchYearOfBirth Whether to match in year of birth.
#' @param ratio Number of allowed matches per individual in the target cohort.
#'
#' @return A cdm reference object that contains the new generated cohort set.
#'
#' @export
#'
#' @examples
#' library(DrugUtilisation)
#' library(CohortConstructor)
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#' cdm$cohort1 %>%
#'   requireCohortIntersectFlag(targetCohortTable = "cohort2",
#'                              targetCohortId = 1,
#'                              indexDate = "cohort_start_date",
#'                              window = c(-Inf, 0))

generateMatchedCohortSet <- function(cdm,
                                     name,
                                     targetCohortName,
                                     targetCohortId = NULL,
                                     matchSex = TRUE,
                                     matchYearOfBirth = TRUE,
                                     ratio = 1){
  # validate initial input
  validateInput(
    cdm = cdm, name = name, targetCohortName = targetCohortName,
    targetCohortId = targetCohortId, matchSex = matchSex,
    matchYearOfBirth = matchYearOfBirth, ratio = ratio
  )

  # table prefix
  #tablePrefix <- randomPrefix()

  # get the number of cohorts
  n <- getNumberOfCohorts(cdm, targetCohortName)

  # get target cohort id
  targetCohortId <- getTargetCohortId(cdm, targetCohortId, targetCohortName)

  # Create the cohort name with cases and controls of the targetCohortId
  cdm <- getNewCohort(cdm, name, targetCohortName, targetCohortId, n)

  # Exclude cases from controls
  cdm <- excludeCases(cdm, name, targetCohortId, n)

  # get matched tables
  matchCols <- getMatchCols(matchSex, matchYearOfBirth)

  if(!is.null(matchCols)){
    # Exclude individuals without any match
    cdm <- excludeNoMatchedIndividuals(cdm, name, matchCols, n)

    # Match as ratio was infinite
    cdm <- infiniteMatching(cdm, name, targetCohortId)

    # Delete controls that are not in observation
    cdm <- checkObservationPeriod(cdm, name, targetCohortId, n)

    # Check ratio
    cdm <- checkRatio(cdm, name, ratio, targetCohortId, n)

    # Check cohort set ref
    cdm <- checkCohortSetRef(cdm, name, targetCohortName, matchSex, matchYearOfBirth, targetCohortId, n)

    # Rename cohort definition ids
    cdm <- renameCohortDefinitionIds(cdm)
  }
  # Return
  return(cdm)
}

#' @noRd
validateInput <- function(cdm,
                          name,
                          targetCohortName,
                          targetCohortId,
                          matchSex,
                          matchYearOfBirth,
                          ratio) {
  errorMessage <- checkmate::makeAssertCollection()
  # Check cdm class
  data_check   <- any("cdm_reference" == class(cdm))
  checkmate::assertTRUE(data_check, add = errorMessage)
  if(!isTRUE(data_check)){
    errorMessage$push(glue::glue("- cdm input must be a cdm object"))
  }
  # Check if targetCohortName is a character
  targetCohortName_format_check <- any(class(targetCohortName) %in% c("character"))
  checkmate::assertTRUE(targetCohortName_format_check, add = errorMessage)
  if(!isTRUE(targetCohortName_format_check)){
    errorMessage$push(glue::glue("- targetCohortName input must be a string"))
  }
  # Check if targetCohortName length
  targetCohortName_length_check <- length(targetCohortName) == 1
  checkmate::assertTRUE(  targetCohortName_length_check, add = errorMessage)
  if(!isTRUE(  targetCohortName_length_check)){
    errorMessage$push(glue::glue("- targetCohortName input must have length equal to 1"))
  }
  # Check if targetCohortName is within the cdm object
  targetCohortName_check <- targetCohortName %in% names(cdm)
  checkmate::assertTRUE(targetCohortName_check, add = errorMessage)
  if(!isTRUE(targetCohortName_check)){
    errorMessage$push(glue::glue("- cdm input has not table named {targetCohortName}"))
  }
  # Check if observation period is within the cdm object
  observation_period_check <- "observation_period" %in% names(cdm)
  checkmate::assertTRUE(observation_period_check , add = errorMessage)
  if(!isTRUE(observation_period_check)){
    errorMessage$push(glue::glue("- cdm input has not table named 'observation_period'"))
  }
  # Check if targetCohortId is a numeric value
  if(!is.null(targetCohortId)){
    targetCohortId_format_check <- any(class(targetCohortId) %in% c("numeric","double","integer"))
    checkmate::assertTRUE(targetCohortId_format_check, add = errorMessage)
    if(!isTRUE(targetCohortId_format_check)){
      errorMessage$push(glue::glue("- targetCohortId input must be numeric"))
    }
  }
  # Check if targetCohortId is in the cohort_definition_id
  if(!is.null(targetCohortId)){
    rows <- cdm[[targetCohortName]] %>% dplyr::filter(.data$cohort_definition_id %in% targetCohortId) %>% dplyr::tally() %>% dplyr::pull()
    targetCohortId_check <- rows != 0
    checkmate::assertTRUE(targetCohortId_check, add = errorMessage)
    if(!isTRUE(targetCohortId_check)){
      errorMessage$push(glue::glue("- {name} table does not containg '{targetCohortId}' as a cohort_definition_id"))
    }
  }
  # Check if ratio is > 0
  ratio_check <- ratio > 0
  checkmate::assertTRUE(ratio_check, add = errorMessage)
  if(!isTRUE(ratio_check)){
    errorMessage$push(glue::glue("- ratio parameter must be > 0 "))
  }

  checkmate::reportAssertions(collection = errorMessage)
  return(invisible(TRUE))
}



randomPrefix <- function(n = 5) {
  paste0(
    "temp_", paste0(sample(letters, 5, TRUE), collapse = ""), "_", collapse = ""
  )
}


getNumberOfCohorts <- function(cdm, targetCohortName){
  # Read number of cohorts
  n <- cdm[[targetCohortName]] %>%
    dplyr::summarise(v = max(.data$cohort_definition_id, na.rm = TRUE)) %>%
    dplyr::pull("v") # number of different cohorts

  if(is.na(n)){# Empty table, number of cohorts is 0
    n <- 0
  }
  return(n)
}


getTargetCohortId <- function(cdm, targetCohortId, targetCohortName){
  if(is.null(targetCohortId)){
    targetCohortId <- CDMConnector::cohortSet(cdm[[targetCohortName]]) %>%
      dplyr::arrange(.data$cohort_definition_id) %>%
      dplyr::pull("cohort_definition_id")
  }

  return(targetCohortId)
}


getNewCohort <- function(cdm, name, targetCohortName, targetCohortId, n){
  if(n == 0){
    cdm[[name]] <- CDMConnector::new_generated_cohort_set(
      cohort_ref = cdm[[targetCohortName]] %>% CDMConnector::compute_query(schema = attr(cdm, "write_schema"),
                                                                           temporary = FALSE,
                                                                           name = name,
                                                                           overwrite = TRUE),
      cohort_attrition_ref = cdm[[targetCohortName]] %>% CDMConnector::cohort_attrition(),
      cohort_set_ref = cdm[[targetCohortName]] %>% CDMConnector::cohort_set(),
      overwrite = TRUE)
  }else{
    # Create controls cohort
    controls <- lapply(targetCohortId+n, function(x) {
      cdm[["person"]] %>%
        dplyr::select("subject_id" = "person_id") %>%
        dplyr::mutate("cohort_definition_id" = .env$x)
    })

    # Create table with controls + cases (all cases existing in the cohort, without considering the targetCohortId)
    all <- Reduce(dplyr::union_all, controls) %>%
      dplyr::mutate("cohort_start_date" = NA,
                    "cohort_end_date"   = NA) %>%
      dplyr::union_all(
        cdm[[targetCohortName]] %>%
          dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
      ) %>%
      CDMConnector::compute_query(schema = attr(cdm, "write_schema"),
                                  temporary = FALSE,
                                  name = name,
                                  overwrite = TRUE)

    cdm[[name]] <- CDMConnector::new_generated_cohort_set(
      cohort_ref = all,
      overwrite = TRUE)

    cohort_set_ref <- cdm[[targetCohortName]] %>%
      CDMConnector::cohort_set() %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) %>%
      dplyr::slice(rep(1:dplyr::n(), times = 2)) %>%
      dplyr::group_by(.data$cohort_definition_id) %>%
      dplyr::mutate(cohort_name = dplyr::if_else(dplyr::row_number() == 2, paste0(.data$cohort_name,"_matched"), .data$cohort_name)) %>%
      dplyr::mutate(cohort_definition_id = dplyr::if_else(dplyr::row_number() == 2, .data$cohort_definition_id+.env$n, .data$cohort_definition_id)) %>%
      dplyr::ungroup()


    cohort_attrition <- CDMConnector::cohort_attrition(cdm[[name]]) %>%
      dplyr::mutate(reason = dplyr::if_else(.data$cohort_definition_id %in% c(.env$n+(1:.env$n)), "Subjects in the database", .data$reason))


    cdm[[name]] <- CDMConnector::new_generated_cohort_set(
      cohort_ref = all,
      cohort_attrition_ref = cohort_attrition,
      cohort_set_ref = cohort_set_ref,
      overwrite = TRUE)
  }
  return(cdm)
}

excludeCases <- function(cdm, name, targetCohortId, n){
  # For each target cohort id
  for(targetCohortId_i in targetCohortId){
    # Controls
    controls <- cdm[[name]] %>%
      dplyr::select("subject_id", "cohort_definition_id") %>%
      dplyr::filter(.data$cohort_definition_id == targetCohortId_i+.env$n) %>%
      dplyr::anti_join(
        # Cases
        cdm[[name]] %>%
          dplyr::select("subject_id","cohort_definition_id") %>%
          dplyr::filter(.data$cohort_definition_id == targetCohortId_i) %>%
          dplyr::mutate(cohort_definition_id = targetCohortId_i + .env$n),
        by = c("subject_id", "cohort_definition_id")
      )

    cdm[[name]] <- cdm[[name]] %>%
      # Delete the controls
      dplyr::filter(.data$cohort_definition_id != targetCohortId_i + .env$n) %>%
      # Add the new controls set
      dplyr::union_all(
        controls %>%
          dplyr::mutate(cohort_start_date = NA,
                        cohort_end_date   = NA)
      ) %>%
      CDMConnector::computeQuery() %>%
      CDMConnector::computeQuery(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)

  }
  # Record attrition
  cdm[[name]] <- cdm[[name]] %>%
    CDMConnector::record_cohort_attrition("Exclude cases",
                                          cohortId = c(targetCohortId+n))%>%
    CDMConnector::computeQuery() %>%
    CDMConnector::computeQuery(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)
  return(cdm)
}


getMatchCols <- function(matchSex, matchYearOfBirth){
  # Obtain matched columns
  matchCols <- c()
  if(matchSex){
    matchCols <- append(matchCols, "gender_concept_id")
  }
  if(matchYearOfBirth){
    matchCols <- append(matchCols, "year_of_birth")
  }
  return(matchCols)
}

excludeNoMatchedIndividuals <- function(cdm, name, matchCols, n){
  cdm[[name]] <- cdm[[name]] %>%
    # Append matchcols
    dplyr::left_join(
      cdm[["person"]] %>%
        dplyr::select("subject_id" = "person_id", dplyr::all_of(matchCols)),
      by = c("subject_id")
    ) %>%
    CDMConnector::computeQuery() %>%
    CDMConnector::computeQuery(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)

  # Create column group id
  cdm[[name]] <- cdm[[name]] %>%
    dplyr::inner_join(
      cdm[[name]] %>%
        dplyr::select(dplyr::all_of(matchCols)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(group_id = dplyr::row_number()),
      by = c(matchCols)
    ) %>%
    dplyr::select(-dplyr::all_of(matchCols)) %>%
    # Create target definition id column
    dplyr::mutate(target_definition_id =
                    dplyr::if_else(
                      .data$cohort_definition_id <= .env$n,
                      .data$cohort_definition_id,
                      .data$cohort_definition_id - .env$n
                    )) %>%
    CDMConnector::compute_query() %>%
    CDMConnector::computeQuery(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)

  # Exclude individuals that do not have any match
  cdm[[name]] <- cdm[[name]] %>%
    dplyr::inner_join(
      cdm[[name]] %>%
        dplyr::mutate(
          "cohort_definition_id" = dplyr::if_else(
            .data$target_definition_id == .data$cohort_definition_id,
            .data$cohort_definition_id + .env$n,
            .data$cohort_definition_id - .env$n
          )
        ) %>%
        dplyr::select("cohort_definition_id", "target_definition_id", "group_id") %>%
        dplyr::distinct(),
      by = c("target_definition_id", "group_id", "cohort_definition_id")
    ) %>%
    CDMConnector::compute_query() %>%
    CDMConnector::compute_query(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE) %>%
    CDMConnector::record_cohort_attrition("Exclude individuals that do not have any match")

  return(cdm)
}


infiniteMatching <- function(cdm, name, targetCohortId){
  # Create pair id to perform a random match
  cdm[[name]] <- cdm[[name]] %>%
    dplyr::mutate(id = dbplyr::sql("random()")) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$group_id) %>%
    dbplyr::window_order(.data$id) %>%
    dplyr::mutate(pair_id = dplyr::row_number()) %>%
    dplyr::select(-"id") %>%
    dplyr::ungroup() %>%
    CDMConnector::computeQuery() %>%
    CDMConnector::computeQuery(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)

  cdm[[name]] <- cdm[[name]] %>%
    dplyr::inner_join(
      # Calculate the maximum number of cases per group
      cdm[[name]] %>%
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) %>%
        dplyr::group_by(.data$cohort_definition_id, .data$group_id) %>%
        dplyr::mutate(max_cases = max(.data$pair_id, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select("group_id", "target_definition_id", "max_cases"),
      by = c("group_id", "target_definition_id")
    ) %>%
    # Calculate the maximum ratio per group
    dplyr::mutate(id = (.data$pair_id-1) %% .data$max_cases + 1) %>%
    dplyr::mutate(pair_id = .data$id) %>%
    dplyr::select(-"max_cases", -"id") %>%
    CDMConnector::compute_query() %>%
    CDMConnector::computeQuery(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)


  # Perform random matches with ratio 1:Inf
  cdm[[name]] <- cdm[[name]] %>%
    dplyr::select(-"cohort_start_date", -"cohort_end_date") %>%
    dplyr::inner_join(
      # Cohort start date and end date of cases
      cdm[[name]] %>%
        dplyr::filter(!is.na(.data$cohort_start_date), !is.na(.data$cohort_end_date)) %>%
        dplyr::select("pair_id", "group_id", "target_definition_id", "cohort_start_date", "cohort_end_date"),
      by = c("pair_id", "group_id", "target_definition_id")
    ) %>%
    dplyr::distinct() %>%
    CDMConnector::compute_query() %>%
    CDMConnector::computeQuery(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)
  return(cdm)
}

checkObservationPeriod <- function(cdm, name, targetCohortId, n){
  cdm[[name]] <- cdm[[name]] %>%
    PatientProfiles::addFutureObservation() %>%
    dplyr::filter(!is.na(.data$future_observation)) %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      .data$cohort_definition_id %in% .env$targetCohortId,
      .data$cohort_end_date,
      as.Date(!!CDMConnector::dateadd("cohort_start_date", "future_observation"))
    )) %>%
    dplyr::select(-"future_observation") %>%
    dplyr::group_by(.data$target_definition_id, .data$group_id, .data$pair_id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    CDMConnector::compute_query() %>%
    CDMConnector::compute_query(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE) %>%
    CDMConnector::record_cohort_attrition("Exclude individuals that are not in observation", cohortId = targetCohortId + n) %>%
    CDMConnector::record_cohort_attrition("Exclude individuals that their only pair is not in observation", cohortId = targetCohortId)
  return(cdm)
}


checkRatio <- function(cdm, name, ratio, targetCohortId, n){
  if(ratio == Inf){
    cdm[[name]] <- cdm[[name]] %>%
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %>%
      CDMConnector::compute_query() %>%
      CDMConnector::compute_query(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)
  }else{
    cdm[[name]] <- cdm[[name]] %>%
      dplyr::group_by(.data$pair_id, .data$group_id, .data$target_definition_id) %>%
      dbplyr::window_order(.data$cohort_definition_id) %>%
      dplyr::filter(dplyr::row_number() <= .env$ratio+1) %>%
      dplyr::ungroup() %>%
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %>%
      CDMConnector::compute_query() %>%
      CDMConnector::compute_query(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE) %>%
      CDMConnector::record_cohort_attrition("Exclude individuals that do not fulfil the ratio", cohortId = targetCohortId+n)
  }


  return(cdm)
}


checkCohortSetRef <- function(cdm, name, targetCohortName, matchSex, matchYearOfBirth, targetCohortId, n){
  cohort_set_ref <- cdm[[name]] %>%
    CDMConnector::cohort_set() %>%
    dplyr::mutate(target_cohort_name  = .env$targetCohortName) %>%
    dplyr::mutate(match_sex           = .env$matchSex) %>%
    dplyr::mutate(match_year_of_birth = .env$matchYearOfBirth) %>%
    dplyr::mutate(match_status        = dplyr::if_else(.data$cohort_definition_id %in% .env$targetCohortId, "target", "matched")) %>%
    dplyr::mutate(target_cohort_id    = dplyr::if_else(.data$cohort_definition_id %in% .env$targetCohortId, .data$cohort_definition_id, .data$cohort_definition_id-n))

  cdm[[name]] <- CDMConnector::new_generated_cohort_set(
    cohort_ref = cdm[[name]],
    cohort_attrition_ref = cdm[[name]] %>% CDMConnector::cohort_attrition(),
    cohort_set_ref = cohort_set_ref,
    overwrite = TRUE)

  return(cdm)
}

renameCohortDefinitionIds <- function(cdm, name){
  new_cohort_set <- cdm[[name]] %>%
    CDMConnector::cohort_set() %>%
    dplyr::mutate(cohort_definition_id_new = target_cohort_id) %>%
    arrange(cohort_definition_id_new) %>%
    dplyr::mutate(cohort_definition_id_new = dplyr::row_number())

  new_cohort_attrition <- cdm[[name]] %>%
    CDMConnector::cohort_attrition() %>%
    inner_join(
      new_cohort_set %>% dplyr::select("cohort_definition_id","cohort_definition_id_new"),
      by = "cohort_definition_id"
    ) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::rename("cohort_definition_id" = "cohort_definition_id_new") %>%
    dplyr::relocate(cohort_definition_id)

  new_cohort_count <- cdm[[name]] %>%
    CDMConnector::cohort_count() %>%
    inner_join(
      new_cohort_set %>% dplyr::select("cohort_definition_id","cohort_definition_id_new"),
      by = "cohort_definition_id"
    ) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::rename("cohort_definition_id" = "cohort_definition_id_new") %>%
    dplyr::relocate(cohort_definition_id)

  new_cohort <- cdm[[name]] %>%
    inner_join(
      new_cohort_set %>% dplyr::select("cohort_definition_id","cohort_definition_id_new"),
      by = "cohort_definition_id",
      copy = TRUE
    ) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::rename("cohort_definition_id" = "cohort_definition_id_new") %>%
    dplyr::relocate(cohort_definition_id) %>%
    CDMConnector::compute_query() %>%
    CDMConnector::compute_query(name = name, temporary = FALSE, schema = attr(cdm, "write_schema"), overwrite = TRUE)

  new_cohort_set <- new_cohort_set %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::rename("cohort_definition_id" = "cohort_definition_id_new") %>%
    dplyr::relocate(cohort_definition_id)

  cdm[[name]] <- CDMConnector::new_generated_cohort_set(
    cohort_ref = new_cohort,
    cohort_attrition_ref =  new_cohort_attrition ,
    cohort_set_ref = new_cohort_set,
    cohort_count_ref = new_cohort_count,
    overwrite = TRUE)
}
