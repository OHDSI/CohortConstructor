validateCDM <- function(cdm) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cohort must be part of a cdm reference")
  }
  return(invisible(cdm))
}

validateCdm <- function(cdm) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a cdm_reference object.")
  }
  return(invisible(cdm))
}

validateCohortTable <- function(cohort, dropExtraColumns = FALSE) {
  if(!"cohort_table" %in% class(cohort) ||
     !all(c("cohort_definition_id", "subject_id",
            "cohort_start_date", "cohort_end_date") %in%
          colnames(cohort))){
    cli::cli_abort("cohort must be a `cohort_table`")
  }
  if (dropExtraColumns) {
    extraColumns <- colnames(cohort)
    extraColumns <- extraColumns[!extraColumns %in% c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    )]
    if (length(extraColumns) > 0) {
      cli::cli_inform(c(
        "!" = "extra columns will be dropped from cohort table:
        {paste0(extraColumns, collapse = ', ')}."
      ))
      cohort <- cohort |>
        dplyr::select(
          "cohort_definition_id", "subject_id", "cohort_start_date",
          "cohort_end_date"
        )
    }
  }
  return(invisible(cohort))
}

validateCohortColumn <- function(columns, cohort, class = NULL) {
  for (column in columns) {
    assertCharacter(column)
    if(!column %in% colnames(cohort)){
      cli::cli_abort("{column} must be a column in the cohort table.")
    }
    if (!is.null(class)) {
      if (cohort |> dplyr::pull(!!column) |> class() != class) {
        cli::cli_abort("{column} must be a column of class {class} in the cohort table.")
      }
    }
  }
  return(invisible(columns))
}

validateCohortId <- function(cohortId, ids) {
  assertNumeric(cohortId, null = TRUE, integerish = TRUE)
  if (is.null(cohortId)) {
    cohortId <- ids
  } else {
    indNot <- !cohortId %in% ids
    if (sum(indNot)>0) {
      if (sum(indNot) == length(cohortId)) {
        cli::cli_abort("No valid cohort ids supplied.")
      } else {
        cli::cli_warn("{paste0(cohortId[indNot], collapse = ', ')} {?is/are} not in the cohort table and won't be used.")
        cohortId <- cohortId[!indNot]
      }
    }
  }
  return(cohortId)
}

validateDateRange<-function(dateRange){
  if(!"Date" %in% class(dateRange)){
    cli::cli_abort("dateRange is not a date")
  }
  if(length(dateRange) != 2){
    cli::cli_abort("dateRange must be length two")
  }
  if(dateRange[1]>dateRange[2]){
    cli::cli_abort("First date in dateRange cannot be after second")
  }
  return(invisible(dateRange))
}

validateName <- function(name) {
  em <- c(
    "x" = "{name} it is not a valida value for name.",
    "i" = "It must be:",
    "*" = "lowercase character vector of length 1",
    "*" = "NA or NULL values are not allowed"
  )
  if (!is.character(name) | length(name) != 1 | is.na(name)) {
    cli::cli_abort(em)
  }
  if (tolower(name) != name){
    cli::cli_abort(em)
  }
  return(invisible(name))
}

validateConceptSet <- function(conceptSet) {
  omopgenerics::newCodelist(conceptSet)
}

validateAgeRange <- function(ageRange) {
  err <- "ageRange must be a list of pairs of age ranges, example: list(c(0, 9), c(10, 19))"
  if (is.null(ageRange)) {
    return(invisible(ageRange))
  }
  if (!is.list(ageRange)) {
    ageRange <- list(ageRange)
  }
  assertList(ageRange, class = c("numeric", "integer"))
  l <- lengths(ageRange)
  if (!all(l == 2)) {
    cli::cli_abort(err)
  }
  if (!all(unlist(ageRange) >= 0)) {
    cli::cli_abort(err)
  }
  min <- lapply(ageRange, function(x) {x[1]}) |> unlist()
  max <- lapply(ageRange, function(x) {x[2]}) |> unlist()
  if (!all(min <= max)) {
    cli::cli_abort(err)
  }
  return(invisible(ageRange))
}

validateSex <- function(sex) {
  if (is.null(sex)) return(sex)
  sex <- tolower(sex)
  assertChoice(sex, c("both", "female", "male"))
  return(invisible(sex))
}

validateMinPriorObservation <- function(minPriorObservation) {
  assertNumeric(minPriorObservation, integerish = TRUE, min = 0, null = T)
  minPriorObservation <- minPriorObservation |> sort()
  return(invisible(minPriorObservation))
}

validateMinFutureObservation <- function(minFutureObservation) {
  assertNumeric(minFutureObservation, integerish = TRUE, min = 0, null = T)
  minFutureObservation <- minFutureObservation |> sort()
  return(invisible(minFutureObservation))
}

validateGap <- function(gap) {
  assertNumeric(gap, integerish = TRUE, min = 0)
}

validateDemographicRequirements <- function(ageRange,
                                            sex,
                                            minPriorObservation,
                                            minFutureObservation) {
  # ageRange:
  assertList(ageRange, class = "numeric")
  for (i in seq_along(ageRange)) {
    if (length(ageRange[[i]]) != 2) {
      cli::cli_abort("Each numeric vector in `ageRange` list must be of length 2.")
    }
    if (ageRange[[i]][1] >= ageRange[[i]][2]) {
      cli::cli_abort("Upper `ageRange` value must be equal or higher than lower `ageRange` value.")
    }
    if (ageRange[[i]][1] < 0 | ageRange[[i]][2] < 0) {
      cli::cli_abort("Both `ageRange` components must be >= 0.")
    }
  }

  # sex:
  assertCharacter(sex)
  if (!all(sex %in% c("Male", "Female", "Both"))) {
    cli::cli_abort("`sex` must be from: 'Male', 'Female', and 'Both'.")
  }

  # minPriorObservation:
  assertNumeric(minPriorObservation, integerish = TRUE, min = 0)
  # minFutureObservation:
  assertNumeric(minFutureObservation, integerish = TRUE, min = 0)
}

validateStrata <- function(strata, cohort) {
  assertList(strata, class = "character")
  colsCohort <- colnames(cohort)
  colsStrata <- unique(unlist(strata))
  misisngCols <- colsStrata[!colsStrata %in%colsCohort]
  if (length(misisngCols) > 0) {
    cli::cli_abort(
      "{misisngCols} column{?s} {?is/are} not present in cohort table"
    )
  }
  return(strata)
}
