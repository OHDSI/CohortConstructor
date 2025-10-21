validateCohortColumn <- function(columns, cohort, class = NULL) {
  for (column in columns) {
    omopgenerics::assertCharacter(column)
    if (!column %in% colnames(cohort)) {
      cli::cli_abort("{column} must be a column in the cohort table.")
    }
    if (!is.null(class)) {
      omopgenerics::validateColumn(column = column, x = cohort, type = class)
    }
  }
  return(invisible(columns))
}

validateDateRange <- function(dateRange) {
  if (!inherits(dateRange, "Date") && !all(is.na(dateRange))) {
    cli::cli_abort("dateRange is not a date")
  }
  if (length(dateRange) != 2) {
    cli::cli_abort("dateRange must be length two")
  }
  if (!anyNA(dateRange)) {
    if (dateRange[1] > dateRange[2]) {
      cli::cli_abort("First date in dateRange cannot be after the second")
    }
  }
  return(invisible(dateRange))
}

validateDaysInCohort <- function(daysInCohort) {
omopgenerics::assertNumeric(daysInCohort, length = 2)
omopgenerics::assertNumeric(daysInCohort[1], min = 1)
omopgenerics::assertNumeric(daysInCohort[2], min = 1)
if(daysInCohort[1] > daysInCohort[2]){
cli::cli_abort("First value for daysInCohort cannot be larger than second")
}
daysInCohort
}

validateDemographicRequirements <- function(ageRange,
                                            sex,
                                            minPriorObservation,
                                            minFutureObservation,
                                            null = FALSE,
                                            length = 1) {
  # ageRange:
  if (!is.list(ageRange) &&
      !is.null(ageRange)) {
    ageRange <- list(ageRange)
  }
  omopgenerics::assertList(ageRange, class = "numeric", null = null, length = length)
  if (!is.null(ageRange)) {
    for (i in seq_along(ageRange)) {
      if (length(ageRange[[i]]) != 2) {
        cli::cli_abort("Each numeric vector in `ageRange` list must be of length 2.")
      }
      if (ageRange[[i]][1] > ageRange[[i]][2]) {
        cli::cli_abort("Upper `ageRange` value must be equal or higher than lower `ageRange` value.")
      }
      if (ageRange[[i]][1] < 0 || ageRange[[i]][2] < 0) {
        cli::cli_abort("Both `ageRange` components must be >= 0.")
      }
    }
  }

  # sex:
  omopgenerics::assertCharacter(sex, null = null, length = length)
  if (!all(sex %in% c("Male", "Female", "Both")) && !is.null(sex)) {
    cli::cli_abort("`sex` must be one of: 'Male', 'Female', and 'Both'.")
  }

  # minPriorObservation:
  if (!is.null(minPriorObservation)) {
    if (all(is.infinite(minPriorObservation))) {
      cli::cli_abort("`minPriorObservation` cannot be infinite.")
    }
  }
  omopgenerics::assertNumeric(
    minPriorObservation,
    integerish = TRUE,
    min = 0,
    null = TRUE,
    length = length
  )
  # minFutureObservation:
  if (!is.null(minFutureObservation)) {
    if (all(is.infinite(minFutureObservation))) {
      cli::cli_abort("`minFutureObservation` cannot be infinite.")
    }
  }
  omopgenerics::assertNumeric(
    minFutureObservation,
    integerish = TRUE,
    min = 0,
    null = TRUE,
    length = length
  )

  return(ageRange)
}

validateStrata <- function(strata, cohort) {
  omopgenerics::assertList(strata, class = "character")
  colsCohort <- colnames(cohort)
  colsStrata <- unique(unlist(strata))
  misisngCols <- colsStrata[!colsStrata %in% colsCohort]
  if (length(misisngCols) > 0) {
    cli::cli_abort("{misisngCols} column{?s} {?is/are} not present in cohort table")
  }
  return(strata)
}

validateValueAsNumber <- function(valueAsNumber) {
  # a named list of lists: out list name indicates cohort name
  # inner list name indicates unit concept id (optional) and value
  # must be a length-2 numeric vector indicating a range

  omopgenerics::assertList(valueAsNumber, null = TRUE, named = TRUE)

  for (innerList in names(valueAsNumber)) {
    # check inner lists are well defined:
    omopgenerics::assertList(
      valueAsNumber[[innerList]], class = c("integer", "numeric"), null = TRUE,
      msg = "`valueAsNumber` must be indicate by a length-2 numeric vecor indicating a range. See examples."
    )
    for (i in seq_along(valueAsNumber[[innerList]])) {
      if (length(valueAsNumber[[innerList]][[i]]) != 2) {
        cli::cli_abort("Each numeric vector in `valueAsNumber` list must be of length 2.")
      }
      if (valueAsNumber[[innerList]][[i]][1] > valueAsNumber[[innerList]][[i]][2]) {
        cli::cli_abort(
          "Upper `valueAsNumber` value must be equal or higher than lower `valueAsNumber` value."
        )
      }
    }
  }
}

validateValueAsConcept <- function(valueAsConcept) {
  omopgenerics::assertList(
    valueAsConcept, null = TRUE, named = TRUE, class = c("integer", "numeric"),
    msg = "`valueAsConcept` must be a named list of concepts ids, names indicating the cohort name, and the numeric vector indicating concepts ids to use as value."
  )
}

validateN <- function(n) {
  omopgenerics::assertNumeric(
    n, integerish = TRUE, min = 0, length = 1, max = 9999999999999
  )
}

validateIntersections <- function(intersections, name = "intersections", targetCohort = NULL, targetCohortId = NULL) {

  if(name == "cohortCombinationCriteria" & is.character(intersections)){
    omopgenerics::assertChoice(intersections, choices = c("any", "all"), length = 1)
      if(intersections == "any"){
        intersections <- c(1, Inf)
      } else if(intersections == "all"){
      intersections <- omopgenerics::settings(targetCohort) |>
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
        dplyr::pull("cohort_definition_id") |>
        length()
      } else {
      cli::cli_abort("Only supported character inputs are 'all' and 'any' but {intersections} supplied")
    }
    }

  if (length(intersections) == 1) {
    intersections <- c(intersections, intersections)
  }
  if (length(intersections) != 2) {
    cli::cli_abort("`{name}` must be of length 1 or 2, but is length {length(intersections)}")
  }
  if (intersections[1] < 0) {
    cli::cli_abort(
      "`{name}` lower limit must be equal or greater than zero but is {intersections[[1]]}"
    )
  }
  if (intersections[1] > intersections[2]) {
    cli::cli_abort("Second value for `{name}` must be equal or greater than the first")
  }
  if (intersections[1] == Inf) {
    cli::cli_abort("First value for `{name}` cannot be Inf")
  }

  return(invisible(intersections))
}


validateTable <- function(table) {
  if (length(table) != 0) {
    notId <- !table %in% domainsData$table
    if (any(notId)) {
      cli::cli_abort("{.strong {table[notId]}} table{?s} do not correspond to any of the supported OMOP tables and will be ignored. Supported tables are: {domainsData$table}")
    }
  }

  return(invisible(table))
}
