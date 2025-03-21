validateCohortColumn <- function(columns, cohort, class = NULL) {
  for (column in columns) {
    omopgenerics::assertCharacter(column)
    if (!column %in% colnames(cohort)) {
      cli::cli_abort("{column} must be a column in the cohort table.")
    }
    if (!is.null(class)) {
      if (!any(cohort |>
               dplyr::pull(!!column) |>
               class() |>
               unique() %in% class)) {
        cli::cli_abort("{column} must be a column of class {class} in the cohort table.")
      }
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

  omopgenerics::assertList(valueAsNumber,
                           class = c("integer", "numeric"),
                           null = TRUE
  )

  # if any is named all must be
  if(is.null(names(valueAsNumber)) || any(nchar(names(valueAsNumber)) == 0)){
    omopgenerics::assertList(valueAsNumber,
                             length = 1,
                             null = TRUE,
                             msg = "If any valueAsNumber has no unit specified, only one range should be specified"
    )
  }

  for (i in seq_along(valueAsNumber)) {
    if (length(valueAsNumber[[i]]) != 2) {
      cli::cli_abort("Each numeric vector in `valueAsNumber` list must be of length 2.")
    }
    if (valueAsNumber[[i]][1] > valueAsNumber[[i]][2]) {
      cli::cli_abort(
        "Upper `valueAsNumber` value must be equal or higher than lower `valueAsNumber` value."
      )
    }
  }
}

validateN <- function(n) {
  omopgenerics::assertNumeric(
    n, integerish = TRUE, min = 0, length = 1, max = 9999999999999
  )
}

validateIntersections <- function(intersections) {
  if (length(intersections) == 1) {
    intersections <- c(intersections, intersections)
  }
  if (length(intersections) != 2) {
    cli::cli_abort("intersections must be of length 1 or 2, but is length {length(intersections)}")
  }
  if (intersections[1] < 0) {
    cli::cli_abort(
      "intersections lower limit must be equal or greater than zero but is {intersections[[1]]}"
    )
  }
  if (intersections[1] > intersections[2]) {
    cli::cli_abort("Second value for intersections must be equal or greater than the first")
  }
  if (intersections[1] == Inf) {
    cli::cli_abort("First value for intersections cannot be Inf")
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
