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

validateValueAsNumber <- function(valueAsNumber, conceptSetName) {

  if (length(valueAsNumber) == 0) return(NULL)

  if (!is.list(valueAsNumber)) {
    cli::cli_abort("`valueAsNumber` must be a list naming the cohorts to be created and the measurement values of each cohort. See the function example.")
  }

  # if one single list: check elements are OK and replicate for all cohorts
  if (!any(sapply(valueAsNumber, is.list))) {
    valueAsNumberSpecificationCheck(valueAsNumber)
    valueAsNumber <- rep(list(valueAsNumber), length(conceptSetName)) |>
      rlang::set_names(conceptSetName)

  # if list of lists:
  } else {
    # if just one inner list: check specification and names (if unnamed apply to all)
    if (length(valueAsNumber) == 1) {
      valueAsNumberSpecificationCheck(unlist(valueAsNumber, recursive = FALSE))
      # unnamed: apply same to all cohorts
      if (length(names(valueAsNumber)) == 0) {
        valueAsNumber <- rep(valueAsNumber, length(conceptSetName)) |>
          rlang::set_names(conceptSetName)
      } else { # check names, drop if name not maching any cohort name
        notId <- !names(valueAsNumber) %in% conceptSetName
        if (any(notId)) {
          cli::cli_warn("{names(valueAsNumber)[notId]} {?does/do} not correspond to any cohort in `conceptSet` and will be ignored.")
          valueAsNumber <- valueAsNumber[!notId]
        }
      }
    } else { # if > 1 inner list: check names (drop not matching with any cohort or missing names) and then check values
      # check names
      # 1) check missing names
      if (length(names(valueAsNumber)) == 0 | any(names(valueAsNumber) == "")) {
        cli::cli_abort(c("`valueAsNumber` is an unnamed list or some elements are not named.", "Each inner list specifing measurement values should point to a codelist, see function examples."))
      }
      # 2) drop non matching with cohort names
      notId <- !names(valueAsNumber) %in% conceptSetName
      if (any(notId)) {
        cli::cli_warn("{names(valueAsNumber)[notId]} {?does/do} not correspond to any codelist in `conceptSet` and will be ignored.")
        valueAsNumber <- valueAsNumber[!notId]
      }
      # check values
      for (nm in names(valueAsNumber)) {
        valueAsNumberSpecificationCheck(valueAsNumber[[nm]])
      }
    }
  }

  return(valueAsNumber)
}

valueAsNumberSpecificationCheck <- function(valueAsNumber) {
  if (!is.null(outterListName)) outterListName <- glue::glue("[['{outterListName}']]")
  for (unitId in names(valueAsNumber)) {
    vec <- valueAsNumber[[unitId]]
    if (!is.numeric(vec) | length(vec) != 2) {
      cli::cli_abort("Elements in `valueAsNumber` must be a numeric vector of length 2, indicating a numeric range of values.")
    }
    if (vec[1] > vec[2]) {
      cli::cli_abort(
        "There are numeric ranges in `valueAsNumber` where the first number is bigger than the second."
      )
    }
  }
}

validateValueAsConcept <- function(valueAsConcept, conceptSetName) {
  if (length(valueAsConcept) == 0) return(NULL)

  message <- "`valueAsConcept` must be either a numeric vector of concept IDs (specification applied to codelists in `conceptSet`), or a named list of numeric vectors where names point to specific conceptSets. See function examples."

  # If one single vector: check correct and apply to all
  if (is.atomic(valueAsConcept)) {
    if (!rlang::is_integerish(valueAsConcept)) {
      cli::cli_abort(message)
    }
    valueAsConcept <- rep(list(valueAsConcept), length(conceptSetName)) |>
      rlang::set_names(conceptSetName)
  }

  if (!is.list(valueAsConcept)) {
    cli::cli_abort(message)
  }

  # if length 1: if named - check name and values, if unnamed - check values and apply to all
  if (length(valueAsConcept) == 1) {
    # unnamed
    if (length(names(valueAsConcept)) == 0) {
      if (!rlang::is_integerish(unlist(valueAsConcept))) {
        cli::cli_abort(message)
      }
      valueAsConcept <- rep(list(unlist(valueAsConcept)), length(conceptSetName)) |>
        rlang::set_names(conceptSetName)

      # named list
    } else {
      notId <- !names(valueAsConcept) %in% conceptSetName
      if (any(notId)) {
        cli::cli_warn("{names(valueAsConcept)[notId]} {?does/do} not correspond to any cohort in `conceptSet` and will be ignored.")
        valueAsConcept <- valueAsConcept[!notId]
      }
    }

  # if > 1 element: drop missing names, drop unmatched names, check values
  } else {
    # check names
    # 1) check missing names
    if (length(names(valueAsConcept)) == 0 | any(names(valueAsConcept) == "")) {
      cli::cli_abort(c("`valueAsConcept` is an unnamed list or some elements are not named.", message))
    }
    # 2) drop non matching with cohort names
    notId <- !names(valueAsConcept) %in% conceptSetName
    if (any(notId)) {
      cli::cli_warn("{names(valueAsConcept)[notId]} {?does/do} not correspond to any codelist in `conceptSet` and will be ignored.")
      valueAsConcept <- valueAsConcept[!notId]
    }
    # check values
    if (!rlang::is_integerish(unlist(valueAsConcept))) {
      cli::cli_abort(c("Concept IDs must be integers.", message))
    }
  }
  return(valueAsConcept)
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
