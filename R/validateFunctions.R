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
  if (!"cohort_table" %in% class(cohort) ||
    !all(
      c(
        "cohort_definition_id",
        "subject_id",
        "cohort_start_date",
        "cohort_end_date"
      ) %in%
        colnames(cohort)
    )) {
    cli::cli_abort("cohort must be a `cohort_table`")
  }
  if (dropExtraColumns) {
    extraColumns <- colnames(cohort)
    extraColumns <- extraColumns[!extraColumns %in% c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    )]
    if (length(extraColumns) > 0) {
      cli::cli_inform(
        c("!" = "extra columns will be dropped from cohort table:
        {paste0(extraColumns, collapse = ', ')}.")
      )
      cohort <- cohort |>
        dplyr::select(
          "cohort_definition_id",
          "subject_id",
          "cohort_start_date",
          "cohort_end_date"
        )
    }
  }
  return(invisible(cohort))
}

validateCohortColumn <- function(columns, cohort, class = NULL) {
  for (column in columns) {
    assertCharacter(column)
    if (!column %in% colnames(cohort)) {
      cli::cli_abort("{column} must be a column in the cohort table.")
    }
    if (!is.null(class)) {
      if (all(!cohort |>
        dplyr::pull(!!column) |>
        class() |>
        unique() %in% class)) {
        cli::cli_abort("{column} must be a column of class {class} in the cohort table.")
      }
    }
  }
  return(invisible(columns))
}

validateCohortId <- function(cohortId, set) {
  # NULL
  if (is.null(cohortId)) {
    cohortId <- set$cohort_definition_id

  # Character
  } else if (is.character(cohortId)) {
    cohortIdIn <- cohortId
    cohortId <- set |> dplyr::filter(.data$cohort_name %in% .env$cohortId) |> dplyr::pull("cohort_definition_id")
    indNot <- !cohortIdIn %in% set$cohort_name
    if (sum(indNot) > 0) {
      if (sum(indNot) == length(cohortIdIn)) {
        cli::cli_abort("No valid cohortId supplied.")
      } else {
        cli::cli_warn(
          "{paste0(cohortIdIn[indNot], collapse = ', ')} {?is/are} not in the cohort table and won't be used."
        )
      }
    }

  # Numeric
  } else if (is.numeric(cohortId)) {
    assertNumeric(cohortId, null = TRUE, integerish = TRUE)
    indNot <- !cohortId %in% set$cohort_definition_id
    if (sum(indNot) > 0) {
      if (sum(indNot) == length(cohortId)) {
        cli::cli_abort("No valid cohort ids supplied.")
      } else {
        cli::cli_warn(
          "{paste0(cohortId[indNot], collapse = ', ')} {?is/are} not in the cohort table and won't be used."
        )
        cohortId <- cohortId[!indNot]
      }
    }

  # Anything else
  } else {
    cli::cli_abort("{.strong cohortId} must be 1) a numeric vector indicating which `cohort_deifnition_id`, 2) a character vector indicating which `cohort_name`, or 3) NULL to use all cohorts in the table.")
  }
  return(cohortId)
}

validateDateRange <- function(dateRange) {
  if (!"Date" %in% class(dateRange) && !all(is.na(dateRange))) {
    cli::cli_abort("dateRange is not a date")
  }
  if (length(dateRange) != 2) {
    cli::cli_abort("dateRange must be length two")
  }
  if (!any(is.na(dateRange))) {
    if (dateRange[1] > dateRange[2]) {
      cli::cli_abort("First date in dateRange cannot be after the second")
    }
  }
}

validateName <- function(name) {
  em <- c(
    "x" = "{name} it is not a valid value for name.",
    "i" = "It must be:",
    "*" = "lowercase character vector of length 1",
    "*" = "NA or NULL values are not allowed"
  )
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    cli::cli_abort(em)
  }
  if (tolower(name) != name) {
    cli::cli_abort(em)
  }
  return(invisible(name))
}

validateConceptSet <- function(conceptSet) {
  # while is.list does not work for tibbles:
  if (!"list" %in% class(conceptSet)) {
    cli::cli_abort(
      "{substitute(conceptSet)} must be a list; it can not contain NA; it has to be named; elements must have class: numeric."
    )
  }
  omopgenerics::newCodelist(conceptSet)
}

validateGap <- function(gap) {
  assertNumeric(gap, integerish = TRUE, min = 0)
}

validateDemographicRequirements <- function(ageRange,
                                            sex,
                                            minPriorObservation,
                                            minFutureObservation,
                                            null = FALSE) {
  # ageRange:
  if (!is.list(ageRange) &&
    !is.null(ageRange)) {
    ageRange <- list(ageRange)
  }
  assertList(ageRange, class = "numeric", null = null)
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
  assertCharacter(sex, null = null)
  if (!all(sex %in% c("Male", "Female", "Both")) && !is.null(sex)) {
    cli::cli_abort("`sex` must be from: 'Male', 'Female', and 'Both'.")
  }

  # minPriorObservation:
  if (!is.null(minPriorObservation)) {
    if (all(is.infinite(minPriorObservation))) {
      cli::cli_abort("`minPriorObservation` cannot be infinite.")
    }
  }
  assertNumeric(
    minPriorObservation,
    integerish = TRUE,
    min = 0,
    null = TRUE
  )
  # minFutureObservation:
  if (!is.null(minFutureObservation)) {
    if (all(is.infinite(minFutureObservation))) {
      cli::cli_abort("`minFutureObservation` cannot be infinite.")
    }
  }
  assertNumeric(
    minFutureObservation,
    integerish = TRUE,
    min = 0,
    null = TRUE
  )

  return(ageRange)
}

validateStrata <- function(strata, cohort) {
  assertList(strata, class = "character")
  colsCohort <- colnames(cohort)
  colsStrata <- unique(unlist(strata))
  misisngCols <- colsStrata[!colsStrata %in% colsCohort]
  if (length(misisngCols) > 0) {
    cli::cli_abort("{misisngCols} column{?s} {?is/are} not present in cohort table")
  }
  return(strata)
}

validateValueAsNumber <- function(valueAsNumber) {
  assertList(valueAsNumber,
    named = TRUE,
    class = "numeric",
    null = TRUE
  )
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
  assertNumeric(n,
    integerish = TRUE,
    min = 0,
    length = 1
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

  intersections
}
