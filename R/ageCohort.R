
#' Create cohorts based on patients birthday
#'
#' @description
#' `ageCohort()` creates a cohort table based on patient bithday. The
#' cohort entry and exit date will be the `nth` bithday of the individual.
#' cohort entry ends.
#'
#' @inheritParams cdmDoc
#' @inheritParams nameDoc
#' @param age Age of entry and exit to the cohort. Multiple values can be
#' supplied.
#' @param ageUnit Unit for the age, it can either be *years* (default), or
#' *days*. Multiple values can be supplied. Its length must be 1 or the same
#' than `age`.
#' @param cohortName
#'
#' @return A cohort table object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortConstructor)
#'
#' cdm <- mockCohortConstructor()
#'
#' cdm$cohort4 <- ageCohort(cdm = cdm, name = "cohort4", age = 2)
#'
#' attrition(cdm$cohort4)
#'
#' # Can also create multiple age cohorts
#'
#' cdm$cohort5 <- ageCohort(
#'   cdm = cdm,
#'   name = "cohort5",
#'   age = c(15, 3),
#'   ageUnit = c("days", "years")
#' )
#'
#' attrition(cdm$cohort5)
#'}
ageCohort <- function(
    cdm,
    name,
    age = 0,
    ageUnit = "years",
    cohortName = "birthday_{age}_{ageUnit}" # supporting glue style with {age} or as many elements than in age
) {
  # input check
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  name <- omopgenerics::validateNameArgument(name = name, cdm = cdm, validation = "warning")
  omopgenerics::assertNumeric(age, integerish = TRUE, min = 0)
  omopgenerics::assertChoice(ageUnit, c("days", "years"))
  # to support months
  # https://github.com/darwin-eu-dev/PatientProfiles/issues/855
  if (length(ageUnit) == 1) {
    ageUnit <- rep(ageUnit, length(age))
  }
  if (length(ageUnit) != length(age)) {
    cli::cli_abort(c(x = "`ageUnit` should have either lenght 1 or the same length than `age`."))
  }

  # empty cohort
  if (length(age) == 0) {
    cli::cli_warn(c("!" = "No `age` provided, returning empty cohort."))
    cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = name)
    return(cdm[[name]])
  }

  # start attrition
  cdm[[name]] <- cdm$person |>
    dplyr::filter(!is.na(.data$year_of_birth)) |>
    dplyr::select("subject_id" = "person_id") |>
    dplyr::distinct() |>
    dplyr::compute(name = name)
  n <- c(
    omopgenerics::numberSubjects(cdm$person),
    omopgenerics::numberSubjects(cdm[[name]])
  )
  attition0 <- dplyr::tibble(
    number_records = .env$n,
    number_subjects = .env$n,
    reason_id = 1:2L,
    reason = c("All individuals in the cdm", "Drop individuals with missing year of birth"),
    excluded_records = c(0, .env$n[2] - .env$n[1]),
    excluded_subjects = .data$excluded_records
  )

  # add date of birth
  if (any(c("days") %in% ageUnit)) {
    cdm[[name]] <- cdm[[name]] |>
      PatientProfiles::addDateOfBirth(
        dateOfBirthName = "date_of_birth",
        name = name
      )
  }

  # create cohorts
  set <- list()
  attrition <- list()
  cohort <- list()
  reasons <- character()
  for (k in seq_along(age)) {
    value <- age[k]
    unit <- ageUnit[k]
    nm <- as.character(glue::glue(cohortName, age = value, ageUnit = unit))
    reasons <- c(
      reasons,
      "In observation {age} {ageUnit} after date of birth" |>
        glue::glue(age = value, ageUnit = unit) |>
        as.character()
    )
    # set
    set[[k]] <- dplyr::tibble(
      cohort_definition_id = .env$k,
      cohort_name = .env$nm,
      age = as.character(.env$value),
      age_unit = .env$unit
    )
    # attrition
    attrition[[k]] <- attition0 |>
      dplyr::mutate(cohort_definition_id = .env$k)
    # cohort
    if (unit == "years") {
      x <- cdm[[name]] |>
        PatientProfiles::addBirthdayQuery(
          birthdayName = "cohort_start_date",
          birthday = value
        )
      q <- c(".env$k", ".data$cohort_start_date") |>
        rlang::parse_exprs() |>
        rlang::set_names(c("cohort_definition_id", "cohort_end_date"))
    } else {
      x <- cdm[[name]]
      q <- c(".env$k", paste0("clock::add_days(.data$date_of_birth, ", value, "L)"), ".data$cohort_start_date") |>
        rlang::parse_exprs() |>
        rlang::set_names(c("cohort_definition_id", "cohort_start_date", "cohort_end_date"))
    }
    cohort[[k]] <- x |>
      dplyr::mutate(!!!q) |>
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date"
      )
  }

  # bind together
  set <- dplyr::bind_rows(set)
  attition <- dplyr::bind_rows(attrition)
  cdm[[name]] <- cohort |>
    purrr::reduce(dplyr::union_all) |>
    dplyr::compute(name = name) |>
    omopgenerics::newCohortTable(
      cohortSetRef = set,
      cohortAttritionRef = attition,
      cohortCodelistRef = NULL,
      .softValidation = TRUE
    ) |>
    PatientProfiles::filterInObservation(indexDate = "cohort_start_date") |>
    dplyr::compute(name = name) |>
    omopgenerics::recordCohortAttrition(
      cohortId = seq_along(age),
      reason = reasons
    )

  return(cdm[[name]])
}
