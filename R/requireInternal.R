
requireInternal <- function(cohort,
                            cohortId,
                            atFirst,
                            params,
                            reason,
                            name) {
  cdm <- omopgenerics::cdmReference(cohort)

  prefix <- omopgenerics::tmpPrefix()
  on.exit(
    omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix)),
    add = TRUE
  )

  # empty cohortId
  if (length(cohortId) == 0) {
    cli::cli_inform("Returning entry cohort as `cohortId` is not valid.")
    # return entry cohort as cohortId is used to modify not subset
    cohort <- cohort |>
      dplyr::compute(name = name, logPrefix = "CohortConstructor_requireInternal_empty") |>
      omopgenerics::newCohortTable(.softValidation = TRUE)
    return(cohort)
  }

  # if needs filter
  tmpNewCohort <- omopgenerics::uniqueTableName(prefix = prefix)
  tmpUnchanged <- omopgenerics::uniqueTableName(prefix = prefix)
  cdm <- filterCohortInternal(cdm, cohort, cohortId, tmpNewCohort, tmpUnchanged)
  newCohort <- cdm[[tmpNewCohort]]

  # if atFirst -> restrict to first record
  if (atFirst) {
    allNewCohort <- newCohort
    newCohort <- firstRecord(newCohort, prefix)
  }

  # add the column of interest
  newCohort <- addColumn(newCohort, params)

  # filter to rows of interest
  newCohort <- filterColumn(newCohort, params)

  # remove column if needed
  newCohort <- removeColumn(newCohort, params)

  # Keep all entries for subjects whose first record meets the requirement.
  if (atFirst) {
    newCohort <- allNewCohort |>
      dplyr::semi_join(
        newCohort,
        by = c("cohort_definition_id", "subject_id")
      )
  }

  # add all
  if (isTRUE(needsIdFilter(cohort, cohortId))) {
    newCohort <- newCohort |>
      # join non modified cohorts
      dplyr::union_all(cdm[[tmpUnchanged]]) |>
      dplyr::compute(
        name = name,
        logPrefix = "CohortConstructor_requireUnion"
      )
  } else {
    newCohort <- newCohort |>
      dplyr::compute(name = name, logPrefix = "CohortConstructor_name")
  }

  # record attrition
  newCohort <- newCohort |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  # add indexes
  useIndexes <- getOption("CohortConstructor.use_indexes")
  if (!isFALSE(useIndexes)) {
    addIndex(
      cohort = newCohort,
      cols = c("subject_id", "cohort_start_date")
    )
  }

  return(newCohort)
}
firstRecord <- function(cohort, prefix) {
  cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::filter(.data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::compute(
      name = omopgenerics::uniqueTableName(prefix = prefix),
      logPrefix = "CohortConstructor_atFirst"
    )
}
addColumn <- function(cohort, params) {
  if (params$add_type == "duration") {
    q <- "clock::date_count_between(start = .data$cohort_start_date, end = .data$cohort_end_date, precision = \"day\")" |>
      rlang::parse_exprs() |>
      rlang::set_names(nm = params$colname)
    cohort <- cohort |>
      dplyr::mutate(!!!q)
  }
  return(cohort)
}
filterColumn <- function(cohort, params) {
  if (params$filter_type == "range") {
    if (isUnbound(params$range_min)) {
      if (!isUnbound(params$range_max)) {
        cohort <- cohort |>
          dplyr::filter(
            .data[[params$colname]] <= !!params$range_max
          )
      }
    } else {
      if (isUnbound(params$range_max)) {
        cohort <- cohort |>
          dplyr::filter(
            .data[[params$colname]] >= !!params$range_min
          )
      } else {
        cohort <- cohort |>
          dplyr::filter(
            .data[[params$colname]] >= !!params$range_min &
              .data[[params$colname]] <= !!params$range_max
          )
      }
    }
  }
  return(cohort)
}
isUnbound <- function(x) {
  is.na(x) | is.infinite(x)
}
removeColumn <- function(cohort, params) {
  if (isTRUE(params$remove_column)) {
    cohort <- cohort |>
      dplyr::select(!dplyr::all_of(params$colname))
  }
  return(cohort)
}
