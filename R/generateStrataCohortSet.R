#' Generate a stratified cohort set from an existent cohort.
#'
#' @param cdm A cdm reference.
#' @param name Name of the new generated cohort.
#' @param targetCohortName Name of an existent cohort in the cdm to create the
#' combinations.
#' @param targetCohortId Ids to stratify of the target cohort. If NULL all
#' cohort present in the table will be used.
#' @param strataColumn Columns in targetCohortName that you want to use to
#' stratificatify.
#'
#' @export
#'
#' @return The cdm object with the new generated cohort set
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort2 <- cdm$cohort2 |>
#'   addSex()
#'
#' cdm <- generateStrataCohortSet(
#'   cdm = cdm,
#'   name = "cohort3",
#'   targetCohortName = "cohort2",
#'   strataColumn = "sex"
#' )
#'
#' cdm$cohort3
#'
#' settings(cdm$cohort3)
#'
#' }

generateStrataCohortSet <- function(cdm,
                                    name,
                                    targetCohortName,
                                    targetCohortId = NULL,
                                    strataColumn = character()) {
  # initial checks
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(targetCohortName, len = 1, any.missing = FALSE)
  checkmate::assertTRUE(targetCohortName %in% names(cdm))
  cohort <- cdm[[targetCohortName]]
  checkmate::assertClass(cohort, "cohort_table")
  checkmate::assertIntegerish(targetCohortId, null.ok = TRUE, any.missing = FALSE)
  opt <- settings(cohort) |> dplyr::pull("cohort_definition_id") |> sort()
  if (is.null(targetCohortId)) {
    targetCohortId <- opt
  } else {
    checkmate::assertTRUE(targetCohortId %in% opt)
  }
  if (!is.list(strataColumn)) {
    strataColumn <- list(strataColumn)
  }
  checkmate::assertList(strataColumn, types = "character", any.missing = FALSE, min.len = 1)
  checkmate::assertTRUE(all(unique(unlist(strataColumn)) %in% colnames(cohort)))

  id <- 0
  originalAttrition <- attrition(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
    dplyr::rename("target_cohort_definition_id" = "cohort_definition_id")
  originalSettings <- settings(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
    dplyr::rename("target_cohort_definition_id" = "cohort_definition_id")

  cohort <- cdm[[targetCohort]] |>
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) |>
    dplyr::rename("target_cohort_definition_id" = "cohort_definition_id") |>
    dplyr::compute(temporary = FALSE, name = name, overwrite = TRUE)

  strataColumns <- unique(unlist(strataColumn))
  strataValues <- lapply(strataColumns, function(x) {
    cohort |>
      dplyr::select(dplyr::all_of(x)) |>
      dplyr::distinct() |>
      dplyr::pull() |>
      sort() |>
      as.character()
  })
  names(strataValues) <- strataColumns

  fullCohort <- list()
  fullSettings <- list()
  k <- 1
  for (s in strataColumn) {
    opts <- strataValues[s]
    opts$target_cohort_definition_id = targetCohortId
    cs <- createCohortSet(originalSettings, opts, id)
    fullCohort[[k]] <- cohort |>
      dplyr::inner_join(
        cs |>
          dplyr::select(dplyr::all_of(
            "cohort_definition_id", "target_cohort_definition_id", s
          )),
        by = c("target_cohort_definition_id", s),
        copy = TRUE
      ) |>
      dplyr::select(!dplyr::all_of(c("target_cohort_definition_id", s)))
    fullSettings[[k]] <- cs
    k <- k +1
    id <- id + nrow(cs)
  }

  fullSettings <- fullSettings |>
    dplyr::bind_rows() |>
    dplyr::relocate("cohort_definition_id", "cohort_name")

  fullCohort <- Reduce(dplyr::union_all, fullCohort) |>
    dplyr::relocate(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::compute(temporary = FALSE, name = name, overwrite = TRUE)

  # misisng update attrition

  cdm[[name]] <- newCohortTable(
    table = fullCohort, cohortSetRef = fullSettings,
    cohortAttritionRef = oldAttrition
  )

  return(cdm)
}

createCohortSet <- function(originalSettings, opts, id) {
  extraCols <- colnames(originalSettings)
  extraCols <- extraCols[
    !extraCols %in% c("cohort_definition_id", "cohort_name")
  ]
  x <- expand.grid(opts) |>
    dplyr::as_tibble() |>
    dplyr::mutate("cohort_definition_id" = dplyr::row_number() + .env$id) |>
    dplyr::inner_join(originalSettings, by = "target_cohort_definition_id")
  for (nm in names(opts)) {
    x <- x |> dplyr::mutate(
      "cohort_name" = paste0(.data$cohort_name, "; ", nm, "==", .data[[nm]])
    )
  }
  x |>
    dplyr::select(
      "cohort_definition_id", "cohort_name", dplyr::all_of(extraCols),
      "target_cohort_definition_id", dplyr::all_of(names(opts))
    )
}
