# Argument descriptions repeated > 1:

#' Helper for consistent documentation of `cohort`.
#'
#' @param cohort A cohort table in a cdm reference.
#'
#' @name cohortDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of `cohortId`.
#'
#' @param cohortId Vector identifying which cohorts to include
#' (cohort_definition_id or cohort_name). Cohorts not included will be
#' removed from the cohort set.
#'
#' @name cohortIdSubsetDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of `cohortId`.
#'
#' @param cohortId Vector identifying which cohorts to modify
#' (cohort_definition_id or cohort_name). If NULL, all cohorts will be
#' used; otherwise, only the specified cohorts will be modified, and the
#' rest will remain unchanged.
#'
#' @name cohortIdModifyDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of `name`.
#'
#' @param name Name of the new cohort table created in the cdm object.
#'
#' @name nameDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of `conceptSet`.
#'
#' @param conceptSet A conceptSet, which can either be a codelist
#' or a conceptSetExpression.
#'
#' @name conceptSetDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of `cdm`.
#'
#' @param cdm A cdm reference.
#'
#' @name cdmDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of `gap`.
#'
#' @param gap Number of days between two subsequent cohort entries to be merged
#' in a single cohort record.
#'
#' @name gapDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of `dateColumns` and `returnReason`.
#'
#' @param dateColumns Character vector indicating date columns in the cohort
#' table to consider.
#' @param returnReason If TRUE it will return a column indicating which of the
#' `dateColumns` was used.
#' @param keepDateColumns If TRUE the returned cohort will keep columns in
#' `dateColumns`.
#'
#' @name columnDateDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of `window`.
#'
#' @param window A list of vectors specifying minimum and maximum days from
#' `indexDate` to consider events over.
#'
#' @name windowDoc
#' @keywords internal
NULL


#' Helper for consistent documentation of arguments in `requireIntersect`
#' functions.
#'
#' @param indexDate Name of the column in the cohort that contains the date to
#' compute the intersection.
#' @param intersections A range indicating number of intersections for
#' criteria to be fulfilled. If a single number is passed, the number of
#' intersections must match this.
#' @param targetStartDate Start date of reference in cohort table.
#' @param targetEndDate End date of reference in cohort table. If NULL,
#' incidence of target event in the window will be considered as intersection,
#' otherwise prevalence of that event will be used as intersection (overlap
#' between cohort and event).
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of the cohort.
#' @param targetCohortTable Name of the cohort that we want to check for
#' intersect.
#' @param targetCohortId Vector of cohort definition ids to include.
#' @param tableName Name of the table to check for intersect.
#' @param inObservation If TRUE only records inside an observation period will
#' be considered
#'
#' @name requireIntersectDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of arguments in `atFirst`
#' functions.
#' @param atFirst If FALSE the requirement will be applied to all records, if
#' TRUE, it will only be required for the first entry of each subject.
#' @name atFirstDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of arguments in `requireDemographics`.
#'
#' @param ageRange A list of vectors specifying minimum and maximum age.
#' @param sex Can be "Both", "Male" or "Female".
#' @param minPriorObservation A minimum number of continuous prior observation
#' days in the database.
#' @param minFutureObservation A minimum number of continuous future observation
#' days in the database.
#' @param indexDate Variable in cohort that contains the date to compute the
#' demographics characteristics on which to restrict on.
#' @param requirementInteractions If TRUE, cohorts will be created for
#' all combinations of ageGroup, sex, and daysPriorObservation. If FALSE, only
#' the first value specified for the other factors will be used. Consequently,
#' order of values matters when requirementInteractions is FALSE.
#'
#' @name requireDemographicsDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `keepOriginalCohorts`.
#'
#' @param keepOriginalCohorts If TRUE the original cohorts will be return
#' together with the new ones. If FALSE only the new cohort will be returned.
#'
#' @name keepOriginalCohortsDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `collapse`.
#'
#' @param collapse Whether to collapse the overlapping records (TRUE) or drop
#' the records that have an ongoing prior record.
#'
#' @name collapseDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `days`.
#'
#' @param days Integer with the number of days to add or name of a column (that
#' must be numeric) to add.
#'
#' @name daysDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `requireFullContribution`.
#'
#' @param requireFullContribution Whether to require individuals to contribute all
#' required days. If TRUE, those individuals for which adding days would take
#' them out of observation will be dropped. If FALSE, days will only be added
#' up to the day when the individual leaves observation.
#'
#' @name requireFullContributionDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `.softValidation`.
#'
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) a check that
#' cohort end date is not before cohort start date,  2) a check that there
#' are no missing values in required columns, 3) a check that cohort duration is
#' all within observation period, and 4) that there are no overlapping
#' cohort entries
#'
#' @name softValidationDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `conceptCohort` and `measurementCohort`.
#'
#' @param useRecordsBeforeObservation If FALSE, only records in observation will
#' be used. If TRUE, records before the start of observation period will be
#' considered, with cohort start date set as the start date of the
#' individuals next observation period (as cohort records must be within
#' observation).
#' @param useSourceFields If TRUE, the source concept_id fields will also be
#' used when identifying relevant clinical records. If FALSE, only the standard
#' concept_id fields will be used.
#' @param subsetCohort  A character refering to a cohort table containing
#' individuals for whom cohorts will be generated. Only individuals in this
#' table will appear in the generated cohort.
#' @param subsetCohortId Optional. Specifies cohort IDs from the `subsetCohort`
#' table to include. If none are provided, all cohorts from the `subsetCohort`
#' are included.
#'
#' @name baseCohortDoc
#' @keywords internal
NULL
