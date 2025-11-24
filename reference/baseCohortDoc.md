# Helper for consistent documentation of `conceptCohort` and `measurementCohort`.

Helper for consistent documentation of `conceptCohort` and
`measurementCohort`.

## Arguments

- useRecordsBeforeObservation:

  If FALSE, only records in observation will be used. If TRUE, records
  before the start of observation period will be considered, with cohort
  start date set as the start date of the individuals next observation
  period (as cohort records must be within observation).

- useSourceFields:

  If TRUE, the source concept_id fields will also be used when
  identifying relevant clinical records. If FALSE, only the standard
  concept_id fields will be used.

- subsetCohort:

  A character refering to a cohort table containing individuals for whom
  cohorts will be generated. Only individuals in this table will appear
  in the generated cohort.

- subsetCohortId:

  Optional. Specifies cohort IDs from the `subsetCohort` table to
  include. If none are provided, all cohorts from the `subsetCohort` are
  included.
