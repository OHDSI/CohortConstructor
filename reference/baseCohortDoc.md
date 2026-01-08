# Helper for consistent documentation of `conceptCohort` and `measurementCohort`.

Helper for consistent documentation of `conceptCohort` and
`measurementCohort`.

## Arguments

- useRecordsBeforeObservation:

  If FALSE, only records that fall entirely (start and end) within an
  observation period are used. If TRUE, records that start before an
  observation period are included by shifting their start date to the
  beginning of the individual's subsequent observation period, and
  records that start or end outside an observation period are trimmed so
  that cohort records fall entirely within the corresponding observation
  period.

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
