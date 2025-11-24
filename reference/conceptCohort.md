# Create cohorts based on a concept set

`conceptCohort()` creates a cohort table from patient records from the
clinical tables in the OMOP CDM.

The following tables are currently supported for creating concept
cohorts:

- condition_occurrence

- device_exposure

- drug_exposure

- measurement

- observation

- procedure_occurrence

- visit_occurrence

Cohort duration is based on record start and end (e.g.
condition_start_date and condition_end_date for records coming from the
condition_occurrence tables). So that the resulting table satisfies the
requirements of an OMOP CDM cohort table:

- Cohort entries will not overlap. Overlapping records will be combined
  based on the overlap argument.

- Cohort entries will not go out of observation. If a record starts
  outside of an observation period it will be silently ignored. If a
  record ends outside of an observation period it will be trimmed so as
  to end at the preceding observation period end date.

## Usage

``` r
conceptCohort(
  cdm,
  conceptSet,
  name,
  exit = "event_end_date",
  overlap = "merge",
  table = NULL,
  useRecordsBeforeObservation = FALSE,
  useSourceFields = FALSE,
  subsetCohort = NULL,
  subsetCohortId = NULL
)
```

## Arguments

- cdm:

  A cdm reference.

- conceptSet:

  A conceptSet, which can either be a codelist or a
  conceptSetExpression.

- name:

  Name of the new cohort table created in the cdm object.

- exit:

  How the cohort end date is defined. Can be either "event_end_date" or
  "event_start_date".

- overlap:

  How to deal with overlapping records. In all cases cohort start will
  be set as the earliest start date. If "merge", cohort end will be the
  latest end date. If "extend", cohort end date will be set by adding
  together the total days from each of the overlapping records.

- table:

  Name of OMOP tables to search for records of the concepts provided. If
  NULL, each concept will be search at the assigned domain in the
  concept table.

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

## Value

A cohort table

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm$cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 444074), name = "cohort")
#> Warning: ! `codelist` casted to integers.
#> ✖ Domain NA (1 concept) excluded because it is not supported.
#> ℹ No cohort entries found, returning empty cohort table.

cdm$cohort |>
  attrition()
#> # A tibble: 1 × 7
#>   cohort_definition_id number_records number_subjects reason_id reason          
#>                  <int>          <int>           <int>     <int> <chr>           
#> 1                    1              0               0         1 Initial qualify…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>

# Create a cohort based on a concept set. The cohort exit is set to the event start date.
# If two records overlap, the cohort end date is set as the sum of the duration of
# all overlapping records. Only individuals included in the existing `cohort` will be considered.

conceptSet <- list(
  "nitrogen" = c(35604434, 35604439),
  "potassium" = c(40741270, 42899580, 44081436)
)

cdm$study_cohort <- conceptCohort(cdm = cdm,
                                  conceptSet = conceptSet,
                                  name = "study_cohort",
                                  exit = "event_start_date",
                                  overlap = "extend",
                                  subsetCohort = "cohort")
#> Warning: ! `codelist` casted to integers.
#> Warning: There are no individuals in the `subsetCohort` and `subsetCohortId` provided.
#> Returning empty cohort.

cdm$study_cohort |>
  attrition()
#> # A tibble: 2 × 7
#>   cohort_definition_id number_records number_subjects reason_id reason          
#>                  <int>          <int>           <int>     <int> <chr>           
#> 1                    1              0               0         1 Qualifying init…
#> 2                    2              0               0         1 Qualifying init…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>
# }
```
