# Create cohorts based on a arbitrary fields in the OMOP CDM

`customCohort()` creates a cohort table from patient records from the
clinical tables in the OMOP CDM using

## Usage

``` r
customCohort(
  cdm,
  table,
  field,
  values,
  name,
  exit = "event_end_date",
  overlap = "merge",
  useRecordsBeforeObservation = FALSE,
  subsetCohort = NULL,
  subsetCohortId = NULL
)
```

## Arguments

- cdm:

  A cdm reference.

- table:

  Name of OMOP tables to search for records of the concepts provided. If
  NULL, each concept will be search at the assigned domain in the
  concept table.

- field:

  Field within which to search for the specified values

- values:

  description

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

- useRecordsBeforeObservation:

  If FALSE, only records that fall entirely (start and end) within an
  observation period are used. If TRUE, records that start before an
  observation period are included by shifting their start date to the
  beginning of the individual's subsequent observation period, and
  records that start or end outside an observation period are trimmed so
  that cohort records fall entirely within the corresponding observation
  period.

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
#> Warning: '/tmp/Rtmpf94ORX/id_mds' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 444074), name = "cohort")
#> Warning: ! `codelist` cast to integers.
#> Warning: The following codelist concept IDs are not present in `cdm$concept`:
#> • a: 444074
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
#> Warning: ! `codelist` cast to integers.
#> Warning: The following codelist concept IDs are not present in `cdm$concept`:
#> • nitrogen: 35604434, 35604439
#> • potassium: 40741270, 42899580, 44081436
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
