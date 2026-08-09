# Restrict cohort to last entry per person

`requireIsLastEntry()` filters cohort records, keeping only the last
cohort entry per person.

## Usage

``` r
requireIsLastEntry(cohort, cohortId = NULL, name = tableName(cohort))
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- name:

  Name of the new cohort table created in the cdm object.

## Value

A cohort table in a cdm reference.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpTrjJ4I/id_mds' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort1 <- requireIsLastEntry(cdm$cohort1)
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
# }
```
