# Restrict cohort to specific entry

`requireIsEntry()` filters cohort records, keeping only a range a
specified cohort entries per person.

## Usage

``` r
requireIsEntry(cohort, entryRange, cohortId = NULL, name = tableName(cohort))
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- entryRange:

  Range for entries to include.

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
#> Warning: '/tmp/RtmpkyjIPy/id_mds' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort1 <- requireIsEntry(cdm$cohort1, c(1, Inf))
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
# }
```
