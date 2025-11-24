# Restrict cohort to first entry

`requireIsFirstEntry()` filters cohort records, keeping only the first
cohort entry per person.

## Usage

``` r
requireIsFirstEntry(cohort, cohortId = NULL, name = tableName(cohort))
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
#> ℹ Reading GiBleed tables.
cdm$cohort1 <- requireIsFirstEntry(cdm$cohort1)
# }
```
