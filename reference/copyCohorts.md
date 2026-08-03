# Copy a cohort table

`copyCohorts()` copies an existing cohort table to a new location.

## Usage

``` r
copyCohorts(cohort, name, n = 1, cohortId = NULL)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- name:

  Name of the new cohort table created in the cdm object.

- n:

  Number of times to duplicate the selected cohorts.

- cohortId:

  Vector identifying which cohorts to include (cohort_definition_id or
  cohort_name). Cohorts not included will be removed from the cohort
  set.

## Value

A new cohort table containing cohorts from the original cohort table.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpZ9Jh1V/id_mds' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort3 <- copyCohorts(cdm$cohort1, n = 2, cohortId = 1, name = "cohort3")
# }
```
