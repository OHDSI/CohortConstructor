# Split cohorts based on time-windows

Split cohorts based on time-windows

## Usage

``` r
timeWindowCohorts(
  cohort,
  window,
  cohortId = NULL,
  keepOriginalCohorts = TRUE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- window:

  A list specifying the time windows (in days) used to split the cohort.
  Each element must be a numeric vector of length 2:
  `c(start_day, end_day)`, where the values are days since
  `cohort_start_date`. Use `Inf` as the end value to indicate a window
  that extends until the subject's `cohort_end_date`. If the list is
  named, window names will be used to identify the output cohorts

- cohortId:

  Vector identifying which cohorts to include (cohort_definition_id or
  cohort_name). Cohorts not included will be removed from the cohort
  set.

- keepOriginalCohorts:

  If TRUE the original cohorts will be return together with the new
  ones. If FALSE only the new cohort will be returned.

- name:

  Name of the new cohort table created in the cdm object.

## Value

A cohort table

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
# if "cohort1" contained pregnancy episodes, we can generate trimester-specific
# cohorts with this function
cdm$pregnancy_trimesters <- timeWindowCohorts(
  cohort = cdm$cohort1,
  window = list(
    "trimester_1" = c(0, 90),
    "trimester_2" = c(91,180),
    "trimester_3" = c(181, Inf)
  ),
  cohortId = NULL,
  keepOriginalCohorts = FALSE,
  name = "pregnancy_trimesters"
)
# }
```
