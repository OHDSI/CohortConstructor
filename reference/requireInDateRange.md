# Require that an index date is within a date range

`requireInDateRange()` filters cohort records, keeping only those for
which the index date is within the specified date range.

## Usage

``` r
requireInDateRange(
  cohort,
  dateRange,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- dateRange:

  A date vector with the minimum and maximum dates between which the
  index date must have been observed.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- indexDate:

  Name of the column in the cohort that contains the date of interest.

- atFirst:

  If FALSE the requirement will be applied to all records, if TRUE, it
  will only be required for the first entry of each subject.

- name:

  Name of the new cohort table created in the cdm object.

## Value

The cohort table with any cohort entries outside of the date range
dropped

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm$cohort2 <- cdm$cohort1 |>
  requireInDateRange(
    indexDate = "cohort_start_date",
    dateRange = as.Date(c("2010-01-01", "2019-01-01")),
    name = "cohort2"
  )

# modify same input cohort table to start between 2010 until end of data
cdm$cohort1 <- cdm$cohort1 |>
  requireInDateRange(
    indexDate = "cohort_start_date",
    dateRange = as.Date(c("2010-01-01", NA))
  )
# }
```
