# Collapse cohort entries using a certain gap to concatenate records.

`collapseCohorts()` concatenates cohort records, allowing for some
number of days between one finishing and the next starting.

## Usage

``` r
collapseCohorts(
  cohort,
  cohortId = NULL,
  gap = 0,
  name = tableName(cohort),
  .softValidation = FALSE
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- gap:

  Number of days between two subsequent cohort entries to be merged in a
  single cohort record.

- name:

  Name of the new cohort table created in the cdm object.

- .softValidation:

  Whether to perform a soft validation of consistency. If set to FALSE
  four additional checks will be performed: 1) a check that cohort end
  date is not before cohort start date, 2) a check that there are no
  missing values in required columns, 3) a check that cohort duration is
  all within observation period, and 4) that there are no overlapping
  cohort entries

## Value

A cohort table

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
# collapse just cohort 1, with a gap of 7 days
cdm$cohort1 <- cdm$cohort1 |>
  collapseCohorts(cohortId = 1, gap = 7)

# collapse both cohorts with a gap of 1 year, and change table name
cdm$collapsed_cohort <- cdm$cohort1 |>
  collapseCohorts(gap = 365, name = "collapsed_cohort")
# }
```
