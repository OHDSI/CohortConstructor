# Require cohort entries last for a certain number of days

`requireDuration()` filters cohort records, keeping only those which
last for the specified amount of days

## Usage

``` r
requireDuration(
  cohort,
  daysInCohort,
  cohortId = NULL,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- daysInCohort:

  Number of days cohort entries must last. Can be a vector of length two
  if a range, or a vector of length one if a specific number of days.
  Note, cohort entry and exit on the same day counts as one day in the
  cohort. So if, for example, you wish to require individuals are in the
  cohort for at least one night then set daysInCohort to c(2, Inf).
  Meanwhile, if set to c(30, 90) then only cohort entries that are 30
  days or more longer and shorter or equal to 90 days will be kept.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- name:

  Name of the new cohort table created in the cdm object.

## Value

The cohort table with any cohort entries that last less or more than the
required duration dropped

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requireDuration(daysInCohort = c(2, Inf))
#> # A tibble: 57 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1985-02-05        2012-10-06     
#>  2                    1          3 1996-08-11        1997-06-16     
#>  3                    1          4 2012-07-30        2012-10-27     
#>  4                    1          6 2019-03-30        2019-04-22     
#>  5                    1          8 2017-07-24        2017-08-02     
#>  6                    1         10 2006-01-11        2006-04-27     
#>  7                    1         11 2008-08-13        2015-11-19     
#>  8                    1         13 2002-10-28        2010-10-25     
#>  9                    1         14 1998-07-26        2000-02-01     
#> 10                    1         15 2012-02-04        2017-03-12     
#> # ℹ 47 more rows
# }
```
