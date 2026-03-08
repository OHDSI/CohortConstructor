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
#> # A tibble: 60 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2013-05-23        2015-04-13     
#>  2                    1          2 2007-06-25        2008-10-06     
#>  3                    1          3 2012-12-13        2012-12-25     
#>  4                    1          7 2018-04-03        2018-06-04     
#>  5                    1          8 2013-09-13        2014-01-20     
#>  6                    1          9 2014-12-21        2015-11-12     
#>  7                    1         10 2005-03-17        2007-11-03     
#>  8                    1         11 2015-08-27        2016-06-28     
#>  9                    1         12 1999-12-20        2001-10-25     
#> 10                    1         13 2011-09-24        2016-07-13     
#> # ℹ 50 more rows
# }
```
