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
#> Warning: '/tmp/RtmpLZ7KDK/id_boq' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requireDuration(daysInCohort = c(2, Inf))
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 54 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2005-05-25        2006-08-20     
#>  2                    1          2 1987-06-29        1990-01-03     
#>  3                    1          6 2014-03-30        2015-02-21     
#>  4                    1          7 2018-04-07        2018-04-25     
#>  5                    1         10 2008-12-27        2010-04-02     
#>  6                    1         13 2010-12-10        2011-06-29     
#>  7                    1         14 1995-02-12        2002-11-12     
#>  8                    1         15 2009-04-01        2009-11-09     
#>  9                    1         17 2008-06-28        2008-12-28     
#> 10                    1         18 2019-08-14        2019-08-15     
#> # ℹ 44 more rows
# }
```
