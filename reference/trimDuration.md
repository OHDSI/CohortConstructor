# Trim cohort dates to be within a certain interval of days

`trimDuration()` resets the cohort start and end date, keeping only
those which include the specified amount of days

## Usage

``` r
trimDuration(cohort, daysInCohort, cohortId = NULL, name = tableName(cohort))
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- daysInCohort:

  Number of days cohort relative to current cohort start dates. Cohort
  entries will be trimmed to these dates. Note, cohort entry and exit on
  the same day counts as one day in the cohort.Set lower bound to 1 if
  keeping cohort start to the same as current cohort start.

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
#> Warning: '/tmp/RtmpTrjJ4I/id_boq' already exists
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
