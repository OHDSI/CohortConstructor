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
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  requireDuration(daysInCohort = c(2, Inf))
#> # A tibble: 56 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1985-07-22        1986-08-15     
#>  2                    1          2 2017-05-08        2017-12-24     
#>  3                    1          3 2007-10-12        2008-05-24     
#>  4                    1          5 2016-04-21        2017-03-02     
#>  5                    1          6 2001-05-18        2001-09-12     
#>  6                    1          7 1996-10-06        1998-06-19     
#>  7                    1          8 2008-04-15        2011-09-08     
#>  8                    1          9 2000-01-02        2000-08-31     
#>  9                    1         10 2009-11-09        2010-06-11     
#> 10                    1         11 1991-05-31        1998-11-26     
#> # ℹ 46 more rows
# }
```
