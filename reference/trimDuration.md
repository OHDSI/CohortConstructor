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
#> # A tibble: 61 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          3 1996-09-27        2004-03-02     
#>  2                    1          4 2008-12-18        2010-10-28     
#>  3                    1          5 1977-09-01        1979-01-18     
#>  4                    1          8 1993-12-06        1995-12-23     
#>  5                    1         10 2016-07-30        2017-11-21     
#>  6                    1         11 2017-08-15        2017-08-31     
#>  7                    1         12 1965-09-28        1968-10-29     
#>  8                    1         13 1995-05-20        1995-08-06     
#>  9                    1         14 1992-02-26        2001-10-20     
#> 10                    1         16 2008-12-26        2009-05-08     
#> # ℹ 51 more rows
# }
```
