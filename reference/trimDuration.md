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
#> # A tibble: 59 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1996-01-07        1996-05-25     
#>  2                    1          2 2007-02-15        2007-07-06     
#>  3                    1          3 1981-07-06        1998-11-21     
#>  4                    1          4 2008-08-08        2013-02-01     
#>  5                    1          7 1996-10-22        1997-01-07     
#>  6                    1          8 1981-06-23        1998-01-26     
#>  7                    1          9 1996-09-04        1998-08-06     
#>  8                    1         11 2008-01-26        2010-03-29     
#>  9                    1         12 2011-03-26        2011-08-11     
#> 10                    1         13 2000-09-28        2006-03-13     
#> # ℹ 49 more rows
# }
```
