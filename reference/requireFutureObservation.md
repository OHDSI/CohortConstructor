# Restrict cohort on future observation

`requireFutureObservation()` filters cohort records, keeping only
records where individuals satisfy the specified future observation
criteria.

## Usage

``` r
requireFutureObservation(
  cohort,
  minFutureObservation,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- minFutureObservation:

  A minimum number of continuous future observation days in the
  database.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- indexDate:

  Variable in cohort that contains the date to compute the demographics
  characteristics on which to restrict on.

- atFirst:

  If FALSE the requirement will be applied to all records, if TRUE, it
  will only be required for the first entry of each subject.

- name:

  Name of the new cohort table created in the cdm object.

## Value

The cohort table with only records for individuals satisfying the future
observation requirement

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requireFutureObservation(indexDate = "cohort_start_date",
                           minFutureObservation = 30)
#> # A tibble: 52 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1994-04-13        1994-05-08     
#>  2                    1          2 1993-04-23        1996-01-19     
#>  3                    1          3 1996-03-19        1996-09-17     
#>  4                    1          5 2014-07-07        2014-08-23     
#>  5                    1          6 1992-02-18        2001-10-18     
#>  6                    1         10 2000-07-16        2002-07-27     
#>  7                    1         13 1998-02-17        2002-11-23     
#>  8                    1         16 1992-01-22        1997-11-26     
#>  9                    1         17 2011-11-12        2013-10-22     
#> 10                    1         19 2005-01-11        2005-12-31     
#> # ℹ 42 more rows
# }
```
