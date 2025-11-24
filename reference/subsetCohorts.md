# Generate a cohort table keeping a subset of cohorts.

`subsetCohorts()` filters an existing cohort table, keeping only the
records from cohorts that are specified.

## Usage

``` r
subsetCohorts(cohort, cohortId, name = tableName(cohort))
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- cohortId:

  Vector identifying which cohorts to include (cohort_definition_id or
  cohort_name). Cohorts not included will be removed from the cohort
  set.

- name:

  Name of the new cohort table created in the cdm object.

## Value

Cohort table with only cohorts in cohortId.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  subsetCohorts(cohortId = 1)
#> # A tibble: 60 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1993-05-08        1997-02-04     
#>  2                    1          2 1994-10-03        1995-06-15     
#>  3                    1          4 1964-10-03        1972-10-30     
#>  4                    1          7 1997-08-27        2002-11-16     
#>  5                    1          8 2000-05-11        2002-01-27     
#>  6                    1          9 2014-11-25        2016-01-26     
#>  7                    1         10 1996-11-14        2009-11-02     
#>  8                    1         11 1979-05-27        1982-11-01     
#>  9                    1         13 2018-07-05        2019-07-11     
#> 10                    1         17 2015-08-19        2015-10-27     
#> # ℹ 50 more rows
# }
```
