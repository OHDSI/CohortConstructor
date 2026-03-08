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
#> # A tibble: 58 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2003-04-30        2010-10-11     
#>  2                    1          4 2006-11-05        2006-11-17     
#>  3                    1          6 1997-05-07        1998-08-19     
#>  4                    1          7 1996-11-20        1998-03-01     
#>  5                    1          8 2019-03-24        2019-06-09     
#>  6                    1          9 1993-05-29        2003-05-19     
#>  7                    1         11 2018-03-25        2018-03-27     
#>  8                    1         12 1994-11-11        1997-09-10     
#>  9                    1         15 2016-11-26        2019-08-12     
#> 10                    1         16 2005-07-07        2009-01-30     
#> # ℹ 48 more rows
# }
```
