# Generate a cohort table keeping a subset of cohorts.

`subsetCohorts()` filters an existing cohort table, keeping only the
records from cohorts that are specified.

## Usage

``` r
subsetCohorts(cohort, cohortId, name = tableName(cohort), negate = FALSE)
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

- negate:

  If TRUE, the cohorts specified in cohortId will be excluded and the
  remaining cohorts kept.

## Value

Cohort table with only cohorts in cohortId.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpkyjIPy/id_xtv' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  subsetCohorts(cohortId = 1)
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
