# Filter cohorts to keep only records for those with a minimum amount of subjects

`requireMinCohortCount()` filters an existing cohort table, keeping only
records from cohorts with a minimum number of individuals

## Usage

``` r
requireMinCohortCount(
  cohort,
  minCohortCount,
  cohortId = NULL,
  updateSettings = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- minCohortCount:

  The minimum count of sbjects for a cohort to be included.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- updateSettings:

  If TRUE, dropped cohorts will also be removed from all cohort table
  attributes (i.e., settings, attrition, counts, and codelist). If
  FALSE, these attributes will be retained but updated to reflect that
  the affected cohorts have been suppressed.

- name:

  Name of the new cohort table created in the cdm object.

## Value

Cohort table

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpTrjJ4I/id_mds' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
requireMinCohortCount(5)
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
