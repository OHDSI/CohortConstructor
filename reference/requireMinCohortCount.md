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
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
requireMinCohortCount(5)
#> # A tibble: 63 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2016-08-11        2016-08-21     
#>  2                    1          2 2010-08-06        2010-08-24     
#>  3                    1          3 1997-04-17        1998-07-22     
#>  4                    1          4 2000-04-09        2003-06-04     
#>  5                    1          5 2003-04-19        2004-03-28     
#>  6                    1          6 2014-12-15        2017-03-11     
#>  7                    1          7 2013-10-09        2015-06-13     
#>  8                    1          8 2012-11-26        2016-01-09     
#>  9                    1          9 2005-06-01        2006-10-26     
#> 10                    1         10 2013-03-29        2013-04-21     
#> # ℹ 53 more rows
# }
```
