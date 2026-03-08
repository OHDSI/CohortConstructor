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
#> # A tibble: 57 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1982-09-06        1994-02-15     
#>  2                    1          2 2006-07-25        2011-02-19     
#>  3                    1          3 2011-12-27        2012-03-08     
#>  4                    1          5 1979-11-01        1982-05-31     
#>  5                    1          6 2002-02-15        2007-03-17     
#>  6                    1          8 2006-05-24        2006-11-23     
#>  7                    1         10 2001-02-22        2001-04-24     
#>  8                    1         11 2003-08-22        2004-02-03     
#>  9                    1         12 2000-11-06        2000-11-07     
#> 10                    1         13 2005-08-04        2006-04-02     
#> # ℹ 47 more rows
# }
```
