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
#> # A tibble: 57 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1982-01-21        1987-09-03     
#>  2                    1          2 1987-02-22        1989-08-15     
#>  3                    1          4 2013-01-17        2014-08-15     
#>  4                    1          5 2005-12-12        2006-04-08     
#>  5                    1          6 2000-05-04        2000-06-23     
#>  6                    1          8 2014-11-17        2016-09-14     
#>  7                    1          9 2015-02-03        2016-01-05     
#>  8                    1         11 1987-05-16        1990-04-14     
#>  9                    1         12 2013-08-29        2014-05-06     
#> 10                    1         14 2002-02-13        2002-05-29     
#> # ℹ 47 more rows
# }
```
