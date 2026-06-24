# Restrict cohort on prior observation

`requirePriorObservation()` filters cohort records, keeping only records
where individuals satisfy the specified prior observation criteria.

## Usage

``` r
requirePriorObservation(
  cohort,
  minPriorObservation,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- minPriorObservation:

  A minimum number of continuous prior observation days in the database.

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

The cohort table with only records for individuals satisfying the prior
observation requirement

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpCzEXGX/id_mds' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requirePriorObservation(indexDate = "cohort_start_date",
                          minPriorObservation = 365)
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 38 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2005-05-25        2006-08-20     
#>  2                    1          2 1987-06-29        1990-01-03     
#>  3                    1         10 2008-12-27        2010-04-02     
#>  4                    1         14 1995-02-12        2002-11-12     
#>  5                    1         17 2008-06-28        2008-12-28     
#>  6                    1         20 2013-05-08        2014-01-08     
#>  7                    1         21 2018-10-05        2018-12-14     
#>  8                    1         22 1997-08-22        1999-06-21     
#>  9                    1         23 2008-07-26        2009-07-16     
#> 10                    1         24 2004-04-08        2004-08-05     
#> # ℹ 28 more rows
# }
```
