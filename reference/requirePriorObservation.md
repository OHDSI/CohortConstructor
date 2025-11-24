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
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requirePriorObservation(indexDate = "cohort_start_date",
                          minPriorObservation = 365)
#> # A tibble: 35 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          5 2010-03-17        2010-05-13     
#>  2                    1         11 1987-08-15        1994-08-22     
#>  3                    1         16 2013-09-19        2013-09-27     
#>  4                    1         18 2017-04-06        2017-09-18     
#>  5                    1         19 1969-12-17        1972-07-29     
#>  6                    1         21 2015-03-16        2016-12-17     
#>  7                    1         24 1996-09-27        2000-07-08     
#>  8                    1         25 2013-06-19        2013-07-09     
#>  9                    1         28 2012-12-05        2014-03-06     
#> 10                    1         29 2012-11-04        2012-11-23     
#> # ℹ 25 more rows
# }
```
