# Restrict cohort on age

`requireAge()` filters cohort records, keeping only records where
individuals satisfy the specified age criteria.

## Usage

``` r
requireAge(
  cohort,
  ageRange,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- ageRange:

  A list of vectors specifying minimum and maximum age.

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

The cohort table with only records for individuals satisfying the age
requirement

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requireAge(indexDate = "cohort_start_date",
             ageRange = list(c(18, 65)))
#> # A tibble: 33 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1995-04-27        1997-02-18     
#>  2                    1          4 2013-05-31        2015-04-04     
#>  3                    1          5 2017-06-29        2017-08-08     
#>  4                    1          6 2013-11-19        2014-09-10     
#>  5                    1          8 2010-09-28        2012-06-18     
#>  6                    1         10 1988-08-12        1989-09-06     
#>  7                    1         15 2013-10-22        2013-11-07     
#>  8                    1         16 1983-07-10        1987-10-07     
#>  9                    1         18 2014-07-24        2014-07-31     
#> 10                    1         19 2002-09-01        2005-12-09     
#> # ℹ 23 more rows
# }
```
