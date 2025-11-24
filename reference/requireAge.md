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
#> # A tibble: 41 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2005-03-17        2009-08-31     
#>  2                    1          2 2016-11-24        2017-09-20     
#>  3                    1          3 1999-08-26        1999-11-16     
#>  4                    1          7 1997-01-09        1997-04-30     
#>  5                    1          8 2012-12-06        2013-03-29     
#>  6                    1         11 1994-04-03        1999-10-04     
#>  7                    1         12 2018-10-11        2018-11-21     
#>  8                    1         13 1988-07-17        1992-10-08     
#>  9                    1         14 2005-10-24        2014-02-17     
#> 10                    1         16 2008-09-14        2009-01-24     
#> # ℹ 31 more rows
# }
```
