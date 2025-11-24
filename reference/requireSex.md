# Restrict cohort on sex

`requireSex()` filters cohort records, keeping only records where
individuals satisfy the specified sex criteria.

## Usage

``` r
requireSex(
  cohort,
  sex,
  cohortId = NULL,
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- sex:

  Can be "Both", "Male" or "Female".

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- atFirst:

  If FALSE the requirement will be applied to all records, if TRUE, it
  will only be required for the first entry of each subject.

- name:

  Name of the new cohort table created in the cdm object.

## Value

The cohort table with only records for individuals satisfying the sex
requirement

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requireSex(sex = "Female")
#> # A tibble: 31 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          5 1974-12-01        1976-09-29     
#>  2                    1         13 2016-10-30        2018-03-25     
#>  3                    1         14 1973-11-02        1985-05-16     
#>  4                    1         17 2004-03-21        2009-01-31     
#>  5                    1         19 1980-04-07        1980-09-01     
#>  6                    1         23 2003-03-23        2005-01-31     
#>  7                    1         24 2014-07-07        2014-07-12     
#>  8                    1         25 2014-09-12        2016-03-29     
#>  9                    1         26 1998-08-08        1999-02-14     
#> 10                    1         27 1997-07-16        1997-08-26     
#> # ℹ 21 more rows
# }
```
