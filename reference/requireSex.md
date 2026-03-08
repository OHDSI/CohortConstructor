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
#>  1                    1          3 2010-10-18        2011-02-16     
#>  2                    1          6 2018-05-30        2018-05-31     
#>  3                    1          7 2014-06-28        2015-08-04     
#>  4                    1         10 2006-03-05        2006-07-06     
#>  5                    1         12 2013-03-03        2013-03-22     
#>  6                    1         14 2017-12-21        2018-06-10     
#>  7                    1         16 1997-05-21        1998-10-28     
#>  8                    1         18 1982-06-23        1989-02-12     
#>  9                    1         20 2018-01-25        2018-05-22     
#> 10                    1         23 2017-09-12        2018-01-10     
#> # ℹ 21 more rows
# }
```
