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
#> Warning: '/tmp/RtmpXh9DRm/id_boq' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requireSex(sex = "Female")
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 25 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2005-05-25        2006-08-20     
#>  2                    1          6 2014-03-30        2015-02-21     
#>  3                    1         10 2008-12-27        2010-04-02     
#>  4                    1         15 2009-04-01        2009-11-09     
#>  5                    1         17 2008-06-28        2008-12-28     
#>  6                    1         21 2018-10-05        2018-12-14     
#>  7                    1         24 2004-04-08        2004-08-05     
#>  8                    1         28 2002-11-16        2002-12-26     
#>  9                    1         31 2002-01-29        2006-07-30     
#> 10                    1         32 2000-01-08        2000-08-05     
#> # ℹ 15 more rows
# }
```
