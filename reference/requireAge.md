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
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 38 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          2 1987-06-29        1990-01-03     
#>  2                    1          6 2014-03-30        2015-02-21     
#>  3                    1          7 2018-04-07        2018-04-25     
#>  4                    1         13 2010-12-10        2011-06-29     
#>  5                    1         15 2009-04-01        2009-11-09     
#>  6                    1         17 2008-06-28        2008-12-28     
#>  7                    1         18 2019-08-14        2019-08-15     
#>  8                    1         20 2013-05-08        2014-01-08     
#>  9                    1         21 2018-10-05        2018-12-14     
#> 10                    1         23 2008-07-26        2009-07-16     
#> # ℹ 28 more rows
# }
```
