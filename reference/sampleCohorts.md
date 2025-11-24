# Sample a cohort table for a given number of individuals.

`sampleCohorts()` samples an existing cohort table for a given number of
people. All records of these individuals are preserved.

## Usage

``` r
sampleCohorts(cohort, n, cohortId = NULL, name = tableName(cohort))
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- n:

  Number of people to be sampled for each included cohort.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- name:

  Name of the new cohort table created in the cdm object.

## Value

Cohort table with the specified cohorts sampled.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm$cohort2 |> sampleCohorts(cohortId = 1, n = 10)
#> # A tibble: 116 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1         69 1990-03-22        1990-07-07     
#>  2                    1         34 1986-08-03        2000-03-08     
#>  3                    1         70 1992-05-29        1995-01-23     
#>  4                    1         48 1988-08-03        1989-02-20     
#>  5                    1         46 1969-11-06        1985-06-19     
#>  6                    1         46 1986-07-02        1986-10-08     
#>  7                    1         27 2009-09-14        2013-07-24     
#>  8                    1         60 2018-05-08        2018-05-14     
#>  9                    1         60 2018-05-15        2018-08-24     
#> 10                    1         60 2018-09-07        2018-10-24     
#> # ℹ 106 more rows
# }
```
