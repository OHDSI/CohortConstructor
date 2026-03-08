# Sample a cohort table for a given number of individuals.

`sampleCohorts()` samples an existing cohort table for a given number of
people. All records of these individuals are preserved.

## Usage

``` r
sampleCohorts(
  cohort,
  n,
  independent = TRUE,
  cohortId = NULL,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- n:

  Number of people to be sampled.

- independent:

  If TRUE, cohorts will be sampled independently with each cohort
  randomly sampled for n. If FALSE, cohorts will be jointly sampled for
  n across all cohorts.

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
#>  1                    1         44 2015-10-22        2015-10-24     
#>  2                    1          2 2002-01-08        2002-06-10     
#>  3                    1          2 2005-10-31        2008-03-11     
#>  4                    1         34 2000-04-09        2001-04-13     
#>  5                    1         47 2016-12-11        2017-05-15     
#>  6                    1         70 2004-04-24        2005-06-05     
#>  7                    1         70 2005-06-27        2005-08-02     
#>  8                    1         68 1980-09-27        1982-05-14     
#>  9                    1         68 1982-05-15        1986-12-23     
#> 10                    1         68 1986-12-24        1987-04-04     
#> # ℹ 106 more rows
# }
```
