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
#> Warning: '/tmp/RtmpET1GQa/id_boq' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort2 |> sampleCohorts(cohortId = 1, n = 10)
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort2" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 120 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1         26 1989-01-15        1991-02-06     
#>  2                    1         48 1997-08-15        2000-10-06     
#>  3                    1         48 2000-10-07        2001-01-31     
#>  4                    1         48 2001-02-01        2005-11-01     
#>  5                    1         48 2010-01-19        2011-10-18     
#>  6                    1         64 1996-12-05        2001-09-26     
#>  7                    1         29 2015-06-07        2015-08-10     
#>  8                    1         29 2015-09-13        2015-10-25     
#>  9                    1         29 2015-11-29        2016-05-15     
#> 10                    1         85 2011-04-02        2012-11-06     
#> # ℹ 110 more rows
# }
```
