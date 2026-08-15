# Trim cohort on patient demographics

`trimDemographics()` resets the cohort start and end date based on the
specified demographic criteria is satisfied.

## Usage

``` r
trimDemographics(
  cohort,
  cohortId = NULL,
  ageRange = NULL,
  sex = NULL,
  minPriorObservation = NULL,
  minFutureObservation = NULL,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- ageRange:

  A list of vectors specifying minimum and maximum age.

- sex:

  Can be "Both", "Male" or "Female".

- minPriorObservation:

  A minimum number of continuous prior observation days in the database.

- minFutureObservation:

  A minimum number of continuous future observation days in the
  database.

- name:

  Name of the new cohort table created in the cdm object.

## Value

The cohort table with only records for individuals satisfying the
demographic requirements

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpET1GQa/id_mds' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  trimDemographics(ageRange = list(c(10, 30)))
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> ℹ Building new trimmed cohort
#> Adding demographics information
#> Creating initial cohort
#> Trim age
#> ✔ Cohort trimmed
#> # A tibble: 30 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1          2 1987-06-29        1990-01-03     
#>  2                    1         10 2008-12-27        2010-04-02     
#>  3                    1         13 2010-12-10        2011-06-29     
#>  4                    1         14 1995-02-12        2002-11-12     
#>  5                    1         28 2002-11-16        2002-12-26     
#>  6                    1         29 2015-06-07        2015-07-30     
#>  7                    1         34 1992-12-23        2003-03-03     
#>  8                    1         37 2017-04-30        2017-05-29     
#>  9                    1         38 1989-05-15        1990-06-02     
#> 10                    1         39 2015-07-31        2015-08-11     
#> # ℹ 20 more rows
# }
```
