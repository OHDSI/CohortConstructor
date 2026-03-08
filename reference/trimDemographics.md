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
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  trimDemographics(ageRange = list(c(10, 30)))
#> ℹ Building new trimmed cohort
#> Adding demographics information
#> Creating initial cohort
#> Trim age
#> ✔ Cohort trimmed
#> # A tibble: 28 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1          1 2014-12-26        2016-03-26     
#>  2                    1          3 1988-01-29        1993-11-15     
#>  3                    1         11 1995-03-25        2005-05-20     
#>  4                    1         12 1998-03-11        1998-04-15     
#>  5                    1         13 2003-05-04        2009-04-08     
#>  6                    1         17 1960-10-17        1968-12-20     
#>  7                    1         21 2016-08-22        2017-01-13     
#>  8                    1         22 1985-06-22        1987-05-26     
#>  9                    1         29 1985-06-30        1990-12-19     
#> 10                    1         38 2003-01-01        2003-11-10     
#> # ℹ 18 more rows
# }
```
