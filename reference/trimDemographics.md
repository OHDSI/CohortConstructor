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
#> # A tibble: 30 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1          1 2008-06-01        2010-06-17     
#>  2                    1          2 2017-02-13        2018-03-16     
#>  3                    1          3 2010-03-22        2010-09-03     
#>  4                    1          5 2010-10-02        2010-10-19     
#>  5                    1         11 2008-05-27        2009-06-30     
#>  6                    1         12 2008-03-02        2008-03-19     
#>  7                    1         15 1985-05-20        1986-06-09     
#>  8                    1         20 2009-11-19        2010-09-14     
#>  9                    1         28 2010-12-02        2011-02-24     
#> 10                    1         30 2004-06-04        2004-10-12     
#> # ℹ 20 more rows
# }
```
