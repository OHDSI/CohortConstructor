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
#> # A tibble: 31 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1          4 2003-08-15        2009-04-22     
#>  2                    1         11 1994-01-30        1995-06-25     
#>  3                    1         13 2017-09-29        2017-10-01     
#>  4                    1         14 1967-07-07        1973-01-24     
#>  5                    1         19 1989-08-04        1990-05-11     
#>  6                    1         22 2018-07-15        2019-04-10     
#>  7                    1         23 2006-04-17        2008-02-10     
#>  8                    1         26 2002-10-18        2002-10-28     
#>  9                    1         30 1991-07-28        1993-08-19     
#> 10                    1         31 2010-09-11        2011-10-29     
#> # ℹ 21 more rows
# }
```
