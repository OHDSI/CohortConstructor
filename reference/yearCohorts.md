# Generate a new cohort table restricting cohort entries to certain years

`yearCohorts()` splits a cohort into multiple cohorts, one for each
year.

## Usage

``` r
yearCohorts(
  cohort,
  years,
  cohortId = NULL,
  name = tableName(cohort),
  .softValidation = FALSE
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- years:

  Numeric vector of years to use to restrict observation to.

- cohortId:

  Vector identifying which cohorts to include (cohort_definition_id or
  cohort_name). Cohorts not included will be removed from the cohort
  set.

- name:

  Name of the new cohort table created in the cdm object.

- .softValidation:

  Whether to perform a soft validation of consistency. If set to FALSE
  four additional checks will be performed: 1) a check that cohort end
  date is not before cohort start date, 2) a check that there are no
  missing values in required columns, 3) a check that cohort duration is
  all within observation period, and 4) that there are no overlapping
  cohort entries

## Value

A cohort table.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm$cohort1 <- cdm$cohort1 |>
  yearCohorts(years = 2000:2002)

settings(cdm$cohort1)
#> # A tibble: 3 × 5
#>   cohort_definition_id cohort_name   target_cohort_definition_id  year
#>                  <int> <chr>                               <int> <int>
#> 1                    1 cohort_1_2000                           1  2000
#> 2                    2 cohort_1_2001                           1  2001
#> 3                    3 cohort_1_2002                           1  2002
#> # ℹ 1 more variable: target_cohort_name <chr>
# }
```
