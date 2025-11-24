# Utility function to change the name of a cohort.

Utility function to change the name of a cohort.

## Usage

``` r
renameCohort(cohort, newCohortName, cohortId = NULL)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- newCohortName:

  Character vector with same

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

## Value

A cohort_table object.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

settings(cdm$cohort1)
#> # A tibble: 1 × 2
#>   cohort_definition_id cohort_name
#>                  <int> <chr>      
#> 1                    1 cohort_1   

cdm$cohort1 <- cdm$cohort1 |>
  renameCohort(newCohortName = "new_name")

settings(cdm$cohort1)
#> # A tibble: 1 × 2
#>   cohort_definition_id cohort_name
#>                  <int> <chr>      
#> 1                    1 new_name   
# }
```
