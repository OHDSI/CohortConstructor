# Generate a combination cohort set between the intersection of different cohorts.

`intersectCohorts()` combines different cohort entries, with those
records that overlap combined and kept. Cohort entries are when an
individual was in *both* of the cohorts.

## Usage

``` r
intersectCohorts(
  cohort,
  cohortId = NULL,
  gap = 0,
  returnNonOverlappingCohorts = FALSE,
  keepOriginalCohorts = FALSE,
  name = tableName(cohort),
  .softValidation = FALSE
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- cohortId:

  Vector identifying which cohorts to include (cohort_definition_id or
  cohort_name). Cohorts not included will be removed from the cohort
  set.

- gap:

  Number of days between two subsequent cohort entries to be merged in a
  single cohort record.

- returnNonOverlappingCohorts:

  Whether the generated cohorts are mutually exclusive or not.

- keepOriginalCohorts:

  If TRUE the original cohorts will be return together with the new
  ones. If FALSE only the new cohort will be returned.

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

cdm$cohort3 <- intersectCohorts(cohort = cdm$cohort2, name = "cohort3")

settings(cdm$cohort3)
#> # A tibble: 1 × 5
#>   cohort_definition_id cohort_name         gap cohort_1 cohort_2
#>                  <int> <chr>             <dbl>    <dbl>    <dbl>
#> 1                    1 cohort_1_cohort_2     0        1        1
# }
```
