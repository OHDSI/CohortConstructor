# Generate cohort from the union of different cohorts

`unionCohorts()` combines different cohort entries, with those records
that overlap combined and kept. Cohort entries are when an individual
was in *either* of the cohorts.

## Usage

``` r
unionCohorts(
  cohort,
  cohortId = NULL,
  gap = 0,
  cohortName = NULL,
  keepOriginalCohorts = FALSE,
  name = tableName(cohort)
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

- cohortName:

  Name of the returned cohort. If NULL, the cohort name will be created
  by collapsing the individual cohort names, separated by "\_".

- keepOriginalCohorts:

  If TRUE the original cohorts will be return together with the new
  ones. If FALSE only the new cohort will be returned.

- name:

  Name of the new cohort table created in the cdm object.

## Value

A cohort table.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpET1GQa/id_mds' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort2 <- cdm$cohort2 |>
  unionCohorts()
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort2" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.

settings(cdm$cohort2)
#> # A tibble: 1 × 3
#>   cohort_definition_id cohort_name         gap
#>                  <int> <chr>             <dbl>
#> 1                    1 cohort_1_cohort_2     0

# }
```
