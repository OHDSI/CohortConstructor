# Add an index to a cohort table

Adds an index on subject_id and cohort_start_date to a cohort table.
Note, currently only indexes will be added if the table is in a postgres
database.

## Usage

``` r
addCohortTableIndex(cohort)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

## Value

The cohort table

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 <- addCohortTableIndex(cdm$cohort1)
# }
```
