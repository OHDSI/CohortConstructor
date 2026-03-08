# Restrict cohort on prior observation

`requirePriorObservation()` filters cohort records, keeping only records
where individuals satisfy the specified prior observation criteria.

## Usage

``` r
requirePriorObservation(
  cohort,
  minPriorObservation,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- minPriorObservation:

  A minimum number of continuous prior observation days in the database.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- indexDate:

  Variable in cohort that contains the date to compute the demographics
  characteristics on which to restrict on.

- atFirst:

  If FALSE the requirement will be applied to all records, if TRUE, it
  will only be required for the first entry of each subject.

- name:

  Name of the new cohort table created in the cdm object.

## Value

The cohort table with only records for individuals satisfying the prior
observation requirement

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requirePriorObservation(indexDate = "cohort_start_date",
                          minPriorObservation = 365)
#> # A tibble: 42 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1975-07-06        1995-11-10     
#>  2                    1          2 1988-06-16        1989-04-28     
#>  3                    1          5 1987-05-04        1998-02-25     
#>  4                    1          6 1991-08-06        1991-11-30     
#>  5                    1          7 2004-07-28        2006-12-04     
#>  6                    1         10 1984-09-04        1985-09-11     
#>  7                    1         11 1983-05-16        1983-05-29     
#>  8                    1         12 1995-05-21        1996-07-09     
#>  9                    1         13 1998-06-20        2007-11-16     
#> 10                    1         14 1978-07-31        1985-02-16     
#> # ℹ 32 more rows
# }
```
