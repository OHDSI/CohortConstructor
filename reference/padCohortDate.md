# Set cohort start or cohort end

Set cohort start or cohort end

## Usage

``` r
padCohortDate(
  cohort,
  days,
  cohortDate = "cohort_start_date",
  indexDate = "cohort_start_date",
  collapse = TRUE,
  requireFullContribution = FALSE,
  cohortId = NULL,
  name = tableName(cohort),
  .softValidation = FALSE
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- days:

  Integer with the number of days to add or name of a column (that must
  be numeric) to add.

- cohortDate:

  'cohort_start_date' or 'cohort_end_date'.

- indexDate:

  Variable in cohort that contains the index date to add.

- collapse:

  Whether to collapse the overlapping records (TRUE) or drop the records
  that have an ongoing prior record.

- requireFullContribution:

  Whether to require individuals to contribute all required days. If
  TRUE, those individuals for which adding days would take them out of
  observation will be dropped. If FALSE, days will only be added up to
  the day when the individual leaves observation.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

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

Cohort table

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  padCohortDate(
    cohortDate = "cohort_end_date",
    indexDate = "cohort_start_date",
    days = 10)
#> # A tibble: 60 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1         82 1964-11-20        1964-11-30     
#>  2                    1         73 1968-06-14        1968-06-24     
#>  3                    1         28 1970-05-10        1970-05-20     
#>  4                    1         41 1970-11-18        1970-11-28     
#>  5                    1         63 1972-03-12        1972-03-22     
#>  6                    1          8 1980-05-11        1980-05-21     
#>  7                    1         36 1985-01-01        1985-01-11     
#>  8                    1         45 1985-06-17        1985-06-27     
#>  9                    1          2 1990-05-07        1990-05-17     
#> 10                    1         14 1992-03-29        1992-04-08     
#> # ℹ 50 more rows
# }
```
