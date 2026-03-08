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
#> # A tibble: 58 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 1968-09-22        1968-10-02     
#>  2                    1         29 1970-04-03        1970-04-13     
#>  3                    1         12 1972-08-13        1972-08-23     
#>  4                    1         26 1973-06-25        1973-07-05     
#>  5                    1         30 1974-04-21        1974-05-01     
#>  6                    1         65 1974-10-25        1974-11-04     
#>  7                    1         47 1979-09-01        1979-09-11     
#>  8                    1         36 1985-11-03        1985-11-13     
#>  9                    1         71 1988-11-05        1988-11-15     
#> 10                    1          4 1990-03-22        1990-04-01     
#> # ℹ 48 more rows
# }
```
