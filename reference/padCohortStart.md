# Add days to cohort start

`padCohortStart()` Adds (or subtracts) a certain number of days to the
cohort start date. Note:

- If the days added means that cohort start would be after cohort end
  then the cohort entry will be dropped.

- If subtracting day means that cohort start would be before observation
  period start then the cohort entry will be dropped.

## Usage

``` r
padCohortStart(
  cohort,
  days,
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
# add 10 days to each cohort entry
cdm$cohort1 |>
  padCohortStart(days = 10)
#> # A tibble: 59 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1         82 1967-07-30        1968-07-20     
#>  2                    1         24 1974-12-29        1975-06-12     
#>  3                    1         84 1976-08-01        1996-08-21     
#>  4                    1         13 1977-02-12        1977-02-23     
#>  5                    1         56 1982-12-15        1985-01-18     
#>  6                    1         79 1984-04-04        1984-10-23     
#>  7                    1         11 1984-12-07        1985-01-14     
#>  8                    1          4 1985-04-24        1986-09-12     
#>  9                    1         71 1986-01-30        1989-08-06     
#> 10                    1          7 1986-08-03        1989-06-08     
#> # ℹ 49 more rows
# }
```
