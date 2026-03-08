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
#> # A tibble: 55 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          4 1971-07-15        1977-11-13     
#>  2                    1         60 1973-11-25        1974-04-30     
#>  3                    1         45 1975-05-06        1978-05-31     
#>  4                    1         53 1975-10-28        1978-12-03     
#>  5                    1         58 1982-04-20        1995-03-24     
#>  6                    1         75 1982-08-22        1983-11-18     
#>  7                    1         31 1983-06-12        1984-09-07     
#>  8                    1         30 1985-11-26        1988-10-18     
#>  9                    1         24 1986-03-15        1987-04-02     
#> 10                    1         52 1988-05-21        1989-06-23     
#> # ℹ 45 more rows
# }
```
