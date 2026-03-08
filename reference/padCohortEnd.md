# Add days to cohort end

`padCohortEnd()` Adds (or subtracts) a certain number of days to the
cohort end date. Note:

- If the days added means that cohort end would be after observation
  period end date, then observation period end date will be used for
  cohort exit.

- If the days added means that cohort exit would be after the next
  cohort start then these overlapping cohort entries will be collapsed.

- If days subtracted means that cohort end would be before cohort start
  then the cohort entry will be dropped.

## Usage

``` r
padCohortEnd(
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
# add 10 days to each cohort exit
cdm$cohort1 |>
  padCohortEnd(days = 10)
#> # A tibble: 58 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1         21 1968-01-18        1970-05-28     
#>  2                    1         54 1978-10-30        1980-04-25     
#>  3                    1         58 1982-03-28        1986-09-25     
#>  4                    1          9 1985-05-14        1986-11-03     
#>  5                    1         57 1986-06-17        1986-07-21     
#>  6                    1         40 1987-09-11        1988-12-03     
#>  7                    1          7 1988-09-21        1991-06-07     
#>  8                    1         55 1988-12-15        1994-05-01     
#>  9                    1         72 1989-09-12        1992-01-14     
#> 10                    1          6 1990-12-12        1998-02-01     
#> # ℹ 48 more rows
# }
```
