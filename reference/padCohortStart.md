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
#> Warning: '/tmp/RtmpHvlfV7/id_diu' already exists
#> ℹ Reading GiBleed tables.
# add 10 days to each cohort entry
cdm$cohort1 |>
  padCohortStart(days = 10)
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 53 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1         60 1983-07-22        1983-11-16     
#>  2                    1         33 1987-01-10        1987-03-09     
#>  3                    1          2 1987-07-09        1990-01-03     
#>  4                    1         26 1989-01-25        1991-02-06     
#>  5                    1         38 1989-05-25        1990-06-02     
#>  6                    1         34 1993-01-02        2003-03-03     
#>  7                    1         14 1995-02-22        2002-11-12     
#>  8                    1         51 1995-06-23        1998-10-11     
#>  9                    1         74 1996-10-24        1997-12-25     
#> 10                    1         64 1996-12-15        2001-09-26     
#> # ℹ 43 more rows
# }
```
