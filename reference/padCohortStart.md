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
#> Warning: '/tmp/RtmpeRbMYX/id_diu' already exists
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
#>  1                    1          1 2005-06-04        2006-08-20     
#>  2                    1          2 1987-07-09        1990-01-03     
#>  3                    1          6 2014-04-09        2015-02-21     
#>  4                    1          7 2018-04-17        2018-04-25     
#>  5                    1         10 2009-01-06        2010-04-02     
#>  6                    1         13 2010-12-20        2011-06-29     
#>  7                    1         14 1995-02-22        2002-11-12     
#>  8                    1         15 2009-04-11        2009-11-09     
#>  9                    1         17 2008-07-08        2008-12-28     
#> 10                    1         20 2013-05-18        2014-01-08     
#> # ℹ 43 more rows
# }
```
