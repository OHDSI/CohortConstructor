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
#> Warning: '/tmp/RtmpZ9Jh1V/id_diu' already exists
#> ℹ Reading GiBleed tables.
# add 10 days to each cohort exit
cdm$cohort1 |>
  padCohortEnd(days = 10)
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 54 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2005-05-25        2006-08-30     
#>  2                    1          2 1987-06-29        1990-01-13     
#>  3                    1          6 2014-03-30        2015-03-03     
#>  4                    1          7 2018-04-07        2018-05-05     
#>  5                    1         10 2008-12-27        2010-04-12     
#>  6                    1         13 2010-12-10        2011-07-09     
#>  7                    1         14 1995-02-12        2002-11-22     
#>  8                    1         15 2009-04-01        2009-11-19     
#>  9                    1         17 2008-06-28        2009-01-07     
#> 10                    1         18 2019-08-14        2019-08-15     
#> # ℹ 44 more rows
# }
```
