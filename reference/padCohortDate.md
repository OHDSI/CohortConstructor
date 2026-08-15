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
#> Warning: '/tmp/RtmpET1GQa/id_mds' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  padCohortDate(
    cohortDate = "cohort_end_date",
    indexDate = "cohort_start_date",
    days = 10)
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 54 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2005-05-25        2005-06-04     
#>  2                    1          2 1987-06-29        1987-07-09     
#>  3                    1          6 2014-03-30        2014-04-09     
#>  4                    1          7 2018-04-07        2018-04-17     
#>  5                    1         10 2008-12-27        2009-01-06     
#>  6                    1         13 2010-12-10        2010-12-20     
#>  7                    1         14 1995-02-12        1995-02-22     
#>  8                    1         15 2009-04-01        2009-04-11     
#>  9                    1         17 2008-06-28        2008-07-08     
#> 10                    1         18 2019-08-14        2019-08-15     
#> # ℹ 44 more rows
# }
```
