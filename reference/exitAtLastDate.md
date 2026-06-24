# Set cohort end date to the last of a set of column dates

`exitAtLastDate()` resets cohort end date based on a set of specified
column dates. The last date that occurs is chosen.

## Usage

``` r
exitAtLastDate(
  cohort,
  dateColumns,
  cohortId = NULL,
  returnReason = FALSE,
  keepDateColumns = TRUE,
  name = tableName(cohort),
  .softValidation = FALSE
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- dateColumns:

  Character vector indicating date columns in the cohort table to
  consider.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- returnReason:

  If TRUE it will return a column indicating which of the `dateColumns`
  was used.

- keepDateColumns:

  If TRUE the returned cohort will keep columns in `dateColumns`.

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

The cohort table.

## Examples

``` r
# \donttest{
library(CohortConstructor)
library(PatientProfiles)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpCzEXGX/id_diu' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort1 <- cdm$cohort1 |>
  addTableIntersectDate(tableName = "observation", nameStyle = "next_obs", order = "first") |>
  addFutureObservation(futureObservationType = "date", name = "cohort1")

cdm$cohort1 |>
  exitAtLastDate(dateColumns = c("next_obs", "future_observation"))
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 54 × 6
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date next_obs  
#>                   <int>      <int> <date>            <date>          <date>    
#>  1                    1          1 2005-05-25        2014-05-01      2005-12-06
#>  2                    1          2 1987-06-29        1996-11-18      1987-07-09
#>  3                    1          6 2014-03-30        2015-04-17      2014-05-25
#>  4                    1          7 2018-04-07        2018-05-30      2018-04-12
#>  5                    1         10 2008-12-27        2011-11-17      2009-04-27
#>  6                    1         13 2010-12-10        2013-07-28      2011-02-20
#>  7                    1         14 1995-02-12        2005-01-07      1995-02-23
#>  8                    1         15 2009-04-01        2010-09-11      2009-04-07
#>  9                    1         17 2008-06-28        2014-07-28      2009-01-07
#> 10                    1         18 2019-08-14        2019-08-15      2019-08-14
#> # ℹ 44 more rows
#> # ℹ 1 more variable: future_observation <date>
# }
```
