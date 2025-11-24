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
#> ℹ Reading GiBleed tables.

cdm$cohort1 <- cdm$cohort1 |>
  addTableIntersectDate(tableName = "observation", nameStyle = "next_obs", order = "first") |>
  addFutureObservation(futureObservationType = "date", name = "cohort1")

cdm$cohort1 |>
  exitAtLastDate(dateColumns = c("next_obs", "future_observation"))
#> Joining with `by = join_by(cohort_definition_id, subject_id,
#> cohort_start_date)`
#> # A tibble: 58 × 6
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date next_obs  
#>                   <int>      <int> <date>            <date>          <date>    
#>  1                    1          1 2015-11-01        2016-10-12      2015-11-13
#>  2                    1          2 2014-05-10        2015-02-21      2014-05-10
#>  3                    1          3 2004-05-26        2007-06-19      2004-10-11
#>  4                    1          4 2007-10-10        2017-09-28      2009-04-16
#>  5                    1          5 1979-12-02        1980-02-29      NA        
#>  6                    1          6 2018-01-17        2018-04-07      2018-02-18
#>  7                    1          8 2013-05-08        2017-04-18      2013-11-22
#>  8                    1          9 2012-07-14        2012-12-21      2012-07-27
#>  9                    1         10 2014-04-19        2014-05-09      2014-04-21
#> 10                    1         12 2013-12-13        2016-12-23      2015-05-28
#> # ℹ 48 more rows
#> # ℹ 1 more variable: future_observation <date>
# }
```
