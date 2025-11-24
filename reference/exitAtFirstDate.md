# Set cohort end date to the first of a set of column dates

`exitAtFirstDate()` resets cohort end date based on a set of specified
column dates. The first date that occurs is chosen.

## Usage

``` r
exitAtFirstDate(
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
  exitAtFirstDate(dateColumns = c("next_obs", "future_observation"))
#> Joining with `by = join_by(cohort_definition_id, subject_id,
#> cohort_start_date)`
#> # A tibble: 63 × 6
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date next_obs  
#>                   <int>      <int> <date>            <date>          <date>    
#>  1                    1          1 2003-04-12        2003-07-09      2003-07-09
#>  2                    1          2 2001-02-11        2001-07-19      2001-07-19
#>  3                    1          4 1957-10-04        1958-02-23      1958-02-23
#>  4                    1          5 2007-03-21        2007-05-30      2007-05-30
#>  5                    1          6 1969-11-24        1969-11-25      1969-11-25
#>  6                    1          7 2006-07-17        2007-08-21      2007-08-21
#>  7                    1         11 2015-03-28        2015-10-27      NA        
#>  8                    1         12 1994-05-22        1994-07-22      1994-07-22
#>  9                    1         13 1994-09-14        1994-09-14      1994-09-14
#> 10                    1         14 2000-05-17        2000-09-26      2000-09-26
#> # ℹ 53 more rows
#> # ℹ 1 more variable: future_observation <date>
# }
```
