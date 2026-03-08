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
#>  1                    1          1 2001-08-27        2002-03-01      2002-03-01
#>  2                    1          2 2004-02-28        2004-11-16      2004-11-16
#>  3                    1          4 2014-12-22        2015-08-12      2015-08-12
#>  4                    1          5 1956-09-11        1956-09-29      1956-09-29
#>  5                    1          6 2015-01-04        2015-01-13      2015-01-13
#>  6                    1          7 1969-08-13        1969-09-13      1969-09-13
#>  7                    1         11 2018-03-14        2018-04-04      NA        
#>  8                    1         12 2004-11-10        2005-08-13      2005-08-13
#>  9                    1         13 1993-09-23        1993-09-23      1993-09-23
#> 10                    1         14 1990-06-10        1991-06-13      1991-06-13
#> # ℹ 53 more rows
#> # ℹ 1 more variable: future_observation <date>
# }
```
