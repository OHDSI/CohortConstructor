# Update cohort start date to be the first date from of a set of column dates

`entryAtFirstDate()` resets cohort start date based on a set of
specified column dates. The first date that occurs is chosen.

## Usage

``` r
entryAtFirstDate(
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
  addTableIntersectDate(
    tableName = "drug_exposure",
    nameStyle = "prior_drug",
    order = "last",
    window = c(-Inf, 0)
  ) |>
  addPriorObservation(priorObservationType = "date", name = "cohort1")

cdm$cohort1 |>
  entryAtFirstDate(dateColumns = c("prior_drug", "prior_observation"))
#> Joining with `by = join_by(cohort_definition_id, subject_id, cohort_end_date)`
#> # A tibble: 58 × 6
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date prior_drug
#>                   <int>      <int> <date>            <date>          <date>    
#>  1                    1          1 2009-02-01        2012-06-23      2010-11-19
#>  2                    1          2 2003-12-12        2011-12-21      2010-10-19
#>  3                    1          3 1977-02-08        1981-07-17      1978-03-14
#>  4                    1          6 2019-02-24        2019-05-16      2019-04-26
#>  5                    1         11 1981-05-22        1988-07-17      1986-06-08
#>  6                    1         12 2019-03-13        2019-07-02      2019-06-23
#>  7                    1         13 1999-12-24        2011-03-09      2007-08-16
#>  8                    1         14 2007-03-03        2014-07-21      2013-07-12
#>  9                    1         16 2005-05-12        2016-02-12      2016-02-05
#> 10                    1         18 2013-04-30        2019-10-26      2019-10-06
#> # ℹ 48 more rows
#> # ℹ 1 more variable: prior_observation <date>
# }
```
