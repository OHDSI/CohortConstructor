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
#>  1                    1          1 2001-02-13        2004-01-26      2002-09-07
#>  2                    1          2 2009-02-01        2013-04-26      2012-09-13
#>  3                    1          3 2003-12-12        2015-01-23      2006-09-08
#>  4                    1          6 2008-01-02        2011-11-28      2010-12-09
#>  5                    1         11 1990-10-23        1999-01-24      1996-08-18
#>  6                    1         12 1981-05-22        1988-12-21      1988-04-28
#>  7                    1         13 2019-03-13        2019-07-07      2019-06-02
#>  8                    1         14 1999-12-24        2008-09-30      2007-07-14
#>  9                    1         16 2016-08-17        2019-04-24      2019-04-22
#> 10                    1         18 1972-06-06        1990-04-03      1990-02-08
#> # ℹ 48 more rows
#> # ℹ 1 more variable: prior_observation <date>
# }
```
