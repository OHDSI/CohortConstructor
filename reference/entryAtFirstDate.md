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
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 54 × 6
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date prior_drug
#>                   <int>      <int> <date>            <date>          <date>    
#>  1                    1          1 2003-07-30        2006-08-20      2005-05-12
#>  2                    1          2 1984-04-03        1990-01-03      1987-05-27
#>  3                    1          6 2014-01-05        2015-02-21      2014-03-26
#>  4                    1          7 2018-03-04        2018-04-25      2018-04-07
#>  5                    1         10 1999-10-02        2010-04-02      2008-11-30
#>  6                    1         13 2010-01-02        2011-06-29      2010-12-04
#>  7                    1         14 1993-08-25        2002-11-12      1995-02-11
#>  8                    1         15 2009-01-27        2009-11-09      2009-04-01
#>  9                    1         17 2000-11-29        2008-12-28      2008-06-20
#> 10                    1         18 2019-07-30        2019-08-15      2019-08-14
#> # ℹ 44 more rows
#> # ℹ 1 more variable: prior_observation <date>
# }
```
