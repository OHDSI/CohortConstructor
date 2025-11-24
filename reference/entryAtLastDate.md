# Set cohort start date to the last of a set of column dates

`entryAtLastDate()` resets cohort end date based on a set of specified
column dates. The last date is chosen.

## Usage

``` r
entryAtLastDate(
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
  entryAtLastDate(dateColumns = c("prior_drug", "prior_observation"))
#> Joining with `by = join_by(cohort_definition_id, subject_id, cohort_end_date)`
#> # A tibble: 64 × 6
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date prior_drug
#>                   <int>      <int> <date>            <date>          <date>    
#>  1                    1          1 2005-10-04        2012-06-11      2005-10-04
#>  2                    1          4 2011-11-06        2012-06-05      2011-11-06
#>  3                    1          5 2012-12-02        2014-06-22      2012-12-02
#>  4                    1          6 1987-05-13        1989-09-05      1987-05-13
#>  5                    1          7 1986-12-28        1989-06-13      1986-12-28
#>  6                    1          8 1980-02-25        1986-11-20      1980-02-25
#>  7                    1          9 2010-07-30        2011-05-13      2010-07-30
#>  8                    1         10 2013-09-22        2014-08-03      2013-09-22
#>  9                    1         11 1988-11-02        1993-01-16      1988-11-02
#> 10                    1         12 2005-06-24        2008-03-31      2005-06-24
#> # ℹ 54 more rows
#> # ℹ 1 more variable: prior_observation <date>
# }
```
