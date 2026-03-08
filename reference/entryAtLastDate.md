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
#>  1                    1          1 2001-08-04        2010-04-08      2001-08-04
#>  2                    1          4 1998-01-12        2004-03-03      1998-01-12
#>  3                    1          5 2009-06-27        2009-08-11      2009-06-27
#>  4                    1          6 2011-12-08        2012-06-09      2011-12-08
#>  5                    1          7 2000-01-16        2001-03-25      2000-01-16
#>  6                    1          8 1997-11-29        2000-05-25      1997-11-29
#>  7                    1          9 1989-12-09        1991-08-06      1989-12-09
#>  8                    1         10 2004-09-27        2005-03-31      2004-09-27
#>  9                    1         11 2009-11-15        2016-09-15      2009-11-15
#> 10                    1         12 1998-12-10        2003-10-04      1998-12-10
#> # ℹ 54 more rows
#> # ℹ 1 more variable: prior_observation <date>
# }
```
