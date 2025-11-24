# Trim cohort dates to be within a date range

`trimToDateRange()` resets the cohort start and end date based on the
specified date range.

## Usage

``` r
trimToDateRange(
  cohort,
  dateRange,
  cohortId = NULL,
  startDate = "cohort_start_date",
  endDate = "cohort_end_date",
  name = tableName(cohort),
  .softValidation = FALSE
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- dateRange:

  A window of time during which the start and end date must have been
  observed.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- startDate:

  Variable with earliest date.

- endDate:

  Variable with latest date.

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

The cohort table with record timings updated to only be within the date
range. Any records with all time outside of the range will have been
dropped.

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  trimToDateRange(startDate = "cohort_start_date",
                  endDate = "cohort_end_date",
                  dateRange = as.Date(c("2015-01-01",
                                        "2015-12-31")))
#> # A tibble: 6 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>         
#> 1                    1          8 2015-01-01        2015-05-29     
#> 2                    1         36 2015-01-02        2015-04-15     
#> 3                    1         48 2015-01-01        2015-12-31     
#> 4                    1         50 2015-01-01        2015-09-15     
#> 5                    1         77 2015-01-01        2015-12-31     
#> 6                    1         80 2015-01-17        2015-09-01     
# }
```
