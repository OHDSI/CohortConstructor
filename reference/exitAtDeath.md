# Set cohort end date to death date

This functions changes cohort end date to subject's death date. In the
case were this generates overlapping records in the cohort, those
overlapping entries will be merged.

## Usage

``` r
exitAtDeath(
  cohort,
  cohortId = NULL,
  requireDeath = FALSE,
  name = tableName(cohort),
  .softValidation = FALSE
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- requireDeath:

  If TRUE, subjects without a death record will be dropped, while if
  FALSE their end date will be left as is.

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
library(PatientProfiles)
library(CohortConstructor)
cdm <- mockPatientProfiles()
cdm$cohort1 |> exitAtDeath()
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> Warning: There was 1 warning in `dplyr::summarise()`.
#> ℹ In argument: `days = min(.data$start, na.rm = TRUE)`.
#> Caused by warning in `min()`:
#> ! no non-missing arguments to min; returning Inf
#> # A tibble: 10 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    2          4 1927-06-24        1931-06-05     
#>  2                    2          9 1935-10-09        1940-11-14     
#>  3                    1          3 1945-10-25        1950-09-15     
#>  4                    2          7 1955-07-26        1960-12-27     
#>  5                    3          2 1960-06-07        1988-06-08     
#>  6                    3          6 1967-05-26        1968-11-12     
#>  7                    3          8 1968-05-06        1982-09-07     
#>  8                    1         10 1982-04-24        1982-05-14     
#>  9                    3          5 1993-01-05        1994-07-11     
#> 10                    1          1 1995-02-22        2000-01-06     
# }
```
