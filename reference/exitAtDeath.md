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
#>  1                    1          3 1910-08-31        1911-06-16     
#>  2                    1          4 1930-05-26        1931-02-15     
#>  3                    3          9 1934-05-20        1938-11-15     
#>  4                    1          2 1951-12-01        1952-12-20     
#>  5                    2          7 1957-01-30        1970-09-01     
#>  6                    3         10 1962-03-21        1967-02-25     
#>  7                    2          8 1968-04-04        1970-05-13     
#>  8                    1          1 1976-06-14        1986-08-16     
#>  9                    2          5 1994-04-17        1996-06-25     
#> 10                    2          6 2006-04-02        2006-08-12     
# }
```
