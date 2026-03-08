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
#> Warning: There was 1 warning in `dplyr::summarise()`.
#> ℹ In argument: `days = min(.data$start, na.rm = TRUE)`.
#> Caused by warning in `min()`:
#> ! no non-missing arguments to min; returning Inf
#> # A tibble: 10 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    2          5 1922-05-02        1936-09-01     
#>  2                    3          7 1931-03-26        1937-10-31     
#>  3                    2          9 1949-11-26        1972-02-22     
#>  4                    1          8 1968-05-10        1991-05-06     
#>  5                    3         10 1969-03-23        1994-01-31     
#>  6                    2          6 1973-11-21        1973-12-03     
#>  7                    2          2 1974-08-15        1986-07-03     
#>  8                    2          3 1993-10-01        1998-07-13     
#>  9                    2          1 1995-04-12        1996-02-17     
#> 10                    1          4 2005-02-12        2013-09-14     
# }
```
