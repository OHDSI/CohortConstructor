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
#> # A tibble: 10 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    2          6 1932-11-21        1932-12-03     
#>  2                    1          8 1939-05-11        1962-05-06     
#>  3                    1          4 1950-02-12        1958-09-14     
#>  4                    3         10 1963-03-24        1988-02-01     
#>  5                    2          5 1967-05-02        1981-09-01     
#>  6                    3          7 1974-03-26        1980-10-31     
#>  7                    2          2 1976-08-14        1988-07-02     
#>  8                    2          9 1980-11-25        2009-03-01     
#>  9                    2          1 1984-04-12        1985-02-17     
#> 10                    2          3 2000-10-01        2005-07-13     
# }
```
