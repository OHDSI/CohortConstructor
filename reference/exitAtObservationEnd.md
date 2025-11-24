# Set cohort end date to end of observation

`exitAtObservationEnd()` resets cohort end date based on a set of
specified column dates. The last date that occurs is chosen.

This functions changes cohort end date to the end date of the
observation period corresponding to the cohort entry. In the case were
this generates overlapping records in the cohort, overlapping entries
will be merged.

## Usage

``` r
exitAtObservationEnd(
  cohort,
  cohortId = NULL,
  persistAcrossObservationPeriods = FALSE,
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

- persistAcrossObservationPeriods:

  If FALSE, limits the cohort to one entry per person, ending at the
  current observation period. If TRUE, subsequent observation periods
  will create new cohort entries (starting from the start of that
  observation period and ending at the end of that observation period).

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
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |> exitAtObservationEnd()
#> # A tibble: 61 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1         42 1970-09-15        1970-12-29     
#>  2                    1         80 1977-12-07        1983-05-23     
#>  3                    1         50 1979-03-16        1985-03-23     
#>  4                    1         51 1980-02-11        1988-05-01     
#>  5                    1         28 1981-03-25        1986-06-15     
#>  6                    1         70 1983-06-05        1995-12-27     
#>  7                    1         79 1987-05-15        2006-07-06     
#>  8                    1         25 1988-08-10        2015-10-29     
#>  9                    1          1 1988-11-14        1989-06-21     
#> 10                    1         61 1991-06-04        2013-10-01     
#> # ℹ 51 more rows
# }
```
