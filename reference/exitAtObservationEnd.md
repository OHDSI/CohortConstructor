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
#>  1                    1         43 1959-09-05        1970-12-29     
#>  2                    1         45 1969-10-24        2000-03-29     
#>  3                    1         81 1973-10-13        1983-05-23     
#>  4                    1         52 1980-05-07        1988-05-01     
#>  5                    1         51 1981-06-04        1985-03-23     
#>  6                    1         28 1982-08-21        2003-05-12     
#>  7                    1          2 1987-06-01        1989-06-21     
#>  8                    1         58 1987-09-22        2007-11-01     
#>  9                    1         40 1987-10-11        1998-07-23     
#> 10                    1         34 1988-05-09        1992-09-03     
#> # ℹ 51 more rows
# }
```
