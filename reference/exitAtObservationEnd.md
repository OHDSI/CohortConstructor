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
#> Warning: '/tmp/RtmpCzEXGX/id_diu' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort1 |> exitAtObservationEnd()
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 54 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1         60 1983-07-12        1988-05-28     
#>  2                    1         33 1986-12-31        1987-06-11     
#>  3                    1          2 1987-06-29        1996-11-18     
#>  4                    1         26 1989-01-15        1998-04-28     
#>  5                    1         38 1989-05-15        1996-10-17     
#>  6                    1         34 1992-12-23        2008-02-07     
#>  7                    1         14 1995-02-12        2005-01-07     
#>  8                    1         51 1995-06-13        2009-09-13     
#>  9                    1         74 1996-10-14        1998-10-03     
#> 10                    1         64 1996-12-05        2014-04-17     
#> # ℹ 44 more rows
# }
```
