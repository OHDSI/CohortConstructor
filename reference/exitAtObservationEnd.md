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
#> Warning: '/tmp/RtmpTrjJ4I/id_diu' already exists
#> ℹ Reading GiBleed tables.
cdm$cohort1 |> exitAtObservationEnd()
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 54 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2005-05-25        2014-05-01     
#>  2                    1          2 1987-06-29        1996-11-18     
#>  3                    1          6 2014-03-30        2015-04-17     
#>  4                    1          7 2018-04-07        2018-05-30     
#>  5                    1         10 2008-12-27        2011-11-17     
#>  6                    1         13 2010-12-10        2013-07-28     
#>  7                    1         14 1995-02-12        2005-01-07     
#>  8                    1         15 2009-04-01        2010-09-11     
#>  9                    1         17 2008-06-28        2014-07-28     
#> 10                    1         18 2019-08-14        2019-08-15     
#> # ℹ 44 more rows
# }
```
