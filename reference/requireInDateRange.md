# Require that an index date is within a date range

`requireInDateRange()` filters cohort records, keeping only those for
which the index date is within the specified date range.

## Usage

``` r
requireInDateRange(
  cohort,
  dateRange,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- dateRange:

  A date vector with the minimum and maximum dates between which the
  index date must have been observed.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- indexDate:

  Name of the column in the cohort that contains the date of interest.

- atFirst:

  If FALSE the requirement will be applied to all records, if TRUE, it
  will only be required for the first entry of each subject.

- name:

  Name of the new cohort table created in the cdm object.

## Value

The cohort table with any cohort entries outside of the date range
dropped

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  requireInDateRange(indexDate = "cohort_start_date",
                     dateRange = as.Date(c("2010-01-01", "2019-01-01")))
#> # A tibble: 20 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2013-02-11        2013-02-25     
#>  2                    1          7 2013-10-02        2019-06-26     
#>  3                    1         11 2014-03-22        2014-12-13     
#>  4                    1         25 2016-02-12        2016-04-07     
#>  5                    1         29 2014-10-06        2015-04-23     
#>  6                    1         30 2016-04-25        2017-08-19     
#>  7                    1         34 2018-02-22        2019-04-02     
#>  8                    1         39 2018-11-29        2019-01-31     
#>  9                    1         40 2010-08-10        2010-10-19     
#> 10                    1         42 2010-04-02        2011-09-30     
#> 11                    1         43 2017-01-02        2017-01-04     
#> 12                    1         51 2018-09-30        2018-12-26     
#> 13                    1         52 2017-10-15        2017-11-04     
#> 14                    1         63 2014-07-14        2015-11-24     
#> 15                    1         69 2014-02-28        2014-03-13     
#> 16                    1         72 2012-01-29        2014-07-07     
#> 17                    1         77 2016-05-06        2016-06-19     
#> 18                    1         80 2016-09-09        2017-07-03     
#> 19                    1         81 2012-10-17        2014-01-20     
#> 20                    1         86 2011-11-04        2012-08-03     
# }
```
