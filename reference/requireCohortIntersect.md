# Require cohort subjects are present (or absence) in another cohort

`requireCohortIntersect()` filters a cohort table based on a requirement
that an individual is seen (or not seen) in another cohort in some time
window around an index date.

## Usage

``` r
requireCohortIntersect(
  cohort,
  targetCohortTable,
  window,
  intersections = c(1, Inf),
  cohortId = NULL,
  targetCohortId = NULL,
  cohortCombinationCriteria = "all",
  indexDate = "cohort_start_date",
  targetStartDate = "cohort_start_date",
  targetEndDate = "cohort_end_date",
  censorDate = NULL,
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- targetCohortTable:

  Name of the cohort that we want to check for intersect.

- window:

  A list of vectors specifying minimum and maximum days from `indexDate`
  to consider events over.

- intersections:

  A range indicating number of intersections for criteria to be
  fulfilled. If a single number is passed, the number of intersections
  must match this.

- cohortId:

  Vector identifying which cohorts to modify (cohort_definition_id or
  cohort_name). If NULL, all cohorts will be used; otherwise, only the
  specified cohorts will be modified, and the rest will remain
  unchanged.

- targetCohortId:

  Vector of cohort definition ids to include.

- cohortCombinationCriteria:

  Can be 'all', 'any, or a numeric vector (length 1 or 2) that specifies
  how many of the target cohorts must meet the intersection requirement.

  Examples:

  - 'all': must meet criteria for each of the target cohorts.

  - 'any': must meet criteria for only one of the target cohorts.

  - Single value: e.g., `4`, exactly 4 cohorts must meet the criteria.
    If there were 4 target cohorts, this would be the same as 'all'.

  - Range: e.g., `c(2, Inf)`, must meet criteria at last 2 of the target
    cohorts. Note, `c(1, Inf)` is equivalent to 'any'.

- indexDate:

  Name of the column in the cohort that contains the date to compute the
  intersection.

- targetStartDate:

  Start date of reference in cohort table.

- targetEndDate:

  End date of reference in cohort table. If NULL, incidence of target
  event in the window will be considered as intersection, otherwise
  prevalence of that event will be used as intersection (overlap between
  cohort and event).

- censorDate:

  Whether to censor overlap events at a specific date or a column date
  of the cohort.

- atFirst:

  If FALSE the requirement will be applied to all records, if TRUE, it
  will only be required for the first entry of each subject.

- name:

  Name of the new cohort table created in the cdm object.

## Value

Cohort table with only those entries satisfying the criteria

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$cohort1 |>
  requireCohortIntersect(targetCohortTable = "cohort2",
                         targetCohortId = 1,
                         indexDate = "cohort_start_date",
                         window = c(-Inf, 0))
#> # A tibble: 18 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          1 2004-10-14        2005-01-25     
#>  2                    1          2 1997-08-26        2000-03-08     
#>  3                    1          8 1970-11-10        1970-12-09     
#>  4                    1         16 1978-03-25        1992-12-23     
#>  5                    1         25 1996-03-04        2010-07-09     
#>  6                    1         30 2006-04-18        2007-06-06     
#>  7                    1         33 1987-03-06        1991-01-04     
#>  8                    1         39 2017-03-23        2017-03-26     
#>  9                    1         40 2009-10-03        2012-04-24     
#> 10                    1         43 1985-02-12        1998-09-23     
#> 11                    1         44 2004-12-30        2007-04-15     
#> 12                    1         48 2006-08-29        2010-10-02     
#> 13                    1         54 1986-11-09        1988-01-06     
#> 14                    1         59 1992-06-28        2000-05-31     
#> 15                    1         62 1967-09-25        1971-07-26     
#> 16                    1         65 1988-01-24        1999-05-24     
#> 17                    1         67 2002-03-09        2002-03-13     
#> 18                    1         69 1988-03-26        1989-02-02     
# }
```
