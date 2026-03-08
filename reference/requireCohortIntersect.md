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
#> # A tibble: 23 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          3 1988-09-07        1988-12-11     
#>  2                    1          9 2000-05-02        2004-03-29     
#>  3                    1         14 1983-09-06        1984-08-15     
#>  4                    1         17 2015-11-04        2015-11-16     
#>  5                    1         19 1986-12-24        1988-07-08     
#>  6                    1         21 2006-05-15        2008-05-28     
#>  7                    1         23 1990-05-12        1990-06-08     
#>  8                    1         28 1992-03-11        1994-09-05     
#>  9                    1         33 2019-12-17        2019-12-19     
#> 10                    1         34 1999-08-28        2003-07-07     
#> # ℹ 13 more rows
# }
```
