# Require cohort subjects are present in another clinical table

`requireTableIntersect()` filters a cohort table based on a requirement
that an individual is seen (or not seen) to have a record (or no
records) in a clinical table in some time window around an index date.

## Usage

``` r
requireTableIntersect(
  cohort,
  tableName,
  window,
  intersections = c(1, Inf),
  cohortId = NULL,
  indexDate = "cohort_start_date",
  targetStartDate = startDateColumn(tableName),
  targetEndDate = endDateColumn(tableName),
  inObservation = TRUE,
  censorDate = NULL,
  atFirst = FALSE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- tableName:

  Name of the table to check for intersect.

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

- inObservation:

  If TRUE only records inside an observation period will be considered

- censorDate:

  Whether to censor overlap events at a specific date or a column date
  of the cohort.

- atFirst:

  If FALSE the requirement will be applied to all records, if TRUE, it
  will only be required for the first entry of each subject.

- name:

  Name of the new cohort table created in the cdm object.

## Value

Cohort table

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  requireTableIntersect(tableName = "drug_exposure",
                            indexDate = "cohort_start_date",
                            window = c(-Inf, 0))
#> # A tibble: 58 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          3 2011-10-21        2012-09-30     
#>  2                    1          5 2013-03-24        2013-07-14     
#>  3                    1          7 2006-05-24        2010-05-15     
#>  4                    1         10 2019-12-06        2019-12-07     
#>  5                    1         11 2008-11-13        2010-09-09     
#>  6                    1         13 1996-12-15        1997-08-11     
#>  7                    1         15 2004-08-28        2004-11-08     
#>  8                    1         17 1996-10-28        2000-04-23     
#>  9                    1         18 1983-03-03        1988-12-25     
#> 10                    1         19 2013-10-20        2014-09-20     
#> # ℹ 48 more rows
# }
```
