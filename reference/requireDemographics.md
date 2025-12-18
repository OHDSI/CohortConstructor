# Restrict cohort on patient demographics

`requireDemographics()` filters cohort records, keeping only records
where individuals satisfy the specified demographic criteria.

## Usage

``` r
requireDemographics(
  cohort,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  ageRange = list(c(0, 150)),
  sex = c("Both"),
  minPriorObservation = 0,
  minFutureObservation = 0,
  atFirst = FALSE,
  name = tableName(cohort)
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

- indexDate:

  Variable in cohort that contains the date to compute the demographics
  characteristics on which to restrict on.

- ageRange:

  A list of vectors specifying minimum and maximum age.

- sex:

  Can be "Both", "Male" or "Female".

- minPriorObservation:

  A minimum number of continuous prior observation days in the database.

- minFutureObservation:

  A minimum number of continuous future observation days in the
  database.

- atFirst:

  If FALSE the requirement will be applied to all records, if TRUE, it
  will only be required for the first entry of each subject.

- name:

  Name of the new cohort table created in the cdm object.

## Value

The cohort table with only records for individuals satisfying the
demographic requirements

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> Warning: '/tmp/RtmpIxuUBh/id_qvh' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  requireDemographics(indexDate = "cohort_start_date",
                      ageRange = list(c(18, 65)),
                      sex = "Female",
                      minPriorObservation = 365)
#> # A tibble: 11 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>  *                <int>      <int> <date>            <date>         
#>  1                    1          2 2016-10-22        2017-09-09     
#>  2                    1          9 2016-01-11        2017-12-06     
#>  3                    1         23 1998-08-13        1998-11-14     
#>  4                    1         50 2013-03-26        2016-11-30     
#>  5                    1         51 2015-09-24        2015-10-30     
#>  6                    1         54 2004-03-03        2005-12-19     
#>  7                    1         58 1999-03-08        2002-10-02     
#>  8                    1         60 2012-02-11        2014-01-26     
#>  9                    1         65 2019-01-01        2019-08-24     
#> 10                    1         70 1997-11-14        1998-01-15     
#> 11                    1         72 2016-07-23        2017-09-13     
# }
```
