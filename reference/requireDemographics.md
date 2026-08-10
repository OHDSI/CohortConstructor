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
#> Warning: '/tmp/RtmpeRbMYX/id_boq' already exists
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  requireDemographics(indexDate = "cohort_start_date",
                      ageRange = list(c(18, 65)),
                      sex = "Female",
                      minPriorObservation = 365)
#> Warning: The `name` argument was not provided.
#> ℹ The original "cohort1" table will be overwritten.
#> ℹ To avoid this, set `name = '<new_table_name>'` in your function call.
#> # A tibble: 8 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#> *                <int>      <int> <date>            <date>         
#> 1                    1         17 2008-06-28        2008-12-28     
#> 2                    1         21 2018-10-05        2018-12-14     
#> 3                    1         31 2002-01-29        2006-07-30     
#> 4                    1         38 1989-05-15        1990-06-02     
#> 5                    1         51 1995-06-13        1998-10-11     
#> 6                    1         73 2000-06-13        2001-09-01     
#> 7                    1         75 2000-04-15        2000-05-17     
#> 8                    1         85 2011-04-02        2012-11-06     
# }
```
