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
#> ℹ Reading GiBleed tables.

cdm$cohort1 |>
  requireDemographics(indexDate = "cohort_start_date",
                      ageRange = list(c(18, 65)),
                      sex = "Female",
                      minPriorObservation = 365)
#> # A tibble: 8 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#> *                <int>      <int> <date>            <date>         
#> 1                    1          5 1995-01-28        1997-01-25     
#> 2                    1          8 2005-09-13        2016-06-01     
#> 3                    1         12 2009-03-30        2015-06-20     
#> 4                    1         16 1976-05-06        1978-02-02     
#> 5                    1         17 2004-10-23        2011-02-26     
#> 6                    1         58 1987-09-20        1988-02-03     
#> 7                    1         60 2001-06-21        2007-02-05     
#> 8                    1         74 1996-10-05        2000-10-20     
# }
```
