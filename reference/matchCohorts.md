# Generate a new cohort matched cohort

`matchCohorts()` generate a new cohort matched to individuals in an
existing cohort. Individuals can be matched based on year of birth and
sex. Matching is done at the record level, so if individuals have
multiple cohort entries they can be matched to different individuals for
each of their records.

Two new cohorts will be created when matching. The first is those cohort
entries which were matched ("\_sampled" is added to the original cohort
name for this cohort). The other is the matches found from the database
population ("\_matched" is added to the original cohort name for this
cohort).

## Usage

``` r
matchCohorts(
  cohort,
  cohortId = NULL,
  matchSex = TRUE,
  matchYearOfBirth = TRUE,
  ratio = 1,
  keepOriginalCohorts = FALSE,
  name = tableName(cohort),
  .softValidation = FALSE
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- cohortId:

  Vector identifying which cohorts to include (cohort_definition_id or
  cohort_name). Cohorts not included will be removed from the cohort
  set.

- matchSex:

  Whether to match in sex.

- matchYearOfBirth:

  Whether to match in year of birth.

- ratio:

  Number of allowed matches per individual in the target cohort.

- keepOriginalCohorts:

  If TRUE the original cohorts will be return together with the new
  ones. If FALSE only the new cohort will be returned.

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

A cohort table.

## Examples

``` r
# \donttest{
library(CohortConstructor)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.
cdm$new_matched_cohort <- cdm$cohort2 |>
  matchCohorts(
    name = "new_matched_cohort",
    cohortId = 2,
    matchSex = TRUE,
    matchYearOfBirth = TRUE,
    ratio = 1)
#> Starting matching
#> ℹ Creating copy of target cohort.
#> • 1 cohort to be matched.
#> ℹ Creating controls cohorts.
#> ℹ Excluding cases from controls
#> • Matching by gender_concept_id and year_of_birth
#> • Removing controls that were not in observation at index date
#> • Excluding target records whose pair is not in observation
#> • Adjusting ratio
#> Binding cohorts
#> ✔ Done
cdm$new_matched_cohort
#> # A tibble: 2 × 5
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date cluster_id
#> *                <int>      <int> <date>            <date>               <int>
#> 1                    1          2 2005-01-05        2006-06-24              11
#> 2                    2         86 2005-01-05        2009-12-25              11
# }
```
