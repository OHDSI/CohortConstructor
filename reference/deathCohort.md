# Create cohort based on the death table

Create cohort based on the death table

## Usage

``` r
deathCohort(cdm, name, subsetCohort = NULL, subsetCohortId = NULL)
```

## Arguments

- cdm:

  A cdm reference.

- name:

  Name of the new cohort table created in the cdm object.

- subsetCohort:

  A character refering to a cohort table containing individuals for whom
  cohorts will be generated. Only individuals in this table will appear
  in the generated cohort.

- subsetCohortId:

  Optional. Specifies cohort IDs from the `subsetCohort` table to
  include. If none are provided, all cohorts from the `subsetCohort` are
  included.

## Value

A cohort table with a death cohort in cdm

## Examples

``` r
# \donttest{
library(CohortConstructor)

cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

# Generate a death cohort
death_cohort <- deathCohort(cdm, name = "death_cohort")
#> ℹ Applying cohort requirements.
#> ✔ Cohort death_cohort created.
death_cohort
#> # A tibble: 10 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1         34 1997-03-24        1997-03-24     
#>  2                    1         25 2005-12-07        2005-12-07     
#>  3                    1        100 2007-03-18        2007-03-18     
#>  4                    1         83 2014-01-18        2014-01-18     
#>  5                    1         38 2016-12-21        2016-12-21     
#>  6                    1         57 2018-10-28        2018-10-28     
#>  7                    1         54 2019-04-13        2019-04-13     
#>  8                    1         89 2019-10-29        2019-10-29     
#>  9                    1         52 2019-11-24        2019-11-24     
#> 10                    1         51 2020-01-01        2020-01-01     

# Create a demographics cohort with age range and sex filters
cdm$my_cohort <- demographicsCohort(cdm, "my_cohort", ageRange = c(50,100), sex = "Female")
#> ℹ Building new trimmed cohort
#> Adding demographics information
#> Creating initial cohort
#> Trim sex
#> Trim age
#> ✔ Cohort trimmed
# Generate a death cohort, restricted to individuals in 'my_cohort'
death_cohort <- deathCohort(cdm, name = "death_cohort", subsetCohort = "my_cohort")
#> ℹ Applying cohort requirements.
#> ✔ Cohort death_cohort created.
death_cohort |> attrition()
#> # A tibble: 5 × 7
#>   cohort_definition_id number_records number_subjects reason_id reason          
#>                  <int>          <int>           <int>     <int> <chr>           
#> 1                    1             10              10         1 Initial qualify…
#> 2                    1             10              10         2 Record in obser…
#> 3                    1             10              10         3 Not missing rec…
#> 4                    1              1               1         4 In subset cohort
#> 5                    1              1               1         5 First death rec…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>
# }
```
