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
#> Warning: '/tmp/RtmpTrjJ4I/id_mds' already exists
#> ℹ Reading GiBleed tables.

# Generate a death cohort
death_cohort <- deathCohort(cdm, name = "death_cohort")
#> ℹ Applying cohort requirements.
#> ✔ Cohort death_cohort created.
death_cohort
#> # A tibble: 10 × 4
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1         14 2005-01-07        2005-01-07     
#>  2                    1         34 2008-02-07        2008-02-07     
#>  3                    1         59 2009-05-28        2009-05-28     
#>  4                    1         51 2009-09-13        2009-09-13     
#>  5                    1         82 2013-04-17        2013-04-17     
#>  6                    1          1 2014-05-01        2014-05-01     
#>  7                    1         43 2014-09-11        2014-09-11     
#>  8                    1         68 2015-07-03        2015-07-03     
#>  9                    1         87 2015-10-30        2015-10-30     
#> 10                    1         39 2019-12-02        2019-12-02     

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
#> 4                    1              0               0         4 In subset cohor…
#> 5                    1              0               0         5 First death rec…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>
# }
```
