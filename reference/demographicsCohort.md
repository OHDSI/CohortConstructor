# Create cohorts based on patient demographics

`demographicsCohort()` creates a cohort table based on patient
characteristics. If and when an individual satisfies all the criteria
they enter the cohort. When they stop satisfying any of the criteria
their cohort entry ends.

## Usage

``` r
demographicsCohort(
  cdm,
  name,
  ageRange = NULL,
  sex = NULL,
  minPriorObservation = NULL
)
```

## Arguments

- cdm:

  A cdm reference.

- name:

  Name of the new cohort table created in the cdm object.

- ageRange:

  A list of vectors specifying minimum and maximum age.

- sex:

  Can be "Both", "Male" or "Female".

- minPriorObservation:

  A minimum number of continuous prior observation days in the database.

## Value

A cohort table

## Examples

``` r
# \donttest{
library(CohortConstructor)

cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cohort <-  cdm |>
    demographicsCohort(name = "cohort3", ageRange = c(18,40), sex = "Male")
#> ℹ Building new trimmed cohort
#> Adding demographics information
#> Creating initial cohort
#> Trim sex
#> Trim age
#> ✔ Cohort trimmed

attrition(cohort)
#> # A tibble: 5 × 7
#>   cohort_definition_id number_records number_subjects reason_id reason          
#>                  <int>          <int>           <int>     <int> <chr>           
#> 1                    1            100             100         1 Initial qualify…
#> 2                    1            100             100         2 Non-missing sex 
#> 3                    1             45              45         3 Sex requirement…
#> 4                    1             45              45         4 Non-missing yea…
#> 5                    1             32              32         5 Age requirement…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>

# Can also create multiple demographic cohorts, and add minimum prior history requirements.

cohort <- cdm |>
    demographicsCohort(name = "cohort4",
    ageRange = list(c(0, 19),c(20, 64),c(65, 150)),
    sex = c("Male", "Female", "Both"),
    minPriorObservation = 365)
#> ℹ Building new trimmed cohort
#> Adding demographics information
#> Creating initial cohort
#> Trim sex
#> Trim age
#> Trim prior observation
#> ✔ Cohort trimmed

attrition(cohort)
#> # A tibble: 54 × 7
#>    cohort_definition_id number_records number_subjects reason_id reason         
#>                   <int>          <int>           <int>     <int> <chr>          
#>  1                    1            100             100         1 Initial qualif…
#>  2                    1            100             100         2 Non-missing sex
#>  3                    1            100             100         3 Sex requiremen…
#>  4                    1            100             100         4 Non-missing ye…
#>  5                    1             50              50         5 Age requiremen…
#>  6                    1             43              43         6 Prior observat…
#>  7                    2            100             100         1 Initial qualif…
#>  8                    2            100             100         2 Non-missing sex
#>  9                    2             55              55         3 Sex requiremen…
#> 10                    2             55              55         4 Non-missing ye…
#> # ℹ 44 more rows
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>
# }
```
