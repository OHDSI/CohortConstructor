# Create a new cohort table from stratifying an existing one

`stratifyCohorts()` creates new cohorts, splitting an existing cohort
based on specified columns on which to stratify on.

## Usage

``` r
stratifyCohorts(
  cohort,
  strata,
  cohortId = NULL,
  removeStrata = TRUE,
  name = tableName(cohort)
)
```

## Arguments

- cohort:

  A cohort table in a cdm reference.

- strata:

  A strata list that point to columns in cohort table.

- cohortId:

  Vector identifying which cohorts to include (cohort_definition_id or
  cohort_name). Cohorts not included will be removed from the cohort
  set.

- removeStrata:

  Whether to remove strata columns from final cohort table.

- name:

  Name of the new cohort table created in the cdm object.

## Value

Cohort table stratified.

## Examples

``` r
# \donttest{
library(CohortConstructor)
library(PatientProfiles)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm$my_cohort <- cdm$cohort1 |>
  addAge(ageGroup = list("child" = c(0, 17), "adult" = c(18, Inf))) |>
  addSex(name = "my_cohort") |>
  stratifyCohorts(
    strata = list("sex", c("sex", "age_group")), name = "my_cohort"
  )

cdm$my_cohort
#> # A tibble: 112 × 5
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date   age
#>  *                <int>      <int> <date>            <date>          <int>
#>  1                    2          1 2003-05-04        2003-10-23         41
#>  2                    1          2 1992-08-18        2008-04-17         40
#>  3                    1          4 2004-08-17        2004-08-22         36
#>  4                    1          6 2009-12-23        2015-12-23          9
#>  5                    1          7 2019-05-11        2019-05-12         34
#>  6                    1          8 2008-11-15        2018-02-21         58
#>  7                    1          9 2008-01-15        2008-04-02         42
#>  8                    2         10 1994-10-09        1997-09-04         11
#>  9                    2         11 1972-12-02        1973-05-02         18
#> 10                    1         12 1994-03-30        1994-10-12         16
#> # ℹ 102 more rows

settings(cdm$my_cohort)
#> # A tibble: 6 × 8
#>   cohort_definition_id cohort_name           target_cohort_id target_cohort_name
#>                  <int> <chr>                            <int> <chr>             
#> 1                    1 cohort_1_female                      1 cohort_1          
#> 2                    2 cohort_1_male                        1 cohort_1          
#> 3                    3 cohort_1_female_adult                1 cohort_1          
#> 4                    4 cohort_1_female_child                1 cohort_1          
#> 5                    5 cohort_1_male_adult                  1 cohort_1          
#> 6                    6 cohort_1_male_child                  1 cohort_1          
#> # ℹ 4 more variables: target_cohort_table_name <chr>, strata_columns <chr>,
#> #   sex <chr>, age_group <chr>

attrition(cdm$my_cohort)
#> # A tibble: 16 × 7
#>    cohort_definition_id number_records number_subjects reason_id reason         
#>                   <int>          <int>           <int>     <int> <chr>          
#>  1                    1             56              56         1 Initial qualif…
#>  2                    1             30              30         2 filter strata:…
#>  3                    2             56              56         1 Initial qualif…
#>  4                    2             26              26         2 filter strata:…
#>  5                    3             56              56         1 Initial qualif…
#>  6                    3             30              30         2 filter strata:…
#>  7                    3             22              22         3 filter strata:…
#>  8                    4             56              56         1 Initial qualif…
#>  9                    4             30              30         2 filter strata:…
#> 10                    4              8               8         3 filter strata:…
#> 11                    5             56              56         1 Initial qualif…
#> 12                    5             26              26         2 filter strata:…
#> 13                    5             20              20         3 filter strata:…
#> 14                    6             56              56         1 Initial qualif…
#> 15                    6             26              26         2 filter strata:…
#> 16                    6              6               6         3 filter strata:…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>
# }
```
