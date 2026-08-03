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
#> # A tibble: 108 × 5
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date   age
#>  *                <int>      <int> <date>            <date>          <dbl>
#>  1                    1          1 2005-05-25        2006-08-20          7
#>  2                    2          2 1987-06-29        1990-01-03         24
#>  3                    1          6 2014-03-30        2015-02-21         53
#>  4                    2          7 2018-04-07        2018-04-25         31
#>  5                    1         10 2008-12-27        2010-04-02         10
#>  6                    2         13 2010-12-10        2011-06-29         22
#>  7                    2         14 1995-02-12        2002-11-12         17
#>  8                    1         15 2009-04-01        2009-11-09         36
#>  9                    1         17 2008-06-28        2008-12-28         56
#> 10                    2         18 2019-08-14        2019-08-15         51
#> # ℹ 98 more rows

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
#>  1                    1             54              54         1 Initial qualif…
#>  2                    1             25              25         2 filter strata:…
#>  3                    2             54              54         1 Initial qualif…
#>  4                    2             29              29         2 filter strata:…
#>  5                    3             54              54         1 Initial qualif…
#>  6                    3             25              25         2 filter strata:…
#>  7                    3             14              14         3 filter strata:…
#>  8                    4             54              54         1 Initial qualif…
#>  9                    4             25              25         2 filter strata:…
#> 10                    4             11              11         3 filter strata:…
#> 11                    5             54              54         1 Initial qualif…
#> 12                    5             29              29         2 filter strata:…
#> 13                    5             24              24         3 filter strata:…
#> 14                    6             54              54         1 Initial qualif…
#> 15                    6             29              29         2 filter strata:…
#> 16                    6              5               5         3 filter strata:…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>
# }
```
