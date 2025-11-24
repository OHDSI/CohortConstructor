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
#> # A tibble: 100 × 5
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date   age
#>  *                <int>      <int> <date>            <date>          <int>
#>  1                    1          1 1992-05-30        1992-07-07          8
#>  2                    2          2 1986-03-15        1988-05-05          6
#>  3                    1          3 1997-03-12        1997-03-14          4
#>  4                    1          5 2018-12-02        2019-03-07         19
#>  5                    2          6 1990-07-04        1998-06-18         23
#>  6                    1         10 1985-04-08        1988-04-11          9
#>  7                    2         12 2014-08-23        2016-01-13         34
#>  8                    1         13 1988-10-25        1989-10-25          4
#>  9                    1         14 1991-12-12        1993-09-26         39
#> 10                    2         15 2007-11-16        2008-03-16         10
#> # ℹ 90 more rows

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
#>  1                    1             50              50         1 Initial qualif…
#>  2                    1             18              18         2 filter strata:…
#>  3                    2             50              50         1 Initial qualif…
#>  4                    2             32              32         2 filter strata:…
#>  5                    3             50              50         1 Initial qualif…
#>  6                    3             18              18         2 filter strata:…
#>  7                    3             11              11         3 filter strata:…
#>  8                    4             50              50         1 Initial qualif…
#>  9                    4             18              18         2 filter strata:…
#> 10                    4              7               7         3 filter strata:…
#> 11                    5             50              50         1 Initial qualif…
#> 12                    5             32              32         2 filter strata:…
#> 13                    5             18              18         3 filter strata:…
#> 14                    6             50              50         1 Initial qualif…
#> 15                    6             32              32         2 filter strata:…
#> 16                    6             14              14         3 filter strata:…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>
# }
```
