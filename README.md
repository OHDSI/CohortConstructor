
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortConstructor

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/CohortConstructor)](https://CRAN.R-project.org/package=CohortConstructor)
[![codecov.io](https://codecov.io/github/oxford-pharmacoepi/CohortConstructor/coverage.svg?branch=main)](https://app.codecov.io/github/oxford-pharmacoepi/CohortConstructor?branch=main)
[![R-CMD-check](https://github.com/oxford-pharmacoepi/CohortConstructor/workflows/R-CMD-check/badge.svg)](https://github.com/oxford-pharmacoepi/CohortConstructor/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of CohortConstructor is to help on the creation of cohorts in
the OMOP Common Data Model.

## Installation

You can install the development version of CohortConstructor from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("oxford-pharmacoepi/CohortConstructor")
```

## Example

Generate a combination cohort.

``` r
library(PatientProfiles)
#> Warning: package 'PatientProfiles' was built under R version 4.2.3
library(CohortConstructor)
library(CDMConnector)

cdm <- mockPatientProfiles()
cdm <- generateCombinationCohortSet(cdm = cdm, name = "cohort3", targetCohortName = "cohort2")

cdm$cohort3
#> # Source:   table<cohort3> [4 x 4]
#> # Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <dbl> <date>            <date>         
#> 1                    2          1 2020-05-25        2020-05-25     
#> 2                    1          1 2019-12-30        2019-12-30     
#> 3                    1          1 2020-05-25        2020-05-25     
#> 4                    3          1 2020-05-25        2020-05-25

cohortSet(cdm$cohort3)
#> # A tibble: 7 × 6
#>   cohort_definition_id cohort_name cohort_1 cohort_2 cohort_3 mutually_exclusive
#>                  <int> <chr>          <dbl>    <dbl>    <dbl> <lgl>             
#> 1                    1 cohort_1           1       NA       NA FALSE             
#> 2                    2 cohort_2          NA        1       NA FALSE             
#> 3                    3 cohort_1+c…        1        1       NA FALSE             
#> 4                    4 cohort_3          NA       NA        1 FALSE             
#> 5                    5 cohort_1+c…        1       NA        1 FALSE             
#> 6                    6 cohort_2+c…       NA        1        1 FALSE             
#> 7                    7 cohort_1+c…        1        1        1 FALSE

cdmDisconnect(cdm)
```
