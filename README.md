
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortConstructor

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/CohortConstructor)](https://CRAN.R-project.org/package=CohortConstructor)
[![codecov.io](https://codecov.io/github/oxford-pharmacoepi/CohortConstructor/coverage.svg?branch=main)](https://app.codecov.io/github/oxford-pharmacoepi/CohortConstructor?branch=main)
[![R-CMD-check](https://github.com/oxford-pharmacoepi/CohortConstructor/workflows/R-CMD-check/badge.svg)](https://github.com/oxford-pharmacoepi/CohortConstructor/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of CohortConstructor is to help on the creation and
manipulation of cohorts in the OMOP Common Data Model.

## Installation

You can install the development version of CohortConstructor from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("oxford-pharmacoepi/CohortConstructor")
```

## Example usage

``` r
library(CDMConnector)
library(PatientProfiles)
library(CohortConstructor)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
cdm <- cdm_from_con(con, cdm_schema = "main", 
                    write_schema = c(prefix = "my_study_", schema = "main"))
```

### Generating concept based cohorts

``` r
cdm <- generate_concept_cohort_set(cdm = cdm, 
                            name = "medications",
                            concept_set = list("diclofenac" = 1124300,
                                               "acetaminophen" = 1127433))
cohort_set(cdm$medications)
#> # A tibble: 2 × 6
#>   cohort_definition_id cohort_name   limit prior_observation future_observation
#>                  <int> <chr>         <chr>             <dbl>              <dbl>
#> 1                    1 diclofenac    first                 0                  0
#> 2                    2 acetaminophen first                 0                  0
#> # ℹ 1 more variable: end <chr>
cohort_count(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    1            830             830
#> 2                    2           2580            2580
```

### Applying demographic requirements

``` r
cdm$medications %>% 
  requireDemographics(ageRange = list(c(40, 65)),
                      sex = "Female")
#> # Source:   SQL [?? x 4]
#> # Database: DuckDB 0.8.1 [eburn@Windows 10 x64:R 4.2.1/C:\Users\eburn\AppData\Local\Temp\Rtmpqm0Jdd\file42405cf744a.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1        730 2002-11-18        2018-12-16     
#>  2                    1       1169 1975-12-23        2018-08-27     
#>  3                    1       1808 2003-12-18        2019-06-05     
#>  4                    1       2858 1953-05-26        2019-05-29     
#>  5                    1       2909 1986-09-03        2007-07-23     
#>  6                    1       2939 1997-10-31        2018-09-04     
#>  7                    1       3175 1999-05-02        2018-09-04     
#>  8                    1       5240 1984-05-31        2019-03-12     
#>  9                    2       1338 1997-02-22        2019-06-21     
#> 10                    2       2026 2009-02-11        2019-06-19     
#> # ℹ more rows
cohort_set(cdm$medications)
#> # A tibble: 2 × 6
#>   cohort_definition_id cohort_name   limit prior_observation future_observation
#>                  <int> <chr>         <chr>             <dbl>              <dbl>
#> 1                    1 diclofenac    first                 0                  0
#> 2                    2 acetaminophen first                 0                  0
#> # ℹ 1 more variable: end <chr>
cohort_count(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    1            156             156
#> 2                    2             76              76
```

### Combining cohorts

Generate a combination cohort.

``` r
cdm <- generateCombinationCohortSet(cdm = cdm, 
                                    name = "combinations", 
                                    targetCohortName = "medications")



cohortSet(cdm$combinations)
#> # A tibble: 3 × 5
#>   cohort_definition_id cohort_name   diclofenac acetaminophen mutually_exclusive
#>                  <int> <chr>              <dbl>         <dbl> <lgl>             
#> 1                    1 diclofenac             1            NA FALSE             
#> 2                    2 acetaminophen         NA             1 FALSE             
#> 3                    3 diclofenac+a…          1             1 FALSE
cohortCount(cdm$combinations)
#> # A tibble: 3 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    1            830             830
#> 2                    2           2580            2580
#> 3                    3            805             805


cdmDisconnect(cdm)
```
