
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
#> Warning: package 'CDMConnector' was built under R version 4.2.3
library(PatientProfiles)
#> Warning: package 'PatientProfiles' was built under R version 4.2.3
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
cohort_count(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    1            830             830
#> 2                    2           2580            2580
cohort_attrition(cdm$medications)
#> # A tibble: 2 × 7
#>   cohort_definition_id number_records number_subjects reason_id reason          
#>                  <int>          <dbl>           <dbl>     <dbl> <chr>           
#> 1                    1            830             830         1 Qualifying init…
#> 2                    2           2580            2580         1 Qualifying init…
#> # ℹ 2 more variables: excluded_records <dbl>, excluded_subjects <dbl>
```

### Applying demographic requirements

``` r
cdm$medications %>% 
  requireDemographics(ageRange = list(c(40, 65)),
                      sex = "Female")
#> # Source:   SQL [?? x 4]
#> # Database: DuckDB 0.8.1 [eburn@Windows 10 x64:R 4.2.1/C:\Users\eburn\AppData\Local\Temp\RtmpIngDmK\file4f3841e26e9e.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <dbl> <date>            <date>         
#>  1                    1         18 2009-03-21        2018-11-07     
#>  2                    1        893 1993-09-26        2019-05-06     
#>  3                    1       2396 1961-08-30        2001-02-28     
#>  4                    1       3159 2000-01-26        2018-10-18     
#>  5                    1       3376 1994-05-06        2019-06-28     
#>  6                    1       4071 1998-08-07        2018-12-27     
#>  7                    1       4636 1986-10-26        2018-10-23     
#>  8                    1       4690 2001-03-20        2018-10-14     
#>  9                    1       4701 2011-07-10        2018-12-22     
#> 10                    2       3951 1997-12-11        2019-04-13     
#> # ℹ more rows
cohort_count(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    1            156             156
#> 2                    2             76              76
cohort_attrition(cdm$medications)
#> # A tibble: 4 × 7
#>   cohort_definition_id number_records number_subjects reason_id reason          
#>                  <int>          <dbl>           <dbl>     <dbl> <chr>           
#> 1                    1            830             830         1 Qualifying init…
#> 2                    2           2580            2580         1 Qualifying init…
#> 3                    2             76              76         2 Demographic req…
#> 4                    1            156             156         2 Demographic req…
#> # ℹ 2 more variables: excluded_records <dbl>, excluded_subjects <dbl>
```

### Require presence in another cohort

We can also require that individuals are in another cohort over some
window. Here for example we require that study participants are in a GI
bleed cohort any time prior up to their entry in the medications cohort.

``` r
cdm <- generate_concept_cohort_set(cdm = cdm, 
                            name = "gibleed",
                            concept_set = list("gibleed" = 192671))

cdm$medications <- cdm$medications %>% 
  requireCohortIntersectFlag(targetCohortTable = "gibleed",
                             window = c(-Inf, 0))

cohort_count(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    2             36              36
#> 2                    1              0               0
cohort_attrition(cdm$medications)
#> # A tibble: 6 × 7
#>   cohort_definition_id number_records number_subjects reason_id reason          
#>                  <int>          <dbl>           <dbl>     <dbl> <chr>           
#> 1                    1            830             830         1 Qualifying init…
#> 2                    2           2580            2580         1 Qualifying init…
#> 3                    2             76              76         2 Demographic req…
#> 4                    1            156             156         2 Demographic req…
#> 5                    2             36              36         3 In cohort gible…
#> 6                    1              0               0         3 In cohort gible…
#> # ℹ 2 more variables: excluded_records <dbl>, excluded_subjects <dbl>
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
#> 1                    2             36              36
#> 2                    1              0               0
#> 3                    3              0               0


cdmDisconnect(cdm)
```
