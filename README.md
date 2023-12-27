
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortConstructor

<!-- badges: start -->

[![R-CMD-check](https://github.com/oxford-pharmacoepi/CohortConstructor/workflows/R-CMD-check/badge.svg)](https://github.com/oxford-pharmacoepi/CohortConstructor/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of CohortConstructor is to help on the creation and
manipulation of cohorts in the OMOP Common Data Model. The package
provides functions to support cohort building pipelines and additional
functions to support cohort evaluation.

## Installation

You can install the development version of CohortConstructor from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("oxford-pharmacoepi/CohortConstructor")
```

## Creating and manipulating cohorts

``` r
library(CDMConnector)
library(PatientProfiles)
library(DrugUtilisation)
#> Warning: package 'DrugUtilisation' was built under R version 4.2.3
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(CohortConstructor)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
cdm <- cdm_from_con(con, cdm_schema = "main", 
                    write_schema = c(prefix = "my_study_", schema = "main"))
```

### Generating concept based cohorts

We’ll start by generating a set of drug cohorts, using
generateDrugUtilisationCohortSet from the DrugUtilisation R package.
Here we make two cohorts, one for diclofenac and another for
acetaminophen, combining records with a gap of 7 days or less.

``` r
cdm <- generateDrugUtilisationCohortSet(cdm = cdm,  
                            name = "medications",
                            conceptSet = list("diclofenac" = 1124300,
                                           "acetaminophen" = 1127433),
                            gapEra = 7)
```

We can see that our starting cohorts, before we add any additional
restrictions, have the following counts

``` r
cohort_set(cdm$medications) %>% glimpse()
#> Rows: 2
#> Columns: 11
#> $ cohort_definition_id    <int> 1, 2
#> $ cohort_name             <chr> "diclofenac", "acetaminophen"
#> $ duration_range_min      <chr> "1", "1"
#> $ duration_range_max      <chr> "Inf", "Inf"
#> $ impute_duration         <chr> "none", "none"
#> $ gap_era                 <chr> "7", "7"
#> $ prior_use_washout       <chr> "0", "0"
#> $ prior_observation       <chr> "0", "0"
#> $ cohort_date_range_start <chr> NA, NA
#> $ cohort_date_range_end   <chr> NA, NA
#> $ limit                   <chr> "all", "all"
cohort_count(cdm$medications) %>% glimpse()
#> Rows: 2
#> Columns: 3
#> $ cohort_definition_id <int> 2, 1
#> $ number_records       <dbl> 9363, 830
#> $ number_subjects      <dbl> 2580, 830
```

### Require in date range

We can require that individuals’ cohort start date fall within a certain
date range.

``` r
cdm$medications <- cdm$medications %>% 
  requireInDateRange(indexDate = "cohort_start_date",
                     dateRange = as.Date(c("2000-01-01", "2020-01-01")))
```

Now that we’ve applied these date restrictions, we can see how many
people and records have been excluded

``` r
cohort_count(cdm$medications) %>% glimpse()
#> Rows: 2
#> Columns: 3
#> $ cohort_definition_id <int> 2, 1
#> $ number_records       <dbl> 2750, 397
#> $ number_subjects      <dbl> 1737, 397
cohort_attrition(cdm$medications) %>% glimpse()
#> Rows: 16
#> Columns: 7
#> $ cohort_definition_id <int> 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
#> $ number_records       <dbl> 9365, 9365, 9363, 9363, 9363, 9363, 9363, 850, 85…
#> $ number_subjects      <dbl> 2580, 2580, 2580, 2580, 2580, 2580, 2580, 850, 85…
#> $ reason_id            <dbl> 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 8, 8
#> $ reason               <chr> "Qualifying initial records", "Duration imputatio…
#> $ excluded_records     <dbl> 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 6613, …
#> $ excluded_subjects    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 843, 4…
cohort_attrition(cdm$medications) %>% 
  filter(reason == "cohort_start_date between 2000-01-01 and 2020-01-01") %>% 
  glimpse()
#> Rows: 2
#> Columns: 7
#> $ cohort_definition_id <int> 2, 1
#> $ number_records       <dbl> 2750, 397
#> $ number_subjects      <dbl> 1737, 397
#> $ reason_id            <dbl> 8, 8
#> $ reason               <chr> "cohort_start_date between 2000-01-01 and 2020-01…
#> $ excluded_records     <dbl> 6613, 433
#> $ excluded_subjects    <dbl> 843, 433
```

### Applying demographic requirements

We can also add restrictions on age (on cohort start date) and sex.

``` r
cdm$medications <- cdm$medications %>% 
  requireDemographics(indexDate = "cohort_start_date",
                      ageRange = list(c(40, 65)),
                      sex = "Female")
```

Again we can see how many individuals we’ve lost after applying this
criteria.

``` r
cohort_attrition(cdm$medications) %>% 
  filter(reason == "Demographic requirements") %>% 
  glimpse()
#> Rows: 2
#> Columns: 7
#> $ cohort_definition_id <int> 2, 1
#> $ number_records       <dbl> 787, 75
#> $ number_subjects      <dbl> 551, 75
#> $ reason_id            <dbl> 9, 9
#> $ reason               <chr> "Demographic requirements", "Demographic requirem…
#> $ excluded_records     <dbl> 1963, 322
#> $ excluded_subjects    <dbl> 1186, 322
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
```

``` r
cohort_attrition(cdm$medications) %>% 
  filter(reason == "In cohort gibleed between -Inf and 0 days relative to cohort_start_date") %>% 
  glimpse()
#> Rows: 2
#> Columns: 7
#> $ cohort_definition_id <int> 2, 1
#> $ number_records       <dbl> 136, 0
#> $ number_subjects      <dbl> 89, 0
#> $ reason_id            <dbl> 10, 10
#> $ reason               <chr> "In cohort gibleed between -Inf and 0 days relati…
#> $ excluded_records     <dbl> 651, 75
#> $ excluded_subjects    <dbl> 462, 75
```

### Combining cohorts

Currently we have two separate cohorts. One of users of diclofenac, the
other of users of acetaminophen.

Let’s say we want to create a cohort of people taking **either**
diclofenac or acetaminophen. We could create this cohort like so:

Alternatively, we might want to create a cohort of people taking
**both** diclofenac and acetaminophen. For this we can create this
combination cohort like so:

Both diclofenac and acetaminophen

Generate a combination cohort.

``` r
cdm <- generateIntersectCohortSet(cdm = cdm, 
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
#> 1                    2            136              89
#> 2                    1              0               0
#> 3                    3              0               0
```

``` r
cdmDisconnect(cdm)
```

## Evaluating cohorts
