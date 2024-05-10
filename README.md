
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortConstructor

> **This package is currently experimental and not currently recommended
> for use.**

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
devtools::install_github("ohdsi/CohortConstructor")
```

## Creating and manipulating cohorts

``` r
library(CDMConnector)
library(PatientProfiles)
#> Warning: package 'PatientProfiles' was built under R version 4.2.3
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
library(CohortConstructor)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
cdm <- cdm_from_con(con, cdm_schema = "main", 
                    write_schema = c(prefix = "my_study_", schema = "main"))
print(cdm)
```

### Generating concept based cohorts

``` r
cdm$fractures <- cdm |> 
  conceptCohort(conceptSet = list(
    "ankle_fracture" = 4059173,
    "forearm_fracture" = 4278672,
    "hip_fracture" = 4230399),
  name = "fractures")
```

We can see that our starting cohorts, before we add any additional
restrictions, have the following counts

``` r
settings(cdm$fractures) %>% glimpse()
#> Rows: 3
#> Columns: 2
#> $ cohort_definition_id <int> 1, 2, 3
#> $ cohort_name          <chr> "ankle_fracture", "forearm_fracture", "hip_fractu…
cohort_count(cdm$fractures) %>% glimpse()
#> Rows: 3
#> Columns: 3
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 464, 569, 138
#> $ number_subjects      <int> 427, 510, 132
```

### Require in date range

We can require that individuals’ cohort start date fall within a certain
date range.

``` r
cdm$fractures <- cdm$fractures %>% 
  requireInDateRange(indexDate = "cohort_start_date",
                     dateRange = as.Date(c("2000-01-01", "2020-01-01")))
```

Now that we’ve applied these date restrictions, we can see how many
people and records have been excluded

``` r
cohort_count(cdm$fractures) %>% glimpse()
#> Rows: 3
#> Columns: 3
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 108, 152, 62
#> $ number_subjects      <int> 104, 143, 60
attrition(cdm$fractures) %>% glimpse()
#> Rows: 6
#> Columns: 7
#> $ cohort_definition_id <int> 1, 1, 2, 2, 3, 3
#> $ number_records       <int> 464, 108, 569, 152, 138, 62
#> $ number_subjects      <int> 427, 104, 510, 143, 132, 60
#> $ reason_id            <int> 1, 2, 1, 2, 1, 2
#> $ reason               <chr> "Initial qualifying events", "cohort_start_date b…
#> $ excluded_records     <int> 0, 356, 0, 417, 0, 76
#> $ excluded_subjects    <int> 0, 323, 0, 367, 0, 72
attrition(cdm$fractures) %>% 
  filter(reason == "cohort_start_date between 2000-01-01 & 2020-01-01") %>% 
  glimpse()
#> Rows: 3
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 108, 152, 62
#> $ number_subjects      <int> 104, 143, 60
#> $ reason_id            <int> 2, 2, 2
#> $ reason               <chr> "cohort_start_date between 2000-01-01 & 2020-01-0…
#> $ excluded_records     <int> 356, 417, 76
#> $ excluded_subjects    <int> 323, 367, 72
```

### Applying demographic requirements

We can also add restrictions on age (on cohort start date) and sex.

``` r
cdm$fractures <- cdm$fractures %>% 
  requireDemographics(indexDate = "cohort_start_date",
                      ageRange = list(c(40, 65)),
                      sex = "Female")
```

Again we can see how many individuals we’ve lost after applying this
criteria.

``` r
attrition(cdm$fractures) %>% 
  filter(reason == "Age requirement: 40 to 65") %>% 
  glimpse()
#> Rows: 3
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 43, 64, 22
#> $ number_subjects      <int> 43, 62, 22
#> $ reason_id            <int> 3, 3, 3
#> $ reason               <chr> "Age requirement: 40 to 65", "Age requirement: 40…
#> $ excluded_records     <int> 65, 88, 40
#> $ excluded_subjects    <int> 61, 81, 38

attrition(cdm$fractures) %>% 
  filter(reason == "Sex requirement: Female") %>% 
  glimpse()
#> Rows: 3
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 19, 37, 12
#> $ number_subjects      <int> 19, 36, 12
#> $ reason_id            <int> 4, 4, 4
#> $ reason               <chr> "Sex requirement: Female", "Sex requirement: Fema…
#> $ excluded_records     <int> 24, 27, 10
#> $ excluded_subjects    <int> 24, 26, 10
```

### Require presence in another cohort

We can also require that individuals are in another cohort over some
window. Here for example we require that study participants are in a GI
bleed cohort any time prior up to their entry in the medications cohort.

``` r
cdm$gibleed <- cdm |> 
  conceptCohort(conceptSet = list("gibleed" = 192671),
  name = "gibleed")


cdm$fractures <- cdm$fractures %>% 
  requireCohortIntersectFlag(targetCohortTable = "gibleed",
                             window = c(-Inf, 0))
```

``` r
attrition(cdm$fractures) %>% 
  filter(reason == "In cohort gibleed between -Inf & 0 days relative to cohort_start_date") %>% 
  glimpse()
#> Rows: 3
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 5, 7, 2
#> $ number_subjects      <int> 5, 6, 2
#> $ reason_id            <int> 7, 7, 7
#> $ reason               <chr> "In cohort gibleed between -Inf & 0 days relative…
#> $ excluded_records     <int> 14, 30, 10
#> $ excluded_subjects    <int> 14, 30, 10
```

### Combining cohorts

Currently we have separate fracture cohorts.

Let’s say we want to create a cohort of people with any of the
fractures. We could create this cohort like so:

``` r
cdm$fractures <- cdm$fractures |> 
  CohortConstructor::unionCohorts()

settings(cdm$fractures)
#> # A tibble: 1 × 3
#>   cohort_definition_id cohort_name                                    gap
#>                  <dbl> <chr>                                        <dbl>
#> 1                    1 ankle_fracture_forearm_fracture_hip_fracture     0
cohortCount(cdm$fractures)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1             14              13
```

``` r
cdmDisconnect(cdm)
```
