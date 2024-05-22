
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortConstructor <img src="man/figures/logo.png" align="right" height="180"/>

> **This package is currently experimental and not currently recommended
> for use.**

<!-- badges: start -->

[![R-CMD-check](https://github.com/OHDSI/CohortConstructor/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/CohortConstructor/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of CohortConstructor is to support the creation and
manipulation of cohorts in the OMOP Common Data Model.

## Installation

You can install the development version of CohortConstructor from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ohdsi/CohortConstructor")
```

## Creating and manipulating cohorts

To illustrate how the functionality let’s create a CDM reference for the
Eunomia dataset Using the CDMConnector package.

``` r
library(CDMConnector)
library(PatientProfiles)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
library(CohortConstructor)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
cdm <- cdm_from_con(con, cdm_schema = "main", 
                    write_schema = c(prefix = "my_study_", schema = "main"))
print(cdm)
```

### Generating concept based cohorts

We start by making a concept based cohort. For this we only need to
provide concept sets and we will get a cohort back, with cohort end date
the event date associated with the records, overlapping records
collapsed, and only records in observation kept.

``` r
cdm$fractures <- cdm |> 
  conceptCohort(conceptSet = list(
    "ankle_fracture" = 4059173,
    "forearm_fracture" = 4278672,
    "hip_fracture" = 4230399),
  name = "fractures")
```

We can see that our starting cohorts, before we add any additional
restrictions, have the following associated settings, counts, and
attrition.

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
attrition(cdm$fractures) %>% glimpse()
#> Rows: 3
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 464, 569, 138
#> $ number_subjects      <int> 427, 510, 132
#> $ reason_id            <int> 1, 1, 1
#> $ reason               <chr> "Initial qualifying events", "Initial qualifying …
#> $ excluded_records     <int> 0, 0, 0
#> $ excluded_subjects    <int> 0, 0, 0
```

### Require in date range

Once we have created our base cohort, we can then start applying
additional cohort requirements. For example, first we can require that
individuals’ cohort start date fall within a certain date range.

``` r
cdm$fractures <- cdm$fractures %>% 
  requireInDateRange(dateRange = as.Date(c("2000-01-01", "2020-01-01")))
```

Now that we’ve applied this date restriction, we can see that our cohort
attributes have been updated

``` r
cohort_count(cdm$fractures) %>% glimpse()
#> Rows: 3
#> Columns: 3
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 108, 152, 62
#> $ number_subjects      <int> 104, 143, 60
attrition(cdm$fractures) %>% 
  filter(reason == "cohort_start_date between 2000-01-01 & 2020-01-01") %>% 
  glimpse()
#> Rows: 0
#> Columns: 7
#> $ cohort_definition_id <int> 
#> $ number_records       <int> 
#> $ number_subjects      <int> 
#> $ reason_id            <int> 
#> $ reason               <chr> 
#> $ excluded_records     <int> 
#> $ excluded_subjects    <int>
```

### Applying demographic requirements

We can also add restrictions on patient characteristics such as age (on
cohort start date by default) and sex.

``` r
cdm$fractures <- cdm$fractures %>% 
  requireDemographics(ageRange = list(c(40, 65)),
                      sex = "Female")
```

Again we can see how many individuals we’ve lost after applying these
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
#> $ reason_id            <int> 4, 4, 4
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
#> $ reason_id            <int> 5, 5, 5
#> $ reason               <chr> "Sex requirement: Female", "Sex requirement: Fema…
#> $ excluded_records     <int> 24, 27, 10
#> $ excluded_subjects    <int> 24, 26, 10
```

### Require presence in another cohort

We can also require that individuals are in another cohort over some
window. Here for example we require that study participants are in a GI
bleed cohort any time prior up to their entry in the fractures cohort.

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
#> $ reason_id            <int> 8, 8, 8
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
