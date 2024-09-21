
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortConstructor <img src="man/figures/logo.png" align="right" height="180"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/CohortConstructor)](https://CRAN.R-project.org/package=CohortConstructor)
[![R-CMD-check](https://github.com/OHDSI/CohortConstructor/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/CohortConstructor/actions)
[![Codecov test
coverage](https://codecov.io/gh/OHDSI/CohortConstructor/branch/main/graph/badge.svg)](https://app.codecov.io/gh/OHDSI/CohortConstructor?branch=main)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of CohortConstructor is to support the creation and
manipulation of study cohorts in data mapped to the OMOP CDM.

## Installation

The package can be installed from CRAN:

``` r
install.packages("CohortConstructor")
```

Or you can install the development version of the package from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("ohdsi/CohortConstructor")
```

## Creating and manipulating cohorts

To illustrate the functionality provided by CohortConstructor let’s
create a set of fracture cohorts using the Eunomia dataset. We’ll first
load required packages and create a cdm reference for the data.

``` r
library(omopgenerics)
library(CDMConnector)
library(PatientProfiles)
library(dplyr)
library(CohortConstructor)
library(CohortCharacteristics)
```

``` r
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
cdm <- cdm_from_con(con, cdm_schema = "main", 
                    write_schema = c(prefix = "my_study_", schema = "main"))
cdm
#> 
#> ── # OMOP CDM reference (duckdb) of Synthea synthetic health database ──────────
#> • omop tables: person, observation_period, visit_occurrence, visit_detail,
#> condition_occurrence, drug_exposure, procedure_occurrence, device_exposure,
#> measurement, observation, death, note, note_nlp, specimen, fact_relationship,
#> location, care_site, provider, payer_plan_period, cost, drug_era, dose_era,
#> condition_era, metadata, cdm_source, concept, vocabulary, domain,
#> concept_class, concept_relationship, relationship, concept_synonym,
#> concept_ancestor, source_to_concept_map, drug_strength
#> • cohort tables: -
#> • achilles tables: -
#> • other tables: -
```

### Generating concept-based fracture cohorts

We will start by making a simple concept-based cohort for each of our
fracture of interest. First we create a codelist for each ankle, forearm
and hip fractures (note, we just use one code for each because we are
using synthetic data).

``` r
fracture_codes <- newCodelist(list("ankle_fracture" = 4059173L,
                                   "forearm_fracture" = 4278672L,
                                   "hip_fracture" = 4230399L))
fracture_codes
#> 
#> ── 3 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - ankle_fracture (1 codes)
#> - forearm_fracture (1 codes)
#> - hip_fracture (1 codes)
```

Now we can quickly create a set of cohorts for each fracture type. For
this we only need to provide the codes we have defined and we will get a
cohort back, with cohort end date set as the event date associated with
the records, overlapping records collapsed, and only records in
observation kept.

``` r
cdm$fractures <- cdm |> 
  conceptCohort(conceptSet = fracture_codes,
  name = "fractures")
```

We can see that our starting cohorts, before we add any additional
restrictions, have the following associated settings, counts, and
attrition.

``` r
settings(cdm$fractures) %>% glimpse()
#> Rows: 3
#> Columns: 4
#> $ cohort_definition_id <int> 1, 2, 3
#> $ cohort_name          <chr> "ankle_fracture", "forearm_fracture", "hip_fractu…
#> $ cdm_version          <chr> "5.3", "5.3", "5.3"
#> $ vocabulary_version   <chr> "v5.0 18-JAN-19", "v5.0 18-JAN-19", "v5.0 18-JAN-…
cohort_count(cdm$fractures) %>% glimpse()
#> Rows: 3
#> Columns: 3
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 464, 569, 138
#> $ number_subjects      <int> 427, 510, 132
attrition(cdm$fractures) %>% glimpse()
#> Rows: 9
#> Columns: 7
#> $ cohort_definition_id <int> 1, 1, 1, 2, 2, 2, 3, 3, 3
#> $ number_records       <int> 464, 464, 464, 569, 569, 569, 138, 138, 138
#> $ number_subjects      <int> 427, 427, 427, 510, 510, 510, 132, 132, 132
#> $ reason_id            <int> 1, 2, 3, 1, 2, 3, 1, 2, 3
#> $ reason               <chr> "Initial qualifying events", "cohort requirements…
#> $ excluded_records     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0
#> $ excluded_subjects    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0
```

### Create an overall fracture cohort

So far we have created three separate fracture cohorts. Let’s say we
also want a cohort of people with any of the fractures. We could union
our three cohorts to create this overall cohort like so:

``` r
cdm$fractures <- unionCohorts(cdm$fractures,
                              cohortName = "any_fracture", 
                              keepOriginalCohorts = TRUE,
                              name ="fractures")
```

``` r
settings(cdm$fractures)
#> # A tibble: 4 × 5
#>   cohort_definition_id cohort_name      cdm_version vocabulary_version   gap
#>                  <int> <chr>            <chr>       <chr>              <dbl>
#> 1                    1 ankle_fracture   5.3         v5.0 18-JAN-19        NA
#> 2                    2 forearm_fracture 5.3         v5.0 18-JAN-19        NA
#> 3                    3 hip_fracture     5.3         v5.0 18-JAN-19        NA
#> 4                    4 any_fracture     <NA>        <NA>                   0
cohortCount(cdm$fractures)
#> # A tibble: 4 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1            464             427
#> 2                    2            569             510
#> 3                    3            138             132
#> 4                    4           1171             924
```

### Require in date range

Once we have created our base fracture cohort, we can then start
applying additional cohort requirements. For example, first we can
require that individuals’ cohort start date fall within a certain date
range.

``` r
cdm$fractures <- cdm$fractures %>% 
  requireInDateRange(dateRange = as.Date(c("2000-01-01", "2020-01-01")))
```

Now that we’ve applied this date restriction, we can see that our cohort
attributes have been updated

``` r
cohort_count(cdm$fractures) %>% glimpse()
#> Rows: 4
#> Columns: 3
#> $ cohort_definition_id <int> 1, 2, 3, 4
#> $ number_records       <int> 108, 152, 62, 322
#> $ number_subjects      <int> 104, 143, 60, 287
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
#> Rows: 4
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3, 4
#> $ number_records       <int> 43, 64, 22, 129
#> $ number_subjects      <int> 43, 62, 22, 122
#> $ reason_id            <int> 6, 6, 6, 4
#> $ reason               <chr> "Age requirement: 40 to 65", "Age requirement: 40…
#> $ excluded_records     <int> 65, 88, 40, 193
#> $ excluded_subjects    <int> 61, 81, 38, 165

attrition(cdm$fractures) %>% 
  filter(reason == "Sex requirement: Female") %>% 
  glimpse()
#> Rows: 4
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3, 4
#> $ number_records       <int> 19, 37, 12, 68
#> $ number_subjects      <int> 19, 36, 12, 65
#> $ reason_id            <int> 7, 7, 7, 5
#> $ reason               <chr> "Sex requirement: Female", "Sex requirement: Fema…
#> $ excluded_records     <int> 24, 27, 10, 61
#> $ excluded_subjects    <int> 24, 26, 10, 57
```

### Require presence in another cohort

We can also require that individuals are (or are not) in another cohort
over some window. Here for example we require that study participants
are in a GI bleed cohort any time prior up to their entry in the
fractures cohort.

``` r
cdm$gibleed <- cdm |> 
  conceptCohort(conceptSet = list("gibleed" = 192671L),
  name = "gibleed")

cdm$fractures <- cdm$fractures %>% 
  requireCohortIntersect(targetCohortTable = "gibleed",
                         intersections = 0,
                         window = c(-Inf, 0))
```

``` r
attrition(cdm$fractures) %>% 
  filter(reason == "Not in cohort gibleed between -Inf & 0 days relative to cohort_start_date") %>% 
  glimpse()
#> Rows: 4
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3, 4
#> $ number_records       <int> 14, 30, 10, 54
#> $ number_subjects      <int> 14, 30, 10, 52
#> $ reason_id            <int> 10, 10, 10, 8
#> $ reason               <chr> "Not in cohort gibleed between -Inf & 0 days rela…
#> $ excluded_records     <int> 5, 7, 2, 14
#> $ excluded_subjects    <int> 5, 6, 2, 13
```

``` r
cdmDisconnect(cdm)
```

### More information

CohortConstructor provides much more functionality for creating and
manipulating cohorts. See the package website for more details.
