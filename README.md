
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
create a cohort of people with a fracture using the Eunomia dataset.
We’ll first load required packages and create a cdm reference for the
data.

``` r
library(omopgenerics)
library(CDMConnector)
library(PatientProfiles)
library(dplyr)
library(CohortConstructor)
library(CohortCharacteristics)
```

``` r
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
cdm <- cdmFromCon(con, cdmSchema = "main", 
                    writeSchema = c(prefix = "my_study_", schema = "main"))
cdm
#> 
#> ── # OMOP CDM reference (duckdb) of Synthea ────────────────────────────────────
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

We will first need to identify codes that could be used to represent
fractures of interest. To find these we’ll use the CodelistGenerator
package (note, we will just find a few codes because we are using
synthetic data with a subset of the full vocabularies).

``` r
library(CodelistGenerator)

hip_fx_codes <- getCandidateCodes(cdm, "hip fracture")
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 1 candidate concept identified
#> 
#> Time taken: 0 minutes and 0 seconds
forearm_fx_codes <- getCandidateCodes(cdm, "forearm fracture")
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 1 candidate concept identified
#> 
#> Time taken: 0 minutes and 0 seconds

fx_codes <- newCodelist(list("hip_fracture" = hip_fx_codes$concept_id,
                             "forearm_fracture"= forearm_fx_codes$concept_id))
fx_codes
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - forearm_fracture (1 codes)
#> - hip_fracture (1 codes)
```

Now we can quickly create our cohorts. For this we only need to provide
the codes we have defined and we will get a cohort back, where we start
by setting cohort exit as the same day as event start (the date of the
fracture).

``` r
cdm$fractures <- cdm |> 
  conceptCohort(conceptSet = fx_codes, 
                exit = "event_start_date", 
                name = "fractures")
```

After creating our initial cohort we will update it so that exit is set
at up to 180 days after start (so long as individuals’ observation end
date is on or after this - if not, exit will be at observation period
end).

``` r
cdm$fractures <- cdm$fractures |> 
  padCohortEnd(days = 180)
```

We can see that our starting cohorts, before we add any additional
restrictions, have the following associated settings, counts, and
attrition.

``` r
settings(cdm$fractures) |> glimpse()
#> Rows: 2
#> Columns: 4
#> $ cohort_definition_id <int> 1, 2
#> $ cohort_name          <chr> "forearm_fracture", "hip_fracture"
#> $ cdm_version          <chr> "5.3", "5.3"
#> $ vocabulary_version   <chr> "v5.0 18-JAN-19", "v5.0 18-JAN-19"
cohortCount(cdm$fractures) |> glimpse()
#> Rows: 2
#> Columns: 3
#> $ cohort_definition_id <int> 1, 2
#> $ number_records       <int> 569, 138
#> $ number_subjects      <int> 510, 132
attrition(cdm$fractures) |> glimpse()
#> Rows: 10
#> Columns: 7
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 2, 2, 2, 2, 2
#> $ number_records       <int> 569, 569, 569, 569, 569, 138, 138, 138, 138, 138
#> $ number_subjects      <int> 510, 510, 510, 510, 510, 132, 132, 132, 132, 132
#> $ reason_id            <int> 1, 2, 3, 4, 5, 1, 2, 3, 4, 5
#> $ reason               <chr> "Initial qualifying events", "Record start <= rec…
#> $ excluded_records     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#> $ excluded_subjects    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
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
#> # A tibble: 3 × 5
#>   cohort_definition_id cohort_name      cdm_version vocabulary_version   gap
#>                  <int> <chr>            <chr>       <chr>              <dbl>
#> 1                    1 forearm_fracture 5.3         v5.0 18-JAN-19        NA
#> 2                    2 hip_fracture     5.3         v5.0 18-JAN-19        NA
#> 3                    3 any_fracture     <NA>        <NA>                   0
cohortCount(cdm$fractures)
#> # A tibble: 3 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1            569             510
#> 2                    2            138             132
#> 3                    3            707             611
```

### Require in date range

Once we have created our base fracture cohort, we can then start
applying additional cohort requirements. For example, first we can
require that individuals’ cohort start date fall within a certain date
range.

``` r
cdm$fractures <- cdm$fractures |> 
  requireInDateRange(dateRange = as.Date(c("2000-01-01", "2020-01-01")))
```

Now that we’ve applied this date restriction, we can see that our cohort
attributes have been updated

``` r
cohortCount(cdm$fractures) |> glimpse()
#> Rows: 3
#> Columns: 3
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 152, 62, 214
#> $ number_subjects      <int> 143, 60, 196
attrition(cdm$fractures) |> 
  filter(reason == "cohort_start_date between 2000-01-01 & 2020-01-01") |> 
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
cdm$fractures <- cdm$fractures |> 
  requireDemographics(ageRange = list(c(40, 65)),
                      sex = "Female")
```

Again we can see how many individuals we’ve lost after applying these
criteria.

``` r
attrition(cdm$fractures) |> 
  filter(reason == "Age requirement: 40 to 65") |> 
  glimpse()
#> Rows: 3
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 64, 22, 86
#> $ number_subjects      <int> 62, 22, 83
#> $ reason_id            <int> 8, 8, 4
#> $ reason               <chr> "Age requirement: 40 to 65", "Age requirement: 40…
#> $ excluded_records     <int> 88, 40, 128
#> $ excluded_subjects    <int> 81, 38, 113

attrition(cdm$fractures) |> 
  filter(reason == "Sex requirement: Female") |> 
  glimpse()
#> Rows: 3
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 37, 12, 49
#> $ number_subjects      <int> 36, 12, 48
#> $ reason_id            <int> 9, 9, 5
#> $ reason               <chr> "Sex requirement: Female", "Sex requirement: Fema…
#> $ excluded_records     <int> 27, 10, 37
#> $ excluded_subjects    <int> 26, 10, 35
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

cdm$fractures <- cdm$fractures |> 
  requireCohortIntersect(targetCohortTable = "gibleed",
                         intersections = 0,
                         window = c(-Inf, 0))
```

``` r
attrition(cdm$fractures) |> 
  filter(reason == "Not in cohort gibleed between -Inf & 0 days relative to cohort_start_date") |> 
  glimpse()
#> Rows: 3
#> Columns: 7
#> $ cohort_definition_id <int> 1, 2, 3
#> $ number_records       <int> 30, 10, 40
#> $ number_subjects      <int> 30, 10, 40
#> $ reason_id            <int> 12, 12, 8
#> $ reason               <chr> "Not in cohort gibleed between -Inf & 0 days rela…
#> $ excluded_records     <int> 7, 2, 9
#> $ excluded_subjects    <int> 6, 2, 8
```

``` r
cdmDisconnect(cdm)
```

### More information

CohortConstructor provides much more functionality for creating and
manipulating cohorts. See the package vignettes for more details.
