# Function to create a mock cdm reference for CohortConstructor

`mockCohortConstructor()` creates an example dataset that can be used
for demonstrating and testing the package

## Usage

``` r
mockCohortConstructor(source = "local")
```

## Arguments

- source:

  Source for the mock cdm, it can either be 'local' or 'duckdb'.

## Value

cdm object

## Examples

``` r
# \donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
#> ℹ Reading GiBleed tables.

cdm
#> 
#> ── # OMOP CDM reference (local) of mock database ───────────────────────────────
#> • omop tables: cdm_source, concept, concept_ancestor, concept_relationship,
#> concept_synonym, condition_occurrence, death, drug_exposure, drug_strength,
#> measurement, observation, observation_period, person, vocabulary
#> • cohort tables: cohort1, cohort2
#> • achilles tables: -
#> • other tables: -
# }
```
