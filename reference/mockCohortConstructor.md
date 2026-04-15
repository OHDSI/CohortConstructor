# Function to create a mock cdm reference for CohortConstructor

`mockCohortConstructor()` creates an example dataset that can be used
for demonstrating and testing the package

## Usage

``` r
mockCohortConstructor(source = "local", seed = 1)
```

## Arguments

- source:

  Source for the mock cdm, it can either be 'local' or 'duckdb'.

- seed:

  An optional integer used to set the seed for random number generation,
  ensuring reproducibility of the generated data. If provided, this seed
  allows the function to produce consistent results each time. If
  'NULL', the seed is not set, which can lead to different outputs on
  each run.

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
