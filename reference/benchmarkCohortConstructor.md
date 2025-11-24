# Run benchmark of CohortConstructor package

Run benchmark of CohortConstructor cohort instantiation time compared to
CIRCE from JSON. More information in the benchmarking vignette.

## Usage

``` r
benchmarkCohortConstructor(
  cdm,
  runCIRCE = TRUE,
  runCohortConstructorDefinition = TRUE,
  runCohortConstructorDomain = TRUE,
  dropCohorts = TRUE
)
```

## Arguments

- cdm:

  A cdm reference.

- runCIRCE:

  Whether to run cohorts from JSON definitions generated with Atlas.

- runCohortConstructorDefinition:

  Whether to run the benchmark part where cohorts are created with
  CohortConstructor by definition (one by one, separately).

- runCohortConstructorDomain:

  Whether to run the benchmark part where cohorts are created with
  CohortConstructor by domain (instantianting base cohort all together,
  as a set).

- dropCohorts:

  Whether to drop cohorts created during benchmark.
