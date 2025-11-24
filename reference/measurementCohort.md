# Create measurement-based cohorts

`measurementCohort()` creates cohorts based on patient records from the
measurement or observation tables. It extends the function
[`conceptCohort()`](https://ohdsi.github.io/CohortConstructor/reference/conceptCohort.md)
by allowing users to specify measurement values associated with those
records.

This function supports both concept-based and value-based filtering:

- Either `valueAsConcept` or `valueAsNumber` must be provided.

- If one of them is specified (not NULL), only records that satisfy the
  other filter will be included.

- If both are provided, records that meet *either* filter will be
  included.

## Usage

``` r
measurementCohort(
  cdm,
  conceptSet,
  name,
  valueAsConcept = NULL,
  valueAsNumber = NULL,
  table = NULL,
  useRecordsBeforeObservation = FALSE,
  useSourceFields = FALSE,
  subsetCohort = NULL,
  subsetCohortId = NULL
)
```

## Arguments

- cdm:

  A cdm reference.

- conceptSet:

  A conceptSet, which can either be a codelist or a
  conceptSetExpression.

- name:

  Name of the new cohort table created in the cdm object.

- valueAsConcept:

  A named list defining cohorts based on measurement values as concept
  IDs. Each element name defines the name of cohort to create, and its
  value is a vector of concept IDs used to filter measurements by
  `value_as_concept_id.` If NULL, all records will be included
  regardless of `value_as_concept_id.`

  For instance, to create two bmi cohorts from a bmi `conceptSet` we can
  do the following:
  `valueAsConcept = list(high_bmi = c(4328749, 35819253), low_bmi = c(4267416, 45881666))`

  See more examples in the function examples.

- valueAsNumber:

  A named list defining cohorts based on numeric measurement ranges.
  Each list element should contain one or more numeric vectors of length
  two, specifying the allowed range(s) for the measurement value. If the
  numeric vector is named, the name should correspond to the
  `unit_concept_id` that will be used for that range.

  For example, the following creates a cohort named "low_weight" based
  on measurements recorded in kilograms (unit_concept_id = 9529) and
  stones (unit_concept_id = 21498905):
  `valueAsNumber = list("low_weight" = list("9529" = c(30, 40), "21498905" = c(4.7, 6.3))) `

  See the examples below for how to define multiple cohorts based on
  different measurement filters.

- table:

  Character vector specifying which OMOP tables to use. Accepts
  "measurement", "observation", or both.

- useRecordsBeforeObservation:

  If FALSE, only records in observation will be used. If TRUE, records
  before the start of observation period will be considered, with cohort
  start date set as the start date of the individuals next observation
  period (as cohort records must be within observation).

- useSourceFields:

  If TRUE, the source concept_id fields will also be used when
  identifying relevant clinical records. If FALSE, only the standard
  concept_id fields will be used.

- subsetCohort:

  A character refering to a cohort table containing individuals for whom
  cohorts will be generated. Only individuals in this table will appear
  in the generated cohort.

- subsetCohortId:

  Optional. Specifies cohort IDs from the `subsetCohort` table to
  include. If none are provided, all cohorts from the `subsetCohort` are
  included.

## Value

A cohort table.

## Examples

``` r
# \donttest{
library(CohortConstructor)
library(omock)
library(dplyr)

cdm <- mockVocabularyTables(concept = tibble(
  concept_id = c(4326744, 4298393, 45770407, 8876, 4124457),
  concept_name = c("Blood pressure", "Systemic blood pressure",
                   "Baseline blood pressure", "millimeter mercury column",
                   "Normal range"),
  domain_id = "Measurement",
  vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "UCUM", "SNOMED"),
  standard_concept = "S",
  concept_class_id = c("Observable Entity", "Observable Entity",
                       "Observable Entity", "Unit", "Qualifier Value"),
  concept_code = NA_character_,
  valid_start_date = as.Date(NA_character_),
  valid_end_date = as.Date(NA_character_),
  invalid_reason = NA_character_
)) |>
  mockCdmFromTables(tables = list(
    measurement = tibble(
      measurement_id = 1:4L,
      person_id = c(1L, 1L, 2L, 3L),
      measurement_concept_id = c(4326744L, 4298393L, 4298393L, 45770407L),
      measurement_date = as.Date(c("2000-07-01", "2000-12-11", "2002-09-08", "2015-02-19")),
      measurement_type_concept_id = 0L,
      value_as_number = c(100, 125, NA, NA),
      value_as_concept_id = c(0L, 0L, 0L, 4124457L),
      unit_concept_id = c(8876L, 8876L, 0L, 0L)
   )
 ))

# create one cohort of blood pressure measurements indicating normal levels
cdm$cohort <- measurementCohort(
  cdm = cdm,
  name = "cohort",
  conceptSet = list("blood_pressure" = c(4326744, 4298393, 45770407)),
  valueAsConcept = list("normal_blood_preassure" = c(4124457)),
  valueAsNumber = list("normal_blood_preassure" = list("8876" = c(70, 120))),
  useRecordsBeforeObservation = FALSE
)
#> Warning: ! `codelist` casted to integers.
#> ℹ Subsetting table measurement using 3 concepts with domain: measurement.
#> ℹ Combining tables.
#> ℹ Applying measurement requirements.
#> ℹ Getting records in observation.
#> ℹ Creating cohort attributes.
#> ✔ Cohort cohort created.

cdm$cohort
#> # A tibble: 2 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>         
#> 1                    1          1 2000-07-01        2000-07-01     
#> 2                    1          3 2015-02-19        2015-02-19     

# create two cohorts of blood preassure measurements, one with results
# indicating normal blood pressure and the other inidcating high

cdm$cohort2 <- measurementCohort(
  cdm = cdm,
  name = "cohort2",
  conceptSet = list("blood_pressure" = c(4326744, 4298393, 45770407)),
  valueAsConcept = list(
    "normal_blood_pressure" = 4124457,
    "high_blood_pressure" = 4328749
  ),
  valueAsNumber = list(
    "normal_blood_pressure" = list("8876" = c(70, 120)),
    "high_blood_pressure" = list("8876" = c(121, 200))
  ),
  useRecordsBeforeObservation = TRUE
)
#> Warning: ! `codelist` casted to integers.
#> ℹ Subsetting table measurement using 3 concepts with domain: measurement.
#> ℹ Combining tables.
#> ℹ Applying measurement requirements.
#> ℹ Getting records in observation.
#> Warning: There were 3 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `trim_record = &...`.
#> ℹ In group 1: `cohort_definition_id = 1`, `subject_id = 1`, `cohort_start_date
#>   = 2000-12-11`, `cohort_end_date = 2000-12-11`.
#> Caused by warning in `min()`:
#> ! no non-missing arguments to min; returning Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.
#> ℹ Creating cohort attributes.
#> ✔ Cohort cohort2 created.

cdm$cohort2 |> settings()
#> # A tibble: 2 × 4
#>   cohort_definition_id cohort_name           cdm_version vocabulary_version
#>                  <int> <chr>                 <chr>       <chr>             
#> 1                    1 high_blood_pressure   5.3         mock              
#> 2                    2 normal_blood_pressure 5.3         mock              

# }
```
