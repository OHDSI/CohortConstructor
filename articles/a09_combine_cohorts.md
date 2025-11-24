# Combining Cohorts

``` r
library(omock)
library(CohortConstructor)
library(CohortCharacteristics)
library(ggplot2)
```

For this example we’ll use the Eunomia synthetic data from the
[omock](https://ohdsi.github.io/omock/) package.

``` r
cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")
```

Let’s start by creating two drug cohorts, one for users of diclofenac
and another for users of acetaminophen.

``` r
cdm$medications <- conceptCohort(cdm = cdm, 
                                 conceptSet = list("diclofenac" = 1124300,
                                                   "acetaminophen" = 1127433), 
                                 name = "medications")
cohortCount(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1           9365            2580
#> 2                    2            830             830
```

To check whether there is an overlap between records in both cohorts
using the function
[`intersectCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/intersectCohorts.md).

``` r
cdm$medintersect <- intersectCohorts(
  cohort = cdm$medications,
  name = "medintersect"
)

cohortCount(cdm$medintersect)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1              6               6
```

There are 6 individuals who had overlapping records in the diclofenac
and acetaminophen cohorts.

We can choose the number of days between cohort entries using the `gap`
argument.

``` r
cdm$medintersect <- intersectCohorts(
  cohort = cdm$medications,
  gap = 365,
  name = "medintersect"
)

cohortCount(cdm$medintersect)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1             94              94
```

There are 94 individuals who had overlapping records (within 365 days)
in the diclofenac and acetaminophen cohorts.

We can also combine different cohorts using the function
[`unionCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/unionCohorts.md).

``` r
cdm$medunion <- unionCohorts(
  cohort = cdm$medications,
  name = "medunion"
)

cohortCount(cdm$medunion)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1          10189            2605
```

We have now created a new cohort which includes individuals in either
the diclofenac cohort or the acetaminophen cohort.

You can keep the original cohorts in the new table if you use the
argument `keepOriginalCohorts = TRUE`.

``` r
cdm$medunion <- unionCohorts(
  cohort = cdm$medications,
  name = "medunion",
  keepOriginalCohorts = TRUE
)

cohortCount(cdm$medunion)
#> # A tibble: 3 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1           9365            2580
#> 2                    2            830             830
#> 3                    3          10189            2605
```

You can also choose the number of days between two subsequent cohort
entries to be merged using the `gap` argument.

``` r
cdm$medunion <- unionCohorts(
  cohort = cdm$medications,
  name = "medunion",
  gap = 365,
  keepOriginalCohorts = TRUE
)

cohortCount(cdm$medunion)
#> # A tibble: 3 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1           9365            2580
#> 2                    2            830             830
#> 3                    3           9682            2605
```
