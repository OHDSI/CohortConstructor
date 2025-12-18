# Filtering cohorts

``` r
library(omock)
library(dplyr)
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

We can take a sample from a cohort table using the function
`sampleCohort()`. This allows us to specify the number of individuals in
each cohort.

``` r
cdm$medications |> sampleCohorts(cohortId = NULL, n = 100)
#> # Source:   table<results.test_medications> [?? x 4]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/Rtmp33mV8r/file276f470d73ee.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1       2358 1984-07-29        1984-08-19     
#>  2                    1       3580 1986-03-13        1986-03-27     
#>  3                    2       4269 2007-06-11        2007-06-11     
#>  4                    1       5137 1970-11-04        1970-11-18     
#>  5                    1       1135 1981-01-01        1981-01-15     
#>  6                    1       1864 1971-04-08        1971-04-22     
#>  7                    1        743 1991-11-08        1991-12-06     
#>  8                    1       2428 2008-02-28        2008-03-06     
#>  9                    1       2540 1972-12-21        1973-01-04     
#> 10                    1       2540 1991-11-30        1991-12-07     
#> # ℹ more rows

cohortCount(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1            352             100
#> 2                    2            100             100
```

When cohortId = NULL all cohorts in the table are used. Note that this
function does not reduced the number of records in each cohort, only the
number of individuals.

It is also possible to only sample one cohort within cohort table,
however the remaining cohorts will still remain.

``` r
cdm$medications <- cdm$medications |> sampleCohorts(cohortId = 2, n = 100)

cohortCount(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1           9365            2580
#> 2                    2            100             100
```

The chosen cohort (users of diclofenac) has been reduced to 100
individuals, as specified in the function, however all individuals from
cohort 1 (users of acetaminophen) and their records remain.

If you want to filter the cohort table to only include individuals and
records from a specified cohort, you can use the function
`subsetCohorts`.

``` r
cdm$medications <- cdm$medications |> subsetCohorts(cohortId = 2)
cohortCount(cdm$medications)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    2            830             830
```

The cohort table has been filtered so it now only includes individuals
and records from cohort 2. If you want to take a sample of the filtered
cohort table then you can use the `sampleCohorts` function.

``` r
cdm$medications <- cdm$medications |> sampleCohorts(cohortId = 2, n = 100)

cohortCount(cdm$medications)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    2            100             100
```
