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
#> # Database: DuckDB 1.4.4 [unknown@Linux 6.14.0-1017-azure:R 4.5.2//tmp/Rtmp1qHR4O/file25f54181c87e.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1       2394 1956-04-19        1956-04-26     
#>  2                    1       2394 1979-10-20        1979-12-19     
#>  3                    1       2461 2006-01-19        2006-02-18     
#>  4                    1       2461 2010-04-04        2010-04-11     
#>  5                    2       3133 1989-06-03        1989-06-03     
#>  6                    1         97 1966-04-21        1966-04-28     
#>  7                    1       2480 2010-08-29        2010-09-23     
#>  8                    1        394 1953-03-10        1953-03-24     
#>  9                    1        877 2005-10-22        2005-11-05     
#> 10                    1        877 2014-07-22        2014-07-29     
#> # ℹ more rows

cohortCount(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1            365             100
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
