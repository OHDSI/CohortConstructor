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
#> # Database: DuckDB 1.4.2 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpK5onQr/file24bf91dbc53.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1       1091 1989-02-04        1989-02-18     
#>  2                    1       1605 1992-08-20        1992-08-27     
#>  3                    1       3568 1986-06-21        1986-07-05     
#>  4                    1       5343 1985-01-23        1985-01-30     
#>  5                    2       2747 2009-11-14        2009-11-14     
#>  6                    2       4081 1999-04-21        1999-04-21     
#>  7                    2       4837 1981-11-03        1981-11-03     
#>  8                    2       4497 1992-11-05        1992-11-05     
#>  9                    1       1250 1997-10-03        1997-10-17     
#> 10                    1       3860 1985-12-30        1986-02-03     
#> # ℹ more rows

cohortCount(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1            385             100
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
