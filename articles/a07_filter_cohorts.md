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
#> # A query:  ?? x 4
#> # Database: DuckDB 1.5.4 [unknown@Linux 6.17.0-1018-azure:R 4.6.0//tmp/RtmpFls3un/file25f863f2e860.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    2       5328 1945-04-08        1945-04-08     
#>  2                    1       1586 1972-07-10        1972-07-24     
#>  3                    2       1032 1989-05-22        1989-05-22     
#>  4                    1       3672 1982-08-27        1982-09-10     
#>  5                    1       4324 1981-08-26        1981-09-02     
#>  6                    2        374 2015-07-17        2015-07-17     
#>  7                    2       1120 1997-03-07        1997-03-07     
#>  8                    1       1608 1957-09-10        1957-09-17     
#>  9                    1       2621 1966-11-03        1966-11-10     
#> 10                    1       4883 1983-03-24        1983-04-14     
#> # ℹ more rows

cohortCount(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1            388             100
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
