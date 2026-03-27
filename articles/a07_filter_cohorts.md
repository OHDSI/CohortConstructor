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
#> # Database: DuckDB 1.5.0 [unknown@Linux 6.17.0-1008-azure:R 4.5.3//tmp/Rtmpx33sHg/file27aa668592b2.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    2       3856 2012-02-05        2012-02-05     
#>  2                    1       1107 1930-04-03        1930-04-10     
#>  3                    2       4602 2011-05-21        2011-05-21     
#>  4                    1       3080 1959-11-05        1959-11-19     
#>  5                    1       3827 1967-08-03        1967-08-17     
#>  6                    1       2525 2009-12-26        2010-01-09     
#>  7                    2       1497 1974-07-25        1974-07-25     
#>  8                    1        624 1957-01-30        1957-03-31     
#>  9                    1       1497 2007-05-01        2007-05-15     
#> 10                    1       4017 1984-03-04        1984-03-18     
#> # ℹ more rows

cohortCount(cdm$medications)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1            358             100
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
