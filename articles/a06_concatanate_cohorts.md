# Concatenating cohort records

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

Let’s start by creating a cohort of users of acetaminophen

``` r
cdm$medications <- conceptCohort(cdm = cdm, 
                                 conceptSet = list("acetaminophen" = 1127433), 
                                 name = "medications")
cohortCount(cdm$medications)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1           9365            2580
```

We can merge cohort records using the
[`collapseCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/collapseCohorts.md)
function in the CohortConstructor package. The function allows us to
specifying the number of days between two cohort entries, which will
then be merged into a single record.

Let’s first define a new cohort where records within 1095 days (~ 3
years) of each other will be merged.

``` r
cdm$medications_collapsed <- cdm$medications |> 
  collapseCohorts(
  gap = 1095,
  name = "medications_collapsed"
)
```

Let’s compare how this function would change the records of a single
individual.

``` r
cdm$medications |>
  filter(subject_id == 1)
#> # Source:   SQL [?? x 4]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/Rtmp8YbmMf/file278575ffedf7.duckdb]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>         
#> 1                    1          1 1980-03-15        1980-03-29     
#> 2                    1          1 1971-01-04        1971-01-18     
#> 3                    1          1 1982-09-11        1982-10-02     
#> 4                    1          1 1976-10-20        1976-11-03
cdm$medications_collapsed |>
  filter(subject_id == 1)
#> # Source:   SQL [?? x 4]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/Rtmp8YbmMf/file278575ffedf7.duckdb]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>         
#> 1                    1          1 1976-10-20        1976-11-03     
#> 2                    1          1 1980-03-15        1982-10-02     
#> 3                    1          1 1971-01-04        1971-01-18
```

Subject 1 initially had 4 records between 1971 and 1982. After
specifying that records within three years of each other are to be
merged, the number of records decreases to three. The record from
1980-03-15 to 1980-03-29 and the record from 1982-09-11 to 1982-10-02
are merged to create a new record from 1980-03-15 to 1982-10-02.

Now let’s look at how the cohorts have been changed.

``` r
summary_attrition <- summariseCohortAttrition(cdm$medications_collapsed)
tableCohortAttrition(summary_attrition)
```

[TABLE]
