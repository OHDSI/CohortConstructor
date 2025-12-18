# Updating cohort start and end dates

## Introduction

Accurately defining cohort entry and exit dates is crucial in
observational research to ensure the validity of study findings. The
`CohortConstructor` package provides several functions to adjust these
dates based on specific criteria, and this vignette demonstrates how to
use them.

Functions to update cohort dates can be categorized into four groups:

- **Exit at Specific Date Functions:** Adjust the cohort end date to
  predefined events (observation end and death date).

- **Cohort Entry or Exit Based on Other Date Columns:** Modify cohort
  start or end dates to the earliest or latests from a set of date
  columns.

- **Trim Dates Functions:** Restrict cohort entries based on demographic
  criteria or specific date ranges.

- **Pad Dates Functions:** Adjust cohort start or end dates by adding or
  subtracting a specified number of days.

We’ll explore each category in the following sections.

First, we’ll connect to the Eunomia synthetic data and create a mock
cohort of women in the database to use as example in the vignette.

``` r
library(dplyr, warn.conflicts = FALSE)
library(CohortConstructor)
library(CohortCharacteristics)
library(PatientProfiles)
library(omock)
library(clock)

cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")
#> ℹ Reading GiBleed tables.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.
#> ℹ Inserting <cdm_reference> into duckdb.

cdm$cohort <- demographicsCohort(cdm = cdm, name = "cohort", sex = "Female")
#> ℹ Building new trimmed cohort
#> Adding demographics information
#> Creating initial cohort
#> Trim sex
#> ✔ Cohort trimmed
```

## Exit at Specific Date

### `exitAtObservationEnd()`

The
[`exitAtObservationEnd()`](https://ohdsi.github.io/CohortConstructor/reference/exitAtObservationEnd.md)
function updates the cohort end date to the end of the observation
period for each subject. This ensures that the cohort exit does not
extend beyond the period during which data is available for the subject.

``` r
cdm$cohort_observation_end <- cdm$cohort |> 
  exitAtObservationEnd(name = "cohort_observation_end")
```

As cohort entries cannot overlap, updating the end date to the
observation end may result in overlapping records. In such cases,
overlapping records are collapsed into a single entry (starting at the
earliest entry and ending at the end of observation).

This function has an argument `persistAcrossObservationPeriods` to
consider cases when a subject may have more than one observation period.
If `persistAcrossObservationPeriods = FALSE` then cohort end date will
be set to the end of the observation period where the record occurs. If
`persistAcrossObservationPeriods = TRUE`, in addition to updating the
cohort end to the current observation end, cohort entries are created
for each of the subsequent observation periods.

### `exitAtDeath()`

The
[`exitAtDeath()`](https://ohdsi.github.io/CohortConstructor/reference/exitAtDeath.md)
function sets the cohort end date to the recorded death date of the
subject.

By default, it keeps the end date of subjects who do not have a death
record unmodified; however, these can be dropped with the argument
`requireDeath.`

``` r
cdm$cohort_death <- cdm$cohort |> 
  exitAtDeath(requireDeath = TRUE, name = "cohort_death")
```

## Cohort Entry or Exit Based on Other Date Columns

### `entryAtFirstDate()`

The
[`entryAtFirstDate()`](https://ohdsi.github.io/CohortConstructor/reference/entryAtFirstDate.md)
function updates the cohort start date to the earliest date among
specified columns.

Next we want to set the entry date to the first of: diclofenac or
acetaminophen prescriptions after cohort start, or cohort end date.

``` r
# create cohort with of drugs diclofenac and acetaminophen 
cdm$medications <- conceptCohort(
  cdm = cdm, name = "medications",
  conceptSet = list("diclofenac" = 1124300, "acetaminophen" = 1127433)
)
#> Warning: ! `codelist` casted to integers.
#> ℹ Subsetting table drug_exposure using 2 concepts with domain: drug.
#> ℹ Combining tables.
#> ℹ Creating cohort attributes.
#> ℹ Applying cohort requirements.
#> ℹ Merging overlapping records.
#> ✔ Cohort medications created.

# add date first ocurrence of these drugs from index date
cdm$cohort_dates <- cdm$cohort |> 
  addCohortIntersectDate(
    targetCohortTable = "medications", 
    nameStyle = "{cohort_name}",
    name = "cohort_dates"
    ) 

# set cohort start at the first ocurrence of one of the drugs, or the end date
cdm$cohort_entry_first <- cdm$cohort_dates |>
  entryAtFirstDate(
    dateColumns = c("diclofenac", "acetaminophen", "cohort_end_date"), 
    name = "cohort_entry_first"
  )
#> Joining with `by = join_by(cohort_definition_id, subject_id, cohort_end_date)`
cdm$cohort_entry_first 
#> # Source:   table<results.test_cohort_entry_first> [?? x 6]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpjdlHrJ/file26e21ee13431.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date diclofenac
#>                   <int>      <int> <date>            <date>          <date>    
#>  1                    1        110 2001-04-06        2019-02-24      NA        
#>  2                    1       2060 1964-02-04        2019-01-02      NA        
#>  3                    1       1724 1950-05-12        2019-02-01      1992-11-15
#>  4                    1       2267 1979-07-05        2018-12-31      NA        
#>  5                    1       3789 1949-05-24        2018-08-30      NA        
#>  6                    1       1599 1972-11-19        2019-07-01      NA        
#>  7                    1       2859 1944-01-08        2018-11-13      NA        
#>  8                    1       3885 1978-07-27        2018-10-12      2004-10-26
#>  9                    1       3392 1968-04-25        2018-07-15      2002-02-10
#> 10                    1       2326 1963-01-07        2019-05-23      1994-01-13
#> # ℹ more rows
#> # ℹ 1 more variable: acetaminophen <date>
```

### `entryAtLastDate()`

The
[`entryAtLastDate()`](https://ohdsi.github.io/CohortConstructor/reference/entryAtLastDate.md)
function works similarly to
[`entryAtFirstDate()`](https://ohdsi.github.io/CohortConstructor/reference/entryAtFirstDate.md),
however now the selected column is the latest date among specified
columns.

``` r
cdm$cohort_entry_last <- cdm$cohort_dates |>
  entryAtLastDate(
    dateColumns = c("diclofenac", "acetaminophen", "cohort_end_date"), 
    keepDateColumns = FALSE,
    name = "cohort_entry_last"
  )

cdm$cohort_entry_last
#> # Source:   table<results.test_cohort_entry_last> [?? x 4]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpjdlHrJ/file26e21ee13431.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1        186 2019-03-25        2019-03-25     
#>  2                    1        534 2018-06-26        2018-06-26     
#>  3                    1        729 2019-03-30        2019-03-30     
#>  4                    1        785 2019-01-07        2019-01-07     
#>  5                    1        829 2018-10-08        2018-10-08     
#>  6                    1       1642 2018-10-23        2018-10-23     
#>  7                    1       1736 2019-05-10        2019-05-10     
#>  8                    1       2004 2019-06-25        2019-06-25     
#>  9                    1       2423 2019-06-03        2019-06-03     
#> 10                    1       2943 2018-06-25        2018-06-25     
#> # ℹ more rows
```

In this example, we set `keepDateColumns` to FALSE, which drops columns
in `dateColumns`.

### `exitAtFirstDate()`

The
[`exitAtFirstDate()`](https://ohdsi.github.io/CohortConstructor/reference/exitAtFirstDate.md)
function updates the cohort end date to the earliest date among
specified columns.

For instance, next we want the exit to be observation end, except if
there is a record of diclofenac or acetaminophen, in which case that
would be the end:

``` r
cdm$cohort_exit_first <- cdm$cohort_dates |>
  addFutureObservation(futureObservationType = "date", name = "cohort_exit_first") |>
  exitAtFirstDate(
    dateColumns = c("future_observation", "acetaminophen", "diclofenac"),
    keepDateColumns = FALSE
  )

cdm$cohort_exit_first 
#> # Source:   table<results.test_cohort_exit_first> [?? x 4]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpjdlHrJ/file26e21ee13431.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1         36 1958-10-21        1966-07-12     
#>  2                    1        763 1965-03-18        2000-11-17     
#>  3                    1        847 1966-04-09        1985-03-20     
#>  4                    1       1000 1963-02-12        1970-09-04     
#>  5                    1       1335 1978-11-20        1981-07-08     
#>  6                    1       1929 1954-08-19        1975-09-19     
#>  7                    1       1950 1940-09-13        1956-06-20     
#>  8                    1       2013 1956-03-17        1964-05-06     
#>  9                    1       2175 1976-01-29        1992-02-06     
#> 10                    1       2270 1978-05-31        2018-06-06     
#> # ℹ more rows
```

### `exitAtLastDate()`

Similarly, the
[`exitAtLastDate()`](https://ohdsi.github.io/CohortConstructor/reference/exitAtLastDate.md)
function sets the cohort end date to the latest date among specified
columns.

``` r
cdm$cohort_exit_last <- cdm$cohort_dates |> 
  exitAtLastDate(
    dateColumns = c("cohort_end_date", "acetaminophen", "diclofenac"),
    returnReason = FALSE,
    keepDateColumns = FALSE,
    name = "cohort_exit_last"
  )
cdm$cohort_exit_last
#> # Source:   table<results.test_cohort_exit_last> [?? x 4]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpjdlHrJ/file26e21ee13431.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1        144 1974-12-20        2019-01-04     
#>  2                    1        165 1968-06-12        2018-09-01     
#>  3                    1        351 1977-11-29        2018-06-29     
#>  4                    1        458 1972-08-13        2017-07-23     
#>  5                    1        743 1975-11-02        2017-10-08     
#>  6                    1        795 1936-08-19        2018-12-17     
#>  7                    1        857 1952-03-03        2016-05-30     
#>  8                    1       1169 1935-10-14        2018-08-27     
#>  9                    1       1352 1946-08-04        2019-02-03     
#> 10                    1       1438 1952-01-18        2019-05-17     
#> # ℹ more rows
```

In this last example, the return cohort doesn’t have the specified date
columns, neither the “reason” column indicating which date was used for
entry/exit. These was set with the `keepDateColumns` and `returnReason`
arguments, common throughout the functions in this category.

## Trim Dates Functions

### `trimDemographics()`

The
[`trimDemographics()`](https://ohdsi.github.io/CohortConstructor/reference/trimDemographics.md)
function restricts the cohort based on patient demographics. This means
that cohort start and end dates are moved (within the original cohort
entry dates) to ensure that individuals meet specific demographic
criteria throughout their cohort participation. If individuals do not
satisfy the criteria at any point during their cohort period, their
records are excluded.

For instance, if we trim using an age range from 18 to 65, individuals
will only contribute in the cohort form the day they are 18 or older, up
to the day before turning 66 (or before if they leave the database).

``` r
cdm$cohort_trim <- cdm$cohort |>
  trimDemographics(ageRange = c(18, 65), name = "cohort_trim")
#> ℹ Building new trimmed cohort
#> Adding demographics information
#> Creating initial cohort
#> Trim age
#> ✔ Cohort trimmed
```

### `trimToDateRange()`

The
[`trimToDateRange()`](https://ohdsi.github.io/CohortConstructor/reference/trimToDateRange.md)
function confines cohort entry and exit dates within a specified date
range, ensuring that cohort periods align with the defined timeframe. If
only the start or end of a range is required, the other can be set to
`NA`.

For example, to restrict cohort dates to be on or after January 1st,
2015:

``` r
# Trim cohort dates to be within the year 2000
cdm$cohort_trim <- cdm$cohort_trim |> 
  trimToDateRange(dateRange = as.Date(c("2015-01-01", NA)))
```

## Pad Dates Functions

### `padCohortStart()`

The
[`padCohortStart()`](https://ohdsi.github.io/CohortConstructor/reference/padCohortStart.md)
function adds (or subtracts) a specified number of days to the cohort
start date.

For example, to subtract 50 days from the cohort start date:

``` r
# Substract 50 days to cohort start
cdm$cohort <- cdm$cohort |> padCohortStart(days = -50, collapse = FALSE)
```

When subtracting days, it may result in cohort start dates preceding the
observation period start. By default, such entries are corrected to the
observation period start. To drop these entries instead, set the
`requireFullContribution` argument to TRUE

Additionally, adjusting cohort start dates may lead to overlapping
entries for the same subject. The `collapse` argument manages this: if
TRUE, merges overlapping entries into a single record with the earliest
start and latest end date (default), if FALSE retains only the first of
the overlapping entries.

### `padCohortEnd()`

Similarly, the
[`padCohortEnd()`](https://ohdsi.github.io/CohortConstructor/reference/padCohortEnd.md)
function adjusts the cohort end date by adding (or subtracting) a
specified number of days.

The example below adds 1000 days to cohort end date, while dropping
records that are outside of observation after adding days.

``` r
cdm$cohort_pad <- cdm$cohort |> 
  padCohortEnd(days = 1000, requireFullContribution = TRUE, name = "cohort_pad")
```

Additionally, days to add can also be specified with a numeric column in
the cohort, which allows to add a specific number of days for each
record:

``` r
cdm$cohort <- cdm$cohort |> 
  dplyr::mutate(days_to_add = date_count_between(start = cohort_start_date, end = cohort_end_date, precision = "day")) |>
  padCohortEnd(days = "days_to_add", requireFullContribution = TRUE)
```

### `padCohortDate()`

The
[`padCohortDate()`](https://ohdsi.github.io/CohortConstructor/reference/padCohortDate.md)
function provides a more flexible approach by allowing adjustments to
either the cohort start or end date based on specified parameters. You
can define which date to adjust (`cohortDate`), the reference date for
the adjustment (`indexDate`), and the number of days to add or subtract.

For example, to set the cohort end date to be 365 days after the cohort
start date:

``` r
cdm$cohort <- cdm$cohort |> 
  padCohortDate(days = 365, cohortDate = "cohort_end_date", indexDate = "cohort_start_date")
```

## Cohort ID argument

For all these functions, the cohortId argument specifies which cohorts
to modify. This allows for targeted adjustments without altering other
cohorts. For instance, to add 10 days to the end date of the
acetaminophen cohort and 20 days to the diclofenac cohort we can do the
following:

``` r
cdm$medications <- cdm$medications |> 
  padCohortDate(days = 10, cohortId = "acetaminophen") |> 
  padCohortDate(days = 20, cohortId = "diclofenac")
```
