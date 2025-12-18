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
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpsQxmIo/file277d14719a2d.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date diclofenac
#>                   <int>      <int> <date>            <date>          <date>    
#>  1                    1         90 1978-08-26        2019-04-10      1997-06-02
#>  2                    1       1613 1959-12-17        2006-11-04      NA        
#>  3                    1        814 1995-12-21        2018-05-13      NA        
#>  4                    1       1110 1988-10-11        2019-01-14      NA        
#>  5                    1       3982 1940-08-03        2019-06-15      1977-11-21
#>  6                    1       2602 1948-11-30        2019-06-11      1986-11-14
#>  7                    1       3843 1978-02-21        2018-12-09      2008-06-21
#>  8                    1       4241 1977-01-01        2018-10-23      NA        
#>  9                    1       5223 1968-06-20        2019-01-26      NA        
#> 10                    1         35 1978-11-14        2018-12-25      NA        
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
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpsQxmIo/file277d14719a2d.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1        124 2017-06-12        2017-06-12     
#>  2                    1        155 2018-12-14        2018-12-14     
#>  3                    1        372 2019-06-29        2019-06-29     
#>  4                    1        552 2019-01-01        2019-01-01     
#>  5                    1        754 2019-06-20        2019-06-20     
#>  6                    1        774 2019-06-10        2019-06-10     
#>  7                    1        967 2019-02-07        2019-02-07     
#>  8                    1       1051 2019-05-05        2019-05-05     
#>  9                    1       1267 2019-02-14        2019-02-14     
#> 10                    1       1808 2019-06-05        2019-06-05     
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
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpsQxmIo/file277d14719a2d.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1         63 1955-12-15        1990-09-14     
#>  2                    1        572 1953-10-23        1961-10-12     
#>  3                    1        754 1937-08-24        1947-08-03     
#>  4                    1        967 1932-10-15        1939-01-14     
#>  5                    1        986 1916-10-28        1932-02-09     
#>  6                    1       1073 1936-06-25        1949-07-18     
#>  7                    1       1267 1952-05-08        1955-10-21     
#>  8                    1       1384 1982-03-17        2016-10-19     
#>  9                    1       1829 1967-07-04        1985-04-02     
#> 10                    1       2006 1946-01-18        1950-03-30     
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
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpsQxmIo/file277d14719a2d.duckdb]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1        148 1978-10-15        2018-10-21     
#>  2                    1        155 1954-05-07        2018-12-14     
#>  3                    1        316 1927-08-02        2019-04-23     
#>  4                    1        621 1912-08-21        2019-04-10     
#>  5                    1        729 1961-07-01        2019-03-30     
#>  6                    1        785 1963-12-09        2019-01-07     
#>  7                    1        829 1973-06-18        2018-10-08     
#>  8                    1        844 1966-04-23        2019-05-11     
#>  9                    1        986 1916-10-28        1996-08-31     
#> 10                    1       1073 1936-06-25        2019-04-18     
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
