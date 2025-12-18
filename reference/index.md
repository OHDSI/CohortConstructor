# Package index

### Build base cohorts

- [`conceptCohort()`](https://ohdsi.github.io/CohortConstructor/reference/conceptCohort.md)
  : Create cohorts based on a concept set
- [`deathCohort()`](https://ohdsi.github.io/CohortConstructor/reference/deathCohort.md)
  : Create cohort based on the death table
- [`demographicsCohort()`](https://ohdsi.github.io/CohortConstructor/reference/demographicsCohort.md)
  : Create cohorts based on patient demographics
- [`measurementCohort()`](https://ohdsi.github.io/CohortConstructor/reference/measurementCohort.md)
  : Create measurement-based cohorts

### Apply cohort table related requirements

- [`requireMinCohortCount()`](https://ohdsi.github.io/CohortConstructor/reference/requireMinCohortCount.md)
  : Filter cohorts to keep only records for those with a minimum amount
  of subjects
- [`requireInDateRange()`](https://ohdsi.github.io/CohortConstructor/reference/requireInDateRange.md)
  : Require that an index date is within a date range
- [`requireDuration()`](https://ohdsi.github.io/CohortConstructor/reference/requireDuration.md)
  : Require cohort entries last for a certain number of days
- [`requireIsFirstEntry()`](https://ohdsi.github.io/CohortConstructor/reference/requireIsFirstEntry.md)
  : Restrict cohort to first entry
- [`requireIsLastEntry()`](https://ohdsi.github.io/CohortConstructor/reference/requireIsLastEntry.md)
  : Restrict cohort to last entry per person
- [`requireIsEntry()`](https://ohdsi.github.io/CohortConstructor/reference/requireIsEntry.md)
  : Restrict cohort to specific entry

### Impose singular demographic requirements on existing cohorts

- [`requireAge()`](https://ohdsi.github.io/CohortConstructor/reference/requireAge.md)
  : Restrict cohort on age
- [`requireSex()`](https://ohdsi.github.io/CohortConstructor/reference/requireSex.md)
  : Restrict cohort on sex
- [`requirePriorObservation()`](https://ohdsi.github.io/CohortConstructor/reference/requirePriorObservation.md)
  : Restrict cohort on prior observation
- [`requireFutureObservation()`](https://ohdsi.github.io/CohortConstructor/reference/requireFutureObservation.md)
  : Restrict cohort on future observation

### Impose multiple demographic requirements on existing cohorts

- [`requireDemographics()`](https://ohdsi.github.io/CohortConstructor/reference/requireDemographics.md)
  : Restrict cohort on patient demographics

### Impose requirements of presence or absence in other cohorts, concept sets, or table

- [`requireCohortIntersect()`](https://ohdsi.github.io/CohortConstructor/reference/requireCohortIntersect.md)
  : Require cohort subjects are present (or absence) in another cohort
- [`requireConceptIntersect()`](https://ohdsi.github.io/CohortConstructor/reference/requireConceptIntersect.md)
  : Require cohort subjects to have (or not have) events of a concept
  list
- [`requireTableIntersect()`](https://ohdsi.github.io/CohortConstructor/reference/requireTableIntersect.md)
  : Require cohort subjects are present in another clinical table

### Update cohort start and end dates

- [`entryAtFirstDate()`](https://ohdsi.github.io/CohortConstructor/reference/entryAtFirstDate.md)
  : Update cohort start date to be the first date from of a set of
  column dates
- [`entryAtLastDate()`](https://ohdsi.github.io/CohortConstructor/reference/entryAtLastDate.md)
  : Set cohort start date to the last of a set of column dates
- [`exitAtDeath()`](https://ohdsi.github.io/CohortConstructor/reference/exitAtDeath.md)
  : Set cohort end date to death date
- [`exitAtFirstDate()`](https://ohdsi.github.io/CohortConstructor/reference/exitAtFirstDate.md)
  : Set cohort end date to the first of a set of column dates
- [`exitAtLastDate()`](https://ohdsi.github.io/CohortConstructor/reference/exitAtLastDate.md)
  : Set cohort end date to the last of a set of column dates
- [`exitAtObservationEnd()`](https://ohdsi.github.io/CohortConstructor/reference/exitAtObservationEnd.md)
  : Set cohort end date to end of observation
- [`padCohortDate()`](https://ohdsi.github.io/CohortConstructor/reference/padCohortDate.md)
  : Set cohort start or cohort end
- [`padCohortEnd()`](https://ohdsi.github.io/CohortConstructor/reference/padCohortEnd.md)
  : Add days to cohort end
- [`padCohortStart()`](https://ohdsi.github.io/CohortConstructor/reference/padCohortStart.md)
  : Add days to cohort start
- [`trimDemographics()`](https://ohdsi.github.io/CohortConstructor/reference/trimDemographics.md)
  : Trim cohort on patient demographics
- [`trimDuration()`](https://ohdsi.github.io/CohortConstructor/reference/trimDuration.md)
  : Trim cohort dates to be within a certain interval of days
- [`trimToDateRange()`](https://ohdsi.github.io/CohortConstructor/reference/trimToDateRange.md)
  : Trim cohort dates to be within a date range

### Concatanate cohort entries

- [`collapseCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/collapseCohorts.md)
  : Collapse cohort entries using a certain gap to concatenate records.

### Filter cohorts

- [`subsetCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/subsetCohorts.md)
  : Generate a cohort table keeping a subset of cohorts.
- [`sampleCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/sampleCohorts.md)
  : Sample a cohort table for a given number of individuals.

### Copy cohorts

- [`copyCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/copyCohorts.md)
  : Copy a cohort table

### Split cohorts

- [`yearCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/yearCohorts.md)
  : Generate a new cohort table restricting cohort entries to certain
  years
- [`stratifyCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/stratifyCohorts.md)
  : Create a new cohort table from stratifying an existing one
- [`timeWindowCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/timeWindowCohorts.md)
  : Split cohorts based on time-windows

### Combine cohorts

- [`intersectCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/intersectCohorts.md)
  : Generate a combination cohort set between the intersection of
  different cohorts.
- [`unionCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/unionCohorts.md)
  : Generate cohort from the union of different cohorts

### Match cohorts

- [`matchCohorts()`](https://ohdsi.github.io/CohortConstructor/reference/matchCohorts.md)
  : Generate a new cohort matched cohort

### Mock data

- [`mockCohortConstructor()`](https://ohdsi.github.io/CohortConstructor/reference/mockCohortConstructor.md)
  : Function to create a mock cdm reference for CohortConstructor

### Package benchmark

- [`benchmarkCohortConstructor()`](https://ohdsi.github.io/CohortConstructor/reference/benchmarkCohortConstructor.md)
  : Run benchmark of CohortConstructor package
- [`benchmarkData`](https://ohdsi.github.io/CohortConstructor/reference/benchmarkData.md)
  : Benchmarking results

### Utility functions

- [`renameCohort()`](https://ohdsi.github.io/CohortConstructor/reference/renameCohort.md)
  : Utility function to change the name of a cohort.
- [`addCohortTableIndex()`](https://ohdsi.github.io/CohortConstructor/reference/addCohortTableIndex.md)
  : Add an index to a cohort table
