# Helper for consistent documentation of arguments in `requireIntersect` functions.

Helper for consistent documentation of arguments in `requireIntersect`
functions.

## Arguments

- indexDate:

  Name of the column in the cohort that contains the date to compute the
  intersection.

- intersections:

  A range indicating number of intersections for criteria to be
  fulfilled. If a single number is passed, the number of intersections
  must match this.

- targetStartDate:

  Start date of reference in cohort table.

- targetEndDate:

  End date of reference in cohort table. If NULL, incidence of target
  event in the window will be considered as intersection, otherwise
  prevalence of that event will be used as intersection (overlap between
  cohort and event).

- censorDate:

  Whether to censor overlap events at a specific date or a column date
  of the cohort.

- targetCohortTable:

  Name of the cohort that we want to check for intersect.

- targetCohortId:

  Vector of cohort definition ids to include.

- tableName:

  Name of the table to check for intersect.

- inObservation:

  If TRUE only records inside an observation period will be considered
