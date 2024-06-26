% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requireTableIntersectFlag.R
\name{requireTableIntersect}
\alias{requireTableIntersect}
\title{Require cohort subjects are present in another clinical table}
\usage{
requireTableIntersect(
  cohort,
  tableName,
  window,
  intersections = c(1, Inf),
  cohortId = NULL,
  indexDate = "cohort_start_date",
  targetStartDate = startDateColumn(tableName),
  targetEndDate = endDateColumn(tableName),
  censorDate = NULL,
  name = tableName(cohort)
)
}
\arguments{
\item{cohort}{A cohort table in a cdm reference.}

\item{tableName}{Name of the table to check for intersect.}

\item{window}{Window to consider events over.}

\item{intersections}{A range indicating number of intersections for
criteria to be fulfilled. If a single number is passed, the number of
intersections must match this.}

\item{cohortId}{IDs of the cohorts to modify. If NULL, all cohorts will be
used; otherwise, only the specified cohorts will be modified, and the
rest will remain unchanged.}

\item{indexDate}{Variable in x that contains the date to compute the
intersection.}

\item{targetStartDate}{Date of reference in cohort table, either for start
(in overlap) or on its own (for incidence).}

\item{targetEndDate}{Date of reference in cohort table, either for end
(overlap) or NULL (if incidence).}

\item{censorDate}{Whether to censor overlap events at a specific date or a
column date of x.}

\item{name}{Name of the new cohort with the future observation restriction.}
}
\value{
Cohort table with only those in the other table kept (or those that
are not in the table if negate = TRUE)
}
\description{
\code{requireTableIntersect()} filters a cohort table based on a requirement
that an individual is seen (or not seen) to have a record (or no records) in
a clinical table in some time window around an index date.
}
\examples{
\donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor(drugExposure = TRUE)
cdm$cohort1 |>
  requireTableIntersect(tableName = "drug_exposure",
                            indexDate = "cohort_start_date",
                            window = c(-Inf, 0))
}
}
