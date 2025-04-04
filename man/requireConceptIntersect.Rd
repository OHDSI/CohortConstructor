% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requireConceptIntersect.R
\name{requireConceptIntersect}
\alias{requireConceptIntersect}
\title{Require cohort subjects to have (or not have) events of a concept list}
\usage{
requireConceptIntersect(
  cohort,
  conceptSet,
  window,
  intersections = c(1, Inf),
  cohortId = NULL,
  indexDate = "cohort_start_date",
  targetStartDate = "event_start_date",
  targetEndDate = "event_end_date",
  inObservation = TRUE,
  censorDate = NULL,
  name = tableName(cohort)
)
}
\arguments{
\item{cohort}{A cohort table in a cdm reference.}

\item{conceptSet}{A conceptSet, which can either be a codelist
or a conceptSetExpression.}

\item{window}{A list of vectors specifying minimum and maximum days from
\code{indexDate} to consider events over.}

\item{intersections}{A range indicating number of intersections for
criteria to be fulfilled. If a single number is passed, the number of
intersections must match this.}

\item{cohortId}{Vector identifying which cohorts to modify
(cohort_definition_id or cohort_name). If NULL, all cohorts will be
used; otherwise, only the specified cohorts will be modified, and the
rest will remain unchanged.}

\item{indexDate}{Name of the column in the cohort that contains the date to
compute the intersection.}

\item{targetStartDate}{Start date of reference in cohort table.}

\item{targetEndDate}{End date of reference in cohort table. If NULL,
incidence of target event in the window will be considered as intersection,
otherwise prevalence of that event will be used as intersection (overlap
between cohort and event).}

\item{inObservation}{If TRUE only records inside an observation period will
be considered.}

\item{censorDate}{Whether to censor overlap events at a specific date or a
column date of the cohort.}

\item{name}{Name of the new cohort table created in the cdm object.}
}
\value{
Cohort table with only those  with the events in the concept list
kept (or those without the event if negate = TRUE)
}
\description{
\code{requireConceptIntersect()} filters a cohort table based on a requirement
that an individual is seen (or not seen) to have events related to a concept
list in some time window around an index date.
}
\examples{
\donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor(conditionOccurrence = TRUE)
cdm$cohort2 <-  requireConceptIntersect(
  cohort = cdm$cohort1,
  conceptSet = list(a = 194152),
  window = c(-Inf, 0),
  name = "cohort2")
  }
}
