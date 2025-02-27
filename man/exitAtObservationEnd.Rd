% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exitAtDate.R
\name{exitAtObservationEnd}
\alias{exitAtObservationEnd}
\title{Set cohort end date to end of observation}
\usage{
exitAtObservationEnd(
  cohort,
  cohortId = NULL,
  limitToCurrentPeriod = TRUE,
  name = tableName(cohort)
)
}
\arguments{
\item{cohort}{A cohort table in a cdm reference.}

\item{cohortId}{Vector identifying which cohorts to modify
(cohort_definition_id or cohort_name). If NULL, all cohorts will be
used; otherwise, only the specified cohorts will be modified, and the
rest will remain unchanged.}

\item{limitToCurrentPeriod}{If TRUE, limits the cohort to one entry per
person, ending at the current observation period. If FALSE, subsequent
observation periods will create new cohort entries.}

\item{name}{Name of the new cohort table created in the cdm object.}
}
\value{
The cohort table.
}
\description{
\code{exitAtObservationEnd()} resets cohort end date based on a set of specified
column dates. The last date that occurs is chosen.

This functions changes cohort end date to the end date of the observation
period corresponding to the cohort entry. In the case were this generates
overlapping records in the cohort, overlapping entries will be merged.
}
\examples{
\donttest{
library(CohortConstructor)

cdm <- mockCohortConstructor()
cdm$cohort1 |> exitAtObservationEnd()
}
}
