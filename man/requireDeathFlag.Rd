% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requireDeathFlag.R
\name{requireDeathFlag}
\alias{requireDeathFlag}
\title{Require cohort subjects have (or do not have) a death record}
\usage{
requireDeathFlag(
  cohort,
  window,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  censorDate = NULL,
  negate = FALSE,
  name = tableName(cohort)
)
}
\arguments{
\item{cohort}{A cohort table in a cdm reference.}

\item{window}{A list of vectors specifying minimum and maximum days from
\code{indexDate} to consider events over.}

\item{cohortId}{Vector identifying which cohorts to modify
(cohort_definition_id or cohort_name). If NULL, all cohorts will be
used; otherwise, only the specified cohorts will be modified, and the
rest will remain unchanged.}

\item{indexDate}{Name of the column in the cohort that contains the date to
use as time 0 for window days.}

\item{censorDate}{Whether to censor overlap events at a specific date or a
column date of the cohort.}

\item{negate}{If set as TRUE, criteria will be applied as exclusion
rather than inclusion (i.e. require absence in another cohort).}

\item{name}{Name of the new cohort table created in the cdm object.}
}
\value{
Cohort table with only those with a death event kept (or without
if negate = TRUE)
}
\description{
\code{requireDeathFlag()} filters a cohort table based on a requirement
that an individual is seen (or not seen) to have a death in some time
window around an index date.
}
\examples{
\donttest{
library(CDMConnector)
library(CohortConstructor)
cdm <- mockCohortConstructor(death = TRUE)
cdm$cohort1 <- cdm$cohort1 |> requireDeathFlag(window = list(c(0, Inf)))
attrition(cdm$cohort1)
}
}
