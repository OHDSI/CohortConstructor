% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conceptCohort.R
\name{conceptCohort}
\alias{conceptCohort}
\title{Create cohorts based on a concept set}
\usage{
conceptCohort(
  cdm,
  conceptSet,
  name,
  exit = "event_end_date",
  useSourceFields = FALSE
)
}
\arguments{
\item{cdm}{A cdm reference.}

\item{conceptSet}{A conceptSet, which can either be a codelist
or a conceptSetExpression.}

\item{name}{Name of the new cohort table created in the cdm object.}

\item{exit}{How the cohort end date is defined. Can be either
"event_end_date" or "event_start_date".}

\item{useSourceFields}{If TRUE, the source concept_id fields will also be
used when identifying relevant clinical records. If FALSE, only the standard
concept_id fields will be used.}
}
\value{
A cohort table
}
\description{
\code{conceptCohort()} creates a cohort table from patient records
from the clinical tables in the OMOP CDM.

The following tables are currently supported for creating concept
cohorts:
\itemize{
\item condition_occurrence
\item device_exposure
\item drug_exposure
\item measurement
\item observation
\item procedure_occurrence
\item visit_occurrence
}

Cohort duration is based on record start and end (e.g.
condition_start_date and condition_end_date for records coming
from the condition_occurrence tables). So that the resulting table
satisfies the requirements of an OMOP CDM cohort table:
\itemize{
\item Overlapping records are collapsed into a single cohort entry.
\item If a record starts outside of an observation period it will be
silently ignored.
\item If a record ends outside of an observation period it will be
trimmed so as to end at the preceding observation period end date.
}
}
\examples{
\donttest{
library(CohortConstructor)

cdm <- mockCohortConstructor(conditionOccurrence = TRUE)

cohort <- conceptCohort(cdm = cdm, conceptSet = list(a = 1), name = "cohort")

cohort |> attrition()
}
}
