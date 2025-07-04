% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requireDemographics.R
\name{requirePriorObservation}
\alias{requirePriorObservation}
\title{Restrict cohort on prior observation}
\usage{
requirePriorObservation(
  cohort,
  minPriorObservation,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  atFirst = FALSE,
  name = tableName(cohort),
  .softValidation = TRUE
)
}
\arguments{
\item{cohort}{A cohort table in a cdm reference.}

\item{minPriorObservation}{A minimum number of continuous prior observation
days in the database.}

\item{cohortId}{Vector identifying which cohorts to modify
(cohort_definition_id or cohort_name). If NULL, all cohorts will be
used; otherwise, only the specified cohorts will be modified, and the
rest will remain unchanged.}

\item{indexDate}{Variable in cohort that contains the date to compute the
demographics characteristics on which to restrict on.}

\item{atFirst}{If FALSE the requirement will be applied to all records, if
TRUE, it will only be required for the first entry of each subject.}

\item{name}{Name of the new cohort table created in the cdm object.}

\item{.softValidation}{Whether to perform a soft validation of consistency.
If set to FALSE four additional checks will be performed: 1) a check that
cohort end date is not before cohort start date,  2) a check that there
are no missing values in required columns, 3) a check that cohort duration is
all within observation period, and 4) that there are no overlapping
cohort entries}
}
\value{
The cohort table with only records for individuals satisfying the
prior observation requirement
}
\description{
\code{requirePriorObservation()} filters cohort records, keeping only records
where individuals satisfy the specified prior observation criteria.
}
\examples{
\donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
cdm$cohort1 |>
  requirePriorObservation(indexDate = "cohort_start_date",
                          minPriorObservation = 365)
}
}
