% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requireDemographics.R
\name{requireAge}
\alias{requireAge}
\title{Restrict cohort on age}
\usage{
requireAge(
  cohort,
  ageRange,
  cohortId = NULL,
  indexDate = "cohort_start_date",
  atFirst = FALSE,
  name = tableName(cohort),
  .softValidation = TRUE
)
}
\arguments{
\item{cohort}{A cohort table in a cdm reference.}

\item{ageRange}{A list of vectors specifying minimum and maximum age.}

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
age requirement
}
\description{
\code{requireAge()} filters cohort records, keeping only records where individuals
satisfy the specified age criteria.
}
\examples{
\donttest{
library(CohortConstructor)
cdm <- mockCohortConstructor()
cdm$cohort1 |>
  requireAge(indexDate = "cohort_start_date",
             ageRange = list(c(18, 65)))
}
}
