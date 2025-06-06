% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addIndex.R
\name{addCohortTableIndex}
\alias{addCohortTableIndex}
\title{Add an index to a cohort table}
\usage{
addCohortTableIndex(cohort)
}
\arguments{
\item{cohort}{A cohort table in a cdm reference.}
}
\value{
The cohort table
}
\description{
Adds an index on subject_id and cohort_start_date to a cohort table. Note,
currently only indexes will be added if the table is in a postgres database.
}
