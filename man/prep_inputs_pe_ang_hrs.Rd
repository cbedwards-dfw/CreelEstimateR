% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_inputs_pe_ang_hrs.R
\name{prep_inputs_pe_ang_hrs}
\alias{prep_inputs_pe_ang_hrs}
\title{Title}
\usage{
prep_inputs_pe_ang_hrs(
  days,
  dwg_summarized,
  interview_ang_per_object,
  paired_census_index_counts,
  study_design
)
}
\arguments{
\item{days}{tibble with time strata and closure fields}

\item{dwg_summarized}{list with shared interview, effort index and effort census tibbles}

\item{interview_ang_per_object}{tibble (from list) that has summarized anglers per count_type object by angler_final}

\item{paired_census_index_counts}{tibble (from list) that has summarized tie-in (aka census) count expansion factors by section & angler_final}

\item{study_design}{parameter specifying study design and which if/else loop gets called below}
}
\value{
?
}
\description{
Title
}
