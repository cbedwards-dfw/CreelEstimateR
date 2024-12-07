% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_paired_census_index.R
\name{plot_paired_census_index}
\alias{plot_paired_census_index}
\title{plot paired census index}
\usage{
plot_paired_census_index(
  days,
  dwg_summarized,
  interview_ang_per_vehic,
  census_expan,
  angler_hours_daily_mean,
  params
)
}
\arguments{
\item{days}{??}

\item{dwg_summarized}{??}

\item{interview_ang_per_vehic}{??}

\item{census_expan}{??}

\item{angler_hours_daily_mean}{??}

\item{params}{??}
}
\value{
List. \verb{$plot} is ggplot object, \verb{$census_TI_expan} is ??
}
\description{
Pared down version of prep_inputs_pe_ang_hrs_vhcl_trlr.R function to return table with season-long
sum of angler counts from paired census and index count surveys and scatter plot showing ratio of census
to index counts relative to a 1:1 line in a scatterplot.
}
\details{
Dev notes: where is angler_hours_daily_mean defined?
}
