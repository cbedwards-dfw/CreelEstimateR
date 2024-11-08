% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_estimates.R
\name{export_estimates}
\alias{export_estimates}
\title{Export creel model estimates}
\usage{
export_estimates(params, analysis_lut, creel_estimates)
}
\arguments{
\item{params}{??}

\item{analysis_lut}{??}

\item{creel_estimates}{??}
}
\value{
??
}
\description{
This function is the primary control of creel model estimate ETL process.
It calls upon PE and BSS outputs to independently reformat to a standardized format.
The resulting objects are either output locally as a csv or uploaded to the creel database.
}
