% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/json_conversion.R
\name{json_conversion}
\alias{json_conversion}
\title{Convert model estimate metadata objects to json format}
\usage{
json_conversion(type)
}
\arguments{
\item{type}{Character string; one of "script", "regulations", or "r_session". Evan or Colt: help!}
}
\value{
Evan or colt: help!
}
\description{
Dev note: This function is modifying \code{analysis_lut} in the global envi with \verb{<<-}. we need
to rewrite to take in and then return \code{analysis_lut}. Also needs to take \code{params} as an arg.
}
\examples{
\dontrun{
json_conversion(type = "script")
json_conversion(type = "r_session")
}
}
