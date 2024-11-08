% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_pe_effort.R
\name{est_pe_effort}
\alias{est_pe_effort}
\title{Calculate mean and variance for section_num-period-day_type-angler_final}
\usage{
est_pe_effort(days, pe_inputs_list, sections)
}
\arguments{
\item{days}{??}

\item{pe_inputs_list}{??}

\item{sections}{??}
}
\value{
??
}
\description{
This is the finest stratification above individual days.
}
\details{
sample size is inherently small for a day_type within week stratification
-the var() and sd() functions return NA when passed a length-1 vector (single obs)
-variance has limited meaning even when n=3, e.g. if sampling Fri & Sat & Sun
BUT sample design itself (and first principles) stratify on day_type
such that pooling over weekend/weekday is counter to data collection protocol/design
could pool over weeks (and perhaps angler_final) if a better variance is desired over the fishery duration
}
