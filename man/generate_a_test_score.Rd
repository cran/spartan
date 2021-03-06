% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aleatory_analysis.R
\name{generate_a_test_score}
\alias{generate_a_test_score}
\title{Take the first set and compare it to a distribution from another set
using the A-Test}
\usage{
generate_a_test_score(ALLATESTRESULTS, SET1, COMPAREDSET, MEASURES)
}
\arguments{
\item{ALLATESTRESULTS}{Current set of A-Test results to append to}

\item{SET1}{Results from sample set one}

\item{COMPAREDSET}{Results from a second set being compared}

\item{MEASURES}{Simulation output measures}
}
\value{
Appended result to ALLATESTRESULTS
}
\description{
Take the first set and compare it to a distribution from another set
using the A-Test
}
