% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efast_analysis.R
\name{construct_result_filename}
\alias{construct_result_filename}
\title{Appends the time to an eFAST argument, if processing multiple timepoints}
\usage{
construct_result_filename(filepath, filename, timepoint = NULL)
}
\arguments{
\item{filepath}{Working directory}

\item{filename}{Name of the file to append time to (or not)}

\item{timepoint}{Timepoint being processed (if any)}
}
\value{
Path to file with timepoint added if necessary
}
\description{
Appends the time to an eFAST argument, if processing multiple timepoints
}
