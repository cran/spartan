\name{Utility: Generate Median Distribution(s)}
\alias{getMediansSubset}
\title{Generate Medians Subset (getMediansSubset)}
\description{This utility function takes a set of simulation runs, all performed with the same parameter set, and generates the set of median output measure results.  This set will contain the median for each output measure, for each run performed. This aids reduction in aleatory uncertainty and production of a representative result. The set of medians is returned to the function that calls this - such that it can be processed appropriately}
\usage{
getMediansSubset(FILEPATH,NUMRUNSPERSAMPLE,MEASURES,
	RESULTFILENAME,ALTFILENAME,OUTPUTFILECOLSTART,
	OUTPUTFILECOLEND)
}
\arguments{
  \item{FILEPATH}{Directory where the parameter sample medians should be output to}
  \item{NUMRUNSPERSAMPLE}{The number of runs performed for each parameter subset. This figure is generated through Aleatory Analysis}
  \item{MEASURES}{Array containing the names of the output measures which are used to analyse the simulation} 
  \item{RESULTFILENAME}{Name of the simulation results file (e.g. "trackedCells_Close.csv")}
  \item{ALTFILENAME}{In some cases, it may be relevant to read from a further results file if the initial file contains no results. This filename is set here}
  \item{OUTPUTFILECOLSTART}{Column number in the results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates}
  \item{OUTPUTFILECOLEND}{Column number in the results file where the last output measure is}
}
\keyword{Median}
\keyword{Distribution}
