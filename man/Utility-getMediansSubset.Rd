\name{Utility: Generate Median Distribution(s)}
\alias{getMediansSubset}
\title{Generate Medians Subset (getMediansSubset)}
\description{This utility function takes a set of simulation runs, all performed with the same parameter set, and generates the set of median output measure results.  This set will contain the median for each output measure, for each run performed. This aids reduction in aleatory uncertainty and production of a representative result}
\usage{
getMediansSubset(FILEPATH,NUMRUNSPERSAMPLE,MEASURES,
	RESULTFILEFORMAT,RESULTFILENAME,ALTFILENAME,
	OUTPUTFILECOLSTART,OUTPUTFILECOLEND,
	MEDIANSFILEFORMAT,MEDIANSFILENAME)
}
\arguments{
  \item{FILEPATH}{Directory where the parameter sample medians should be output to}
  \item{NUMRUNSPERSAMPLE}{The number of runs performed for each parameter subset. This figure is generated through Aleatory Analysis}
  \item{MEASURES}{Array containing the names of the output measures which are used to analyse the simulation} 
  \item{RESULTFILEFORMAT}{The file format output by the simulation. Spartan can process XML or CSV output formats}
  \item{RESULTFILENAME}{Name of the simulation results file (e.g. "trackedCells_Close.csv")}
  \item{ALTFILENAME}{In some cases, it may be relevant to read from a further results file if the initial file contains no results. This filename is set here}
  \item{OUTPUTFILECOLSTART}{Column number in the results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates}
  \item{OUTPUTFILECOLEND}{Column number in the results file where the last output measure is}
  \item{MEDIANSFILEFORMAT}{File format for the median distribution files produced from simulation results. Can be either XML or CSV}
  \item{MEDIANSFILENAME}{The name of the median results file that is created, e.g. Medians.csv}
}
\keyword{Median}
\keyword{Distribution}
