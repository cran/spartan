\name{Technique 2: One-At-A-Time - Perform Analysis of Results at Multiple Timepoints}
\alias{oat_processParamSubsets_overTime}
\alias{oat_analyseAllParams_overTime}
\alias{oat_graphATestsForSampleSize_overTime}
\alias{oat_plotResultDistribution_overTime}
\title{One-At-A-Time - Perform Analysis of Results at Multiple Timepoints}
\description{This uses the same technique above, and all of the described methods, but analyses simulation results at multiple timepoints in the simulation run.  This may give a better indication of when a parameter value begins to become influential throughout a run. This analysis over time method assumes that the result file for each timepoint has the same name (e.g. finalPatchStats), which has the timepoint at which it was captured appended to it (e.g. finalPatchStats_12.csv).  It is assumed that the same folder structure is in place as detailed in Technique 2 above, and shown in figure OAT_Folder_Struc in the extdata folder of this package. The package then examines each timepoint in turn, performing the same analyses documented in Technique 2 above, thus producing A-Test statistics for each parameter value at that timepoint, and graphs to present these results in an easy to read manner. Focus then is set to the next timepoint, until all have been considered.\cr
\cr
Again there are four methods available. These act as a wrapper for the previous methods. For example, lets say we are analysing the results at 6,12,18,and 24 hours. Thus TIMEPOINTS will be declared as TIMEPOINTS<-c(6,12,18,24). All methods below consider each timepoint in turn, and prepare the input and output file names to suit the methods detailed above. So if our result file name was finalPatchStats, the method appends the timepoint being analysed to this and the names of any output files that are generated (producing finalPatchStats_12.csv). Once this is done, the original method described in the technique above is called.
} 
\usage{
oat_processParamSubsets_overTime(FILEPATH,PARAMETERS,PMIN,PMAX,
	PINC,NUMRUNSPERSAMPLE,MEASURES,OUTPUTCOLSTART,OUTPUTCOLEND,
	RESULTSFILENAME,ALTERNATIVEFILENAME,MEDIANSFILENAME,
	TIMEPOINTS)
oat_analyseAllParams_overTime(FILEPATH,PARAMETERS,BASELINE,PMIN,
	PMAX,PINC,MEASURES,MEDIANSFILENAME,ATESTRESULTSFILENAME,
	TIMEPOINTS)
oat_graphATestsForSampleSize_overTime(FILEPATH,PARAMETERS,PMIN,
	PMAX,PINC,ATESTRESULTSFILENAME,MEASURES,ATESTSIGLEVEL,
	TIMEPOINTS,TIMEPOINTSCALE)
oat_plotResultDistribution_overTime(FILEPATH,PARAMETERS,PMIN,
	PMAX,PINC,MEDIANSFILENAME,MEASURES,MEASURE_SCALE,
	TIMEPOINTS,TIMEPOINTSCALE)
}
\arguments{
  \item{FILEPATH}{Directory where the simulation runs can be found}
  \item{PARAMETERS}{Array containing the names of the parameters of which parameter samples will be generated}
  \item{NUMRUNSPERSAMPLE}{The number of runs performed for each parameter subset. This figure is generated through Aleatory Analysis}
  \item{OUTPUTCOLSTART}{Column number in the results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates}
  \item{OUTPUTCOLEND}{Column number in the results file where the last output measure is}
  \item{RESULTSFILENAME}{Name of the simulation results file (e.g. "trackedCells_Close.csv")}
  \item{ALTERNATIVEFILENAME}{In some cases, it may be relevant to read from a further results file if the initial file contains no results. This filename is set here}
  \item{BASELINE}{Array containing the values assigned to each of these parameters in the calibrated baseline}
  \item{PMIN}{Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space}
  \item{PMAX}{Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space}
  \item{PINC}{Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10}
  \item{MEASURES}{Array containing the names of the output measures which are used to analyse the simulation}
  \item{MEDIANSFILENAME}{If oat_processParamSubsets is used, this analyses the results of replicate simulation runs and creates a file containing the median value of each measure for every run, for each timepoint. This specifies what that file should be called.  \bold{Note, the algorithm takes this file name and appends the timepoint to it, so don't specify .csv}. For example, this could be set to "Medians". The algorithm, for timepoint 6, would then make this "Medians_6.csv"}
  \item{ATESTRESULTSFILENAME}{File name of the ATests result summary file created by oat_analyseAllParams.  Again, as multiple timepoints are being considered, don't specify the extension.  If this was set to "ATests", the algorithm would create "ATests_6.csv" for a timepoint of 6}
  \item{TIMEPOINTS}{A list of the simulation timepoints which are to be analysed}
  \item{TIMEPOINTSCALE}{A string detailing how TIMEPOINTS should be interpreted (e.g. this could be "Seconds", "Minutes", "Hours", or simply "Simulation Steps"). Used in graphing}
  \item{ATESTSIGLEVEL}{The A-Test determines if there is a large difference between two sets if the result is greater than 0.21 either side of the 0.5 line. Should this not be suitable, this can be changed here}
  \item{MEASURE_SCALE}{An array containing the measure used for each of the output measures (i.e. microns, microns/min).  Used to label graphs}
}
\references{This technique is described by Read et al (2011) in their paper: Techniques for Grounding Agent-Based Simulations in the Real Domain: a case study in Experimental Autoimmune Encephalomyelitis"
}
\examples{
# THE CODE IN THIS EXAMPLE IS THE SAME AS THAT USED IN THE TUTORIAL, AND
# THUS YOU NEED TO DOWNLOAD THE TUTORIAL DATA SET AND SET FILEPATH
# CORRECTLY TO RUN THIS

##---- Firstly, declare the parameters required for the four functions ----
# A: WHERE THE RESULTS FOR EACH CURVE ARE
FILEPATH<-"/media/FreeAgent/package_Test_Data/OAT/"
# B: AN ARRAY OF THE PARAMETERS WHICH HAVE BEEN PERTURBED OR ARE
# BEING ANALYSED
PARAMETERS<-c("thresholdBindProbability","chemoThreshold",
"chemoUpperLinearAdjust","chemoLowerLinearAdjust",
"maxVCAMeffectProbabilityCutoff","vcamSlope")
#C: ARRAY OF MINIMUM VALUES
PMIN<-c(0,0.10,0.10,0.015,0.1,0.25,8)
# D: ARRAY OF MAXIMUM VALUES
PMAX<-c(100,0.9,0.50,0.08,1.0,5.0,12)
# E: ARRAY OF PARAMETER INCREMENT
PINC<-c(10,0.1,0.05,0.005,0.05,0.25,1)
# F: THE NUMBER OF TIMES THE SIMULATION HAS BEEN RUN FOR EACH
# PARAMETER SET
NUMRUNSPERSAMPLE<-300
# G: THE OUTPUT MEASURES (ARRAY)
MEASURES<-c("Velocity","Displacement")
# H: WHERE THE OUTPUT MEASURES START IN THE RESULTS FILE FROM THE
# SIMULATION. SAVES READING THE WHOLE FILE & AVOIDS POTENTIAL
# PROBLEMS WHERE THE FIRST COLUMN IS DUPLICATED (eg A CELL LABEL)
OUTPUTCOLSTART<-10
# I: WHERE THE OUTPUT MEASURES END
OUTPUTCOLEND<-11
# J: WHAT THE SIMULATION RESULT FILE IS CALLED
RESULTSFILENAME<-"trackedCells_Close"
# K: NAME OF AN ALTERNATIVE RESULTS FILE IF NECESSARY
ALTERNATIVEFILENAME<-NULL
# L: NAME TO BE GIVEN TO EACH FILE OF GENERATED MEDIAN MEASURES FOR
# EACH PARAMETER
MEDIANSFILENAME<-"EgSet_Medians_Over_Time"
# M: NAME TO BE GIVEN TO THE FILES CONTAINING THE A-TEST RESULTS FOR
# EACH PARAMETER
ATESTRESULTSFILENAME<-"EgSet_ATests_Over_Time"
# N: BASELINE PARAMETER VALUES
BASELINE<-c(50,0.3,0.2,0.04,0.60,1.0,12)
# O: LEVEL AT WHICH A-TEST RESULTS ARE SIGNIFICANT
ATESTSIGLEVEL<-0.21
# P: MEASUREMENTS FOR GRAPHING
MEASURE_SCALE<-c(NULL,NULL,"Simulation Steps","Simulation Steps",
	"Simulation Steps","Square Pixels")
# Q: TIMEPOINTS - USED WHEN ANALYSING OVER TIME
TIMEPOINTS<-c(12,36,48,60)
TIMEPOINTSCALE<-"Hours"

# NOW RUN THE METHODS

\dontrun{
# DONTRUN IS SET SO THIS IS NOT EXECUTED WHEN PACKAGE IS COMPILED - BUT THIS
# HAS BEEN TESTED WITH THE TUTORIAL DATA

# A - GENERATE THE MEDIAN SETS FOR EACH SET OF RUNS FOR THE
# PARAMETER VALUE
oat_processParamSubsets_overTime(FILEPATH,PARAMETERS,PMIN,PMAX,
	PINC,NUMRUNSPERSAMPLE,MEASURES,OUTPUTCOLSTART,OUTPUTCOLEND,
	RESULTSFILENAME,ALTERNATIVEFILENAME,MEDIANSFILENAME,
	TIMEPOINTS)

# B - RUN THE ATEST FOR EACH PARAMETER VALUE, AND EACH PARAMETER
oat_analyseAllParams_overTime(FILEPATH,PARAMETERS,BASELINE,PMIN,
	PMAX,PINC,MEASURES,MEDIANSFILENAME,ATESTRESULTSFILENAME,
	TIMEPOINTS)


# C - GRAPH THE RESULTS FOR ALL MEASURES FOR EACH PARAMETER
oat_graphATestsForSampleSize_overTime(FILEPATH,PARAMETERS,PMIN,
	PMAX,PINC,ATESTRESULTSFILENAME,MEASURES,ATESTSIGLEVEL,
	TIMEPOINTS,TIMEPOINTSCALE)

# D - GRAPH THE DISTRIBUTION OF THE RESULTS FOR THIS MEASURE, FOR
# THIS PARAMETER
oat_plotResultDistribution_overTime(FILEPATH,PARAMETERS,PMIN,
	PMAX,PINC,MEDIANSFILENAME,MEASURES,MEASURE_SCALE,
	TIMEPOINTS,TIMEPOINTSCALE)
}
}
\keyword{OAT}
\keyword{Parameter}
\keyword{Analysis}
\keyword{A-Test}
