\name{Technique 4: eFAST - Perform Analysis of Results at Multiple Timepoints}
\alias{efast_generate_medians_for_all_parameter_subsets_overTime}
\alias{efast_get_overall_medians_overTime}
\alias{efast_run_Analysis_overTime}
\title{eFAST: Perform Analysis of Results at Multiple Timepoints}
\description{This uses the same technique above, and all of the described methods, but analyses simulation results at multiple timepoints in the simulation run.  This may give a better indication of when a particular parameter becomes significant (against the 'Dummy') throughout a time period. This analysis over time method assumes that the result file for each timepoint has the same name (e.g. trackedCells_Close), which has the timepoint at which it was captured appended to it (e.g. trackedCells_Close_12).  It is assumed that the same folder structure is in place as detailed in Technique 4 above, and shown in figure eFAST_Folder_Struc in the extdata folder of this package and in the tutorial available on the project website. The method then examines each timepoint in turn, performing the same analyses documented in Technique 4 above, producing the same output files and same graphs, but this time for multiple timepoints.\cr
\cr
There are three methods available. These act as a wrapper for the previous methods. For example, lets say we are analysing the results at 6,12,18,and 24 hours. Thus TIMEPOINTS will be declared as TIMEPOINTS<-c(6,12,18,24). In this case, it is important that we declare the filenames for both input and output files but leave off the file extension (unlike that used above). All methods below consider each timepoint in turn, and prepare the input and output file names to suit the methods detailed above. So if our result file name was finalPatchStats, the method appends the timepoint being analysed to this and the names of any output files that are generated (producing finalPatchStats_12). Once this is done, the original method described in the technique above is called.
}
\usage{
efast_generate_medians_for_all_parameter_subsets_overTime(FILEPATH,
	NUMCURVES,PARAMETERS,NUMSAMPLES,NUMRUNSPERSAMPLE,MEASURES,
	RESULTFILEFORMAT,RESULTFILENAME,ALTERNATIVEFILENAME,
	OUTPUTCOLSTART,OUTPUTCOLEND,MEDIANSFILEFORMAT,MEDIANSFILENAME,
	TIMEPOINTS)
efast_get_overall_medians_overTime(FILEPATH,NUMCURVES,PARAMETERS,
	NUMSAMPLES,MEASURES,MEDIANSFILEFORMAT,MEDIANSFILENAME,
	CURVERESULTSFILENAME,TIMEPOINTS)
efast_run_Analysis_overTime(FILEPATH,MEASURES,PARAMETERS,NUMCURVES,
	NUMSAMPLES,CURVERESULTSFILENAME,EFASTRESULTFILENAME,
	OUTPUTMEASURES_TO_TTEST,TTEST_CONF_INT,GRAPH_FLAG,TIMEPOINTS,
	TIMEPOINTSCALE)

}
\arguments{
  \item{FILEPATH}{Directory where the simulation runs can be found}
  \item{NUMCURVES}{The number of 'resamples' to perform (see eFAST documentation) - recommend using at least 3}
  \item{PARAMETERS}{Array containing the names of the parameters of which parameter samples will be generated}
  \item{NUMSAMPLES}{The number of parameter subsets that were generated in the eFAST design}
  \item{NUMRUNSPERSAMPLE}{The number of runs performed for each parameter subset. This figure is generated through Aleatory Analysis}
  \item{MEASURES}{Array containing the names of the output measures which are used to analyse the simulation}
  \item{RESULTFILEFORMAT}{The file format output by the simulation. Spartan can process XML or CSV output formats}
  \item{RESULTFILENAME}{Name of the simulation results file (e.g. "trackedCells_Close")}
  \item{ALTERNATIVEFILENAME}{In some cases, it may be relevant to read from a further results file if the initial file contains no results. This filename is set here}
  \item{OUTPUTCOLSTART}{Column number in the results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates}
  \item{OUTPUTCOLEND}{Column number in the results file where the last output measure is}
  \item{MEDIANSFILEFORMAT}{File format for the median distribution files produced from simulation results. Can be either XML or CSV}
  \item{MEDIANSFILENAME}{Analyses the results of replicate simulation runs and creates a file containing the median value of each measure for every run. This specifies what that file should be called (e.g. Medians).  Make sure this has no extension as a timepoint will be appended.}
  \item{CURVERESULTSFILENAME}{Whereas efast_generate_medians_for_all_parameter_subsets processes each replicate set of runs to produce a median set to be analysed, get_overall_medians processes each sample point for each parameter, for each curve in turn, producing a summary file for each curve. This specifies the output file name for this result file.  Note as multiple timepoints are being considered the extention should be removed (Example: AllResults.csv was used for one timepoint in the technique above, so could leave this as AllResults). Each curve will contain a results file bearing this name.}
  \item{TIMEPOINTS}{A list of the simulation timepoints which are to be analysed}
  \item{TIMEPOINTSCALE}{A string detailing how TIMEPOINTS should be interpreted (e.g. this could be "Seconds", "Minutes", "Hours", or simply "Simulation Steps"). Used in graphing}
  \item{OUTPUTMEASURES_TO_TTEST}{Which measures in the range should be tested to see if the result is statistically significant.  To do all, and if there were 3 measures, this would be set to 1:3}
  \item{EFASTRESULTFILENAME}{File name under which the full eFAST analysis should be stored. This will contain the partitioning of variance for each parameter. Example: "eFAST_Analysis".  No CSV extension as studying multiple timepoints}
  \item{TTEST_CONF_INT}{The level of significance to use for the T-Test}
  \item{GRAPH_FLAG}{Whether graphs should be produced summarising the output - should be TRUE or FALSE}
}
\references{
For detailed information on how eFAST works, see either of the following: (a) Marino et al (2008): "A methodology for performing global uncertainty and sensitivity analysis in systems biology", (b) Saltelli et al (2000): "Sensitivity Analysis".  MATLAB code is also available via an associated site stated in (a)
}
\examples{
# THE CODE IN THIS EXAMPLE IS THE SAME AS THAT USED IN THE TUTORIAL, AND
# THUS YOU NEED TO DOWNLOAD THE TUTORIAL DATA SET AND SET FILEPATH
# CORRECTLY TO RUN THIS

##---Firstly,declare the parameters required for the functions--
# A: WHERE THE RESULTS FOR EACH CURVE ARE
FILEPATH<-"/media/FreeAgent/package_Test_Data/eFAST/Results/"
# B: HOW MANY CURVES HAVE BEEN USED IN THE SAMPLING
NUMCURVES<-3
# C: AN ARRAY OF THE PARAMETERS WHICH HAVE BEEN PERTURBED OR ARE
# BEING ANALYSED
# AVOID SPACES AND MAKE SURE DUMMY IS INCLUDED LAST
PARAMETERS <- c("thresholdBindProbability","chemoThreshold",
"chemoUpperLinearAdjust","chemoLowerLinearAdjust",
"maxVCAMeffectProbabilityCutoff","vcamSlope","Dummy")
# D: THE NUMBER OF TIMES THE SPACE HAS BEEN SAMPLED
NUMSAMPLES<-65
# E: THE NUMBER OF TIMES THE SIMULATION HAS BEEN RUN FOR EACH
# PARAMETER SET
NUMRUNSPERSAMPLE<-300
# F: THE OUTPUT MEASURES (ARRAY)
MEASURES<-c("Velocity","Displacement")
# G: FORMAT OF THE SIMULATOR RESULT FILES
RESULTFILEFORMAT<-"csv"
# H: WHAT THE SIMULATION RESULT FILE IS CALLED - NOTE NO EXTENSION 
# AS WE'RE LOOKING OVER TIME
RESULTFILENAME<-"trackedCells_Close"
# I: NAME OF AN ALTERNATIVE RESULTS FILE IF NECESSARY
ALTERNATIVEFILENAME<-NULL
# J: WHERE THE OUTPUT MEASURES START IN THE RESULTS FILE FROM
# THE SIMULATION. SAVES READING THE WHOLE FILE & AVOIDS POTENTIAL
# PROBLEMS WHERE THE FIRST COLUMN IS DUPLICATED (eg A CELL LABEL)
# ONLY USED WITH CSV INPUT
OUTPUTCOLSTART<-10
# H: WHERE THE OUTPUT MEASURES END
# ONLY USED WITH CSV INPUT
OUTPUTCOLEND<-11
# I: FILE FORMAT FOR THE MEDIAN OUTPUT FILE - XML OR CSV
MEDIANSFILEFORMAT<-"csv"
# J: NAME OF THE FILE TO CONTAIN THE MEDIANS FOR ALL RUNS UNDER A SET
# OF CONDITIONS - NOTE NO EXTENSION AS WE'RE LOOKING OVER TIME
MEDIANSFILENAME<-"EgSet_Medians_Over_Time"
# K: NAME OF THE SUMMARY FILE FOR EACH CURVE, SHOWING PARAMETER VALUES
# AND MEDIAN RESULTS FOR THAT CONDITION - NOTE NO EXTENSION AS WE'RE 
# LOOKING OVER TIME
CURVERESULTSFILENAME<-"EgSet_AllResults"
# L: NAME OF FILE WHICH IS GENERATED WHICH HAS OVERALL RESULTS IN
# (PARTITIONING OF VARIANCE) - NOTE NO EXTENSION AS WE'RE LOOKING 
# OVER TIME - THUS SEVERAL FINAL ANALYSES ARE PRODUCED
EFASTRESULTFILENAME<-"EgSet_eFAST_Analysis"
# M: WHICH OUTPUT MEASURES IN THE ARRAY TO T-TEST FOR SIGNIFICANCE
# (SHOULD BE ALL BUT IN SOME CASES MAY NOT BE)
OUTPUTMEASURES_TO_TTEST<-1:2
# N: T-TEST CONFIDENCE LEVEL
TTEST_CONF_INT<-0.95
# O: WHETHER USER WANTS RESULTS GRAPHED
GRAPH_FLAG<-TRUE
# P: TIMEPOINTS TO ANALYSE
TIMEPOINTS<-c(12,36,48,60)
# R: SCALE FOR THE TIMEPOINTS (FOR GRAPHING)
TIMEPOINTSCALE<-"Hours"

\dontrun{
# DONTRUN IS SET SO THIS IS NOT EXECUTED WHEN PACKAGE IS COMPILED - BUT THIS
# HAS BEEN TESTED WITH THE TUTORIAL DATA

library(gplots)

##--- NOW RUN THE FOUR METHODS IN THIS ORDER ----
# FIRSTLY, AS FOR EACH PARAMETER SET MULTIPLE RUNS ARE PERFORMED,
# MEDIAN DISTRIBUTIONS NEED TO BE GAINED FOR EVERY RUN
efast_generate_medians_for_all_parameter_subsets_overTime(FILEPATH,
	NUMCURVES,PARAMETERS,NUMSAMPLES,NUMRUNSPERSAMPLE,MEASURES,
	RESULTFILEFORMAT,RESULTFILENAME,ALTERNATIVEFILENAME,
	OUTPUTCOLSTART,OUTPUTCOLEND,MEDIANSFILEFORMAT,MEDIANSFILENAME,
	TIMEPOINTS)

# NOW NEED TO CREATE THE OUTPUT FILE THAT THE EFAST ANALYSIS SCRIPTS
# USE - A FILE SHOWING THE OVERALL MEDIAN RESULTS FOR EACH THE RUNS
# PERFORMED FOR EVERY PARAMETER OF INTEREST, FOR THAT CURVE.
# ONE FILE IS CREATED PER CURVE
efast_get_overall_medians_overTime(FILEPATH,NUMCURVES,PARAMETERS,
	NUMSAMPLES,MEASURES,MEDIANSFILEFORMAT,MEDIANSFILENAME,
	CURVERESULTSFILENAME,TIMEPOINTS)

# NOW THESE ALLCURVE.CSV FILES HAVE BEEN GENERATED, FULL ANALYSIS
# CAN BEGIN
efast_run_Analysis_overTime(FILEPATH,MEASURES,PARAMETERS,NUMCURVES,
	NUMSAMPLES,CURVERESULTSFILENAME,EFASTRESULTFILENAME,
	OUTPUTMEASURES_TO_TTEST,TTEST_CONF_INT,GRAPH_FLAG,TIMEPOINTS,
	TIMEPOINTSCALE)

}

}
\keyword{eFAST}
\keyword{fourier}
\keyword{amplitude}
\keyword{sampling}
\keyword{parameter}
\keyword{analysis}
