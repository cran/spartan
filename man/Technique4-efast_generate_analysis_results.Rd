\name{Technique 4: eFAST - Perform Analysis of Results}
\alias{efast_generate_medians_for_all_parameter_subsets}
\alias{efast_get_overall_medians}
\alias{efast_run_Analysis}
\title{eFAST: Perform Analysis of Results}
\description{This technique analyses simulation results generated through parametering using the eFAST approach (extended Fourier Amplitude Sampling Test, Saltelli et al, reference below). This perturbs the value of all parameters at the same time, with the aim of partitioning the variance in simulation output between input parameters. Values for each parameter are chosen using fourier frequency curves through a parameters potential range of values. A selected number of values are selected from points along the curve. Though all parameters are perturbed simultaneously, the method does focus on one parameter of interest in turn, by giving this a very different sampling frequency to that assigned to the other parameters. Thus for each parameter of interest in turn, a sampling frequency is assigned to each parameter and values chosen at points along the curve. So a set of simulation parameters then exists for each parameter of interest.  As this is the case, this method can be computationally expensive, especially if a large number of samples is taken on the parameter search curve, or there are a large number of parameters. On top of this, to ensure adequate sampling each curve is also resampled with a small adjustment to the frequency, creating more parameter sets on which the simulation should be run. This attempts to limit any correlations and limit the effect of repeated parameter value sets being chosen. Thus, for a system where 8 parameters are being analysed, and 3 different sample curves used, 24 different sets of parameter value sets will be produced. Each of these 24 sets then contains the parameter values chosen from the frequency curves. This number of samples should be no lower than 65 (see the Marino paper for an explanation of how to select sample size).\cr
\cr
Once the sampling has been performed, a number of simulation runs should be performed for each set generated (this number that which has become apparent through analysis of aleatory uncertainty, or use of Technique 1 within the spartan package). The eFAST algorithm then examines the simulation results for each parameter value set and, taking into account the sampling frequency used to produce those parameter values, partitions the variance in output between the input parameters. The spartan package includes methods to both create parameter value samples using fourier frequency sampling, and to analyse the simulation results. This method does the latter.\cr
\cr
\bold{Note:} The package expects that the results to be analysed are within a set folder structure. For an explanation of what this structure should be, see figure eFAST_Folder_Struc.png in the extdata folder within this package, or the tutorial available on the project website.\cr
\cr
There are three methods to this process:\cr
\bold{efast_generate_medians_for_all_parameter_subsets:} As stated previously, a robust representative result is achieved when the simulation is run a number of times for the same parameter conditions. The aim of this method is to examine each parameter value set and produce a file containing the median of each output measure for each simulation run performed under those conditions. For example, we could achieve a representative result if we perform 300 runs of the simulation under the same criteria. The aim would therefore be to produce a file which contains the medians of each of our simulation output measures, of each of the 300 runs. We can then use this distribution in comparison with that produced by another parameter value set. In this case, we examine each resampling curve in turn, and each parameter in turn. Each parameter then has a number of sets of value sets generated in sampling, with each containing a number of simulation runs. When this method is invoked, we will produce a set of medians for each of these value sets, for each parameter. These will be stored in the third level of the folder structure, under the filename specified in the R parameter MEDIANSFILENAME.\cr
\bold{efast_get_overall_medians:}This method produces a summary of the results for a particular resampling curve. This shows, for each parameter of interest, the median of each simulation output measure for each of the 65 parameter value sets generated. Here's an example. We examine resampling curve 1, and firstly examine parameter 1. For this parameter of interest, a number of different parameter value sets were generated from the frequency curves (lets say 65), thus we have 65 different sets of simulation results. The previous method produced a summary showing the median of each output measure for each run. Now, this method calculates the median of these medians, for each output measure, and stores these in the summary. Thus, for each parameter of interest, the medians of each of the 65 sets of results are stored. The next parameter is then examined, until all have been analysed. This produces a snapshot showing the median simulation output for all parameter value sets generated for the first resample curve. These are stored within the second level of the folder structure, with filename as specified by the R variable CURVERESULTSFILENAME.  An example can be seen in the data folder of this package (EgSet_eFAST_AllResults.csv).\cr
\bold{efast_run_Analysis:} Produces a file summarising the analysis; partitioning the variance between parameters and providing relevant statistics. These include, for each parameter of interest, first-order sensitivity index (Si), total-order sensitivity index (STi), complementary parameters sensitivity index (SCi), and relevant p-values and error bar data calculated using a two-sample t-test and standard error respectively. For a more detailed examination of this analysis, see the Marino paper or Saltelli book references, or the tutorial on the package website. An example of the output file generated can be seen in the data folder of this package (EgSet_eFAST_Analysis.csv) For ease of representation, the method also produces a graph showing this data for each simulation output measure. Two examples can be seen in the extdata folder of this package (eFAST_Displacement.pdf and eFAST_Velocity.pdf).
}
\usage{
efast_generate_medians_for_all_parameter_subsets(FILEPATH,
	NUMCURVES,PARAMETERS,NUMSAMPLES,NUMRUNSPERSAMPLE,
	MEASURES,RESULTFILEFORMAT,RESULTFILENAME,
	ALTERNATIVEFILENAME,OUTPUTCOLSTART,OUTPUTCOLEND,
	MEDIANSFILEFORMAT,MEDIANSFILENAME)

efast_get_overall_medians(FILEPATH,NUMCURVES,PARAMETERS,
	NUMSAMPLES,MEASURES,MEDIANSFILEFORMAT,MEDIANSFILENAME,
	CURVERESULTSFILENAME)

efast_run_Analysis(FILEPATH,MEASURES,PARAMETERS,NUMCURVES,
	NUMSAMPLES,OUTPUTMEASURES_TO_TTEST,TTEST_CONF_INT,
	GRAPH_FLAG,CURVERESULTSFILENAME,EFASTRESULTFILENAME,
	TIMEPOINT,TIMEPOINTSCALE)


}
\arguments{
  \item{FILEPATH}{Directory where the simulation runs can be found}
  \item{NUMCURVES}{The number of 'resamples' to perform (see eFAST documentation) - recommend using at least 3}
  \item{PARAMETERS}{Array containing the names of the parameters of which parameter samples will be generated}
  \item{NUMSAMPLES}{The number of parameter subsets that were generated in the eFAST design}
  \item{NUMRUNSPERSAMPLE}{The number of runs performed for each parameter subset. This figure is generated through Aleatory Analysis}
  \item{MEASURES}{Array containing the names of the output measures which are used to analyse the simulation}
  \item{OUTPUTCOLSTART}{Column number in the results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates}
  \item{OUTPUTCOLEND}{Column number in the results file where the last output measure is}
  \item{RESULTFILEFORMAT}{The file format output by the simulation. Spartan can process XML or CSV output formats}
  \item{RESULTFILENAME}{Name of the simulation results file (e.g. "trackedCells_Close")}
  \item{ALTERNATIVEFILENAME}{In some cases, it may be relevant to read from a further results file if the initial file contains no results. This filename is set here}
  \item{MEDIANSFILEFORMAT}{File format for the median distribution files produced from simulation results. Can be either XML or CSV}
  \item{MEDIANSFILENAME}{If oat_processParamSubsets is used, this analyses the results of replicate simulation runs and creates a file containing the median value of each measure for every run. This specifies what that file should be called (e.g. Medians)}
  \item{CURVERESULTSFILENAME}{Whereas efast_generate_medians_for_all_parameter_subsets processes each replicate set of runs to produce a median set to be analysed, get_overall_medians processes each sample point for each parameter, for each curve in turn, producing a summary file for each curve. This specifies the output file name for this result file. Example: AllResults could be set here, then each curve will contain a results file bearing this name}
  \item{OUTPUTMEASURES_TO_TTEST}{Which measures in the range should be tested to see if the result is statistically significant.  To do all, and if there were 3 measures, this would be set to 1:3}
  \item{EFASTRESULTFILENAME}{File name under which the full eFAST analysis should be stored. This will contain the partitioning of variance for each parameter. Example: eFAST_Analysis}
  \item{TTEST_CONF_INT}{The level of significance to use for the T-Test}
  \item{GRAPH_FLAG}{Whether graphs should be produced summarising the output - should be TRUE or FALSE}
  \item{TIMEPOINT}{Used so this method can be used when analysing multiple simulation timepoints.  NULL in this case.  See Technique 4: eFAST Analysis Over Time}
  \item{TIMEPOINTSCALE}{Used so this method can be used when analysing multiple simulation timepoints.  NULL in this case.  See Technique 4: eFAST Analysis Over Time}
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
# H: WHAT THE SIMULATION RESULT FILE IS CALLED - NO EXTENSION
RESULTFILENAME<-"trackedCells_Close_Endpoint"
# I: NAME OF AN ALTERNATIVE RESULTS FILE IF NECESSARY
ALTERNATIVEFILENAME<-NULL
# J: WHERE THE OUTPUT MEASURES START IN THE RESULTS FILE FROM
# THE SIMULATION. SAVES READING THE WHOLE FILE & AVOIDS POTENTIAL
# PROBLEMS WHERE THE FIRST COLUMN IS DUPLICATED (eg A CELL LABEL)
OUTPUTCOLSTART<-10
# K: WHERE THE OUTPUT MEASURES END
OUTPUTCOLEND<-11
# L: FILE FORMAT FOR THE MEDIAN OUTPUT FILE - XML OR CSV
MEDIANSFILEFORMAT<-"csv"
# M: NAME OF THE FILE TO CONTAIN THE MEDIANS FOR ALL RUNS UNDER A SET
# OF CONDITIONS
MEDIANSFILENAME<-"EgSet_Medians"
# N: NAME OF THE SUMMARY FILE FOR EACH CURVE, SHOWING PARAMETER VALUES
# AND MEDIAN RESULTS FOR THAT CONDITION
CURVERESULTSFILENAME<-"EgSet_AllResults"
# O: NAME OF FILE WHICH IS GENERATED WHICH HAS OVERALL RESULTS IN
# (PARTITIONING OF VARIANCE)
EFASTRESULTFILENAME<-"EgSet_eFAST_Analysis"
# P: WHICH OUTPUT MEASURES IN THE ARRAY TO T-TEST FOR SIGNIFICANCE
# (SHOULD BE ALL BUT IN SOME CASES MAY NOT BE)
OUTPUTMEASURES_TO_TTEST<-1:2
# Q: T-TEST CONFIDENCE LEVEL
TTEST_CONF_INT<-0.95
# R: WHETHER USER WANTS RESULTS GRAPHED
GRAPH_FLAG<-TRUE
# S: TIMEPOINT AND TIMEPOINT SCALE NOT USED HERE AS NOT DOING 
# MULTIPLE TIMEPOINTS
TIMEPOINT<-NULL
TIMEPOINTSCALE<-NULL

\dontrun{
# DONTRUN IS SET SO THIS IS NOT EXECUTED WHEN PACKAGE IS COMPILED - BUT THIS
# HAS BEEN TESTED WITH THE TUTORIAL DATA

library(gplots)


##--- NOW RUN THE FOUR METHODS IN THIS ORDER ----
# FIRSTLY, AS FOR EACH PARAMETER SET MULTIPLE RUNS ARE PERFORMED,
# MEDIAN DISTRIBUTIONS NEED TO BE GAINED FOR EVERY RUN
efast_generate_medians_for_all_parameter_subsets(FILEPATH,NUMCURVES,
	PARAMETERS,NUMSAMPLES,NUMRUNSPERSAMPLE,MEASURES,RESULTFILEFORMAT,
	RESULTFILENAME,ALTERNATIVEFILENAME,OUTPUTCOLSTART,OUTPUTCOLEND,
	MEDIANSFILEFORMAT,MEDIANSFILENAME)

# NOW NEED TO CREATE THE OUTPUT FILE THAT THE EFAST ANALYSIS SCRIPTS
# USE - A FILE SHOWING THE OVERALL MEDIAN RESULTS FOR EACH THE RUNS
# PERFORMED FOR EVERY PARAMETER OF INTEREST, FOR THAT CURVE.
# ONE FILE IS CREATED PER CURVE
efast_get_overall_medians(FILEPATH,NUMCURVES,PARAMETERS,NUMSAMPLES,
	MEASURES,MEDIANSFILEFORMAT,MEDIANSFILENAME,
	CURVERESULTSFILENAME)

# NOW THESE ALLCURVE.CSV FILES HAVE BEEN GENERATED, FULL ANALYSIS
# CAN BEGIN
efast_run_Analysis(FILEPATH,MEASURES,PARAMETERS,NUMCURVES,NUMSAMPLES,
	OUTPUTMEASURES_TO_TTEST,TTEST_CONF_INT,GRAPH_FLAG,
	CURVERESULTSFILENAME,EFASTRESULTFILENAME,TIMEPOINT,
	TIMEPOINTSCALE)


}
}
\keyword{eFAST}
\keyword{Fourier}
\keyword{amplitude}
\keyword{sampling}
\keyword{parameter}
\keyword{analysis}
