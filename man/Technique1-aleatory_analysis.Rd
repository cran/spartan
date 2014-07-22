\name{Technique 1: Aleatory Analysis}
\alias{aa_summariseReplicateRuns}
\alias{aa_getATestResults}
\alias{aa_sampleSizeSummary}
\alias{aa_graphSampleSizeSummary}
\title{Technique 1: Aleatory Analysis}
\description{
Aleatory uncertainty is caused by inherent stochasticity within a simulation. For stochastic simulations, a number of replicate runs need to be performed for each parameter set, thus achieving a more representative result. This technique indicates the number of simulation runs necessary to reduce this uncertainty. This follows the method described by Read et al in the reference below. To use this, you should have chosen the number of runs you want to compare (for example, 1,5,50,100,300,500,and 800). For each sample size, you should choose a number of subsets (for example, 20). Then, you should create this number of subsets of runs for each sample size. So, choosing the values from the examples, we have 20 sets where the simulation was run once, 20 sets where each set contains the results of 5 runs, right through to 20 sets where each contains the results of 800 runs. This method then looks at each sample size used, and (a) generates the median distribution for all output measures for each of the subsets (b) goes through each of the subsets (20 in the example case), comparing the median of each measure with the respective median in the first subset, using the Vargha-Delaney A-Test (reference below) to give an indication of how different the results are, (c) for each sample size, creates graphes showing how different the results of each subset are (i.e. the A-Test result). A summary of the A-Test results is also output as a Comma Separated Value file, an example of which can be found in the data folder of this package (AA_Example_ATestMaxAndMedians.csv). A full tutorial on using this technique, along with example simulation output to use, can be found on the project website.\cr
\cr
\bold{Note 1:} From Spartan 2.0, you can specify your simulation data in two ways:\cr
\bold{A} - Set folder structure (as in previous versions of Spartan): This is shown in figure AA_Folder_Struc.png within the extdata folder of this package, and described in detail in the tutorial. Using this structure, the parameter FILEPATH should point to a directory that contains a folder that in turn contains the results for the sample sizes being analysed. For example, if the sample sizes being analysed were 1, 5, 50, 100, 300, 500, and 800, the folder specified by FILEPATH would contain seven folders, one for each of these sample sizes. The folder for each sample size then contains one folder for each of the result subsets. In the example case, this would be 20 folders, numbered 1-20. Each of these folders will contain the number of simulation runs performed for the sample size being analysed. For example, if the uncertainty of 5 simulation runs is being examined, each of the 20 folders will contain the results from 5 simulation runs. If 100 runs were being examined, each of the 20 folders would contain the results from 100 runs, and so on. The folders containing the results from each run should be numbered from 1 to the number of runs performed.\cr
\bold{B} - Single CSV file Input. From Spartan 2.0, you can specify all your results in a single CSV file. An example of this file can be found in the extdata folder of the package, named AA_SimResponses.csv. Each row of this file should correspond to one of the sample sizes and subsets to be analysed. The first two columns should therefore be the sample size being analysed and the number of the subset for this sample size. Remaining columns then list the simulation responses for that set. For example, consider a sample set of 5. The first set of 5 runs, set 1, will exist in this CSV file as 5 rows, with the simulation result from each of the 5 runs in this set. See either the input structure detail on the YCIL website, or the example file for more detail. This technique will then process this file rather than the folder structure as Spartan did previously.\cr
\cr
\bold{Note 2:} From Spartan 2.0, performing this analysis at multiple timepoints is now performed using the same method calls below. There are no additional method calls for timepoint analysis.\cr 
\cr
This technique consists of four methods:\cr
\bold{aa_summariseReplicateRuns}: Only to be applied in cases where simulation responses are supplied in the folder structure (as in all previous versions of Spartan), useful for cases where the simulation is agent-based. Iterates through simulation runs for each sample size creating a CSV file containing results for all sample sizes and all subsets (in the same format as the new CSV file format discussed above). Where a simulation response is comprised of a number of records (for example a number of cells), the median value will be recorded as the response for this subset of the sample size being analysed. This file is output to a CSV file, named as stated by the parameter MEDIANS_SUMMARY_FILE_NAME. If doing this analysis over multiple timepoints, the timepoint will be appended to the filename given in MEDIANS_SUMMARY_FILE_NAME.\cr
\bold{aa_getATestResults}: Examines the CSV file produced either by the method above or provided by the user, analysing each sample size independently, to determine how 'different' the results of each of the subsets are. For each sampple size, the distribution of responses for each subset are compared with the first subset using the Vargha-Delaney A-Test. These scores are stored in a CSV file, with filename as stated in parameter ATESTRESULTSFILENAME. The A-Test results for a sample size are then graphed, showing how different each of the subsets are. An example graph can be seen in the extdata folder of this package (AA_5Samples.pdf). If doing this analysis over multiple timepoints, the timepoint will be appended to the filename given in ATESTRESULTSFILENAME and appended to the name of the graph.\cr
\bold{aa_sampleSizeSummary}: This takes each sample size to be examined in turn, and iterates through all the subsets, determining the median and maximum A-Test score observed for each sample size. A CSV file is created summarising the median and maximum A-Test scores for all sample sizes, named as stated in parameter SUMMARYFILENAME. If doing this analysis over multiple timepoints, the timepoint will be appended to the filename given in SUMMARYFILENAME. \cr
\bold{aa_graphSampleSizeSummary}: Produces a full graph of the data generated by the above method (by full, we mean the y-axis (the A-Test score) goes from 0-1, and the x axis contains all sample sizes examined), making it easy to see how uncertainty reduces with an increase in sample size. An example can be seen in the extdata folder of this package (AA_Results.pdf). This graph is named as stated in the parameter GRAPHOUTPUTFILE, with the timepoint appended if the analysis is for multiple timepoints.
}
\usage{

aa_summariseReplicateRuns(FILEPATH,SAMPLESIZES,MEASURES,RESULTFILENAME,
	ALTFILENAME,OUTPUTFILECOLSTART,OUTPUTFILECOLEND,
	AA_SIM_RESULTS,TIMEPOINTS=NULL,TIMEPOINTSCALE=NULL)

aa_getATestResults(FILEPATH,SAMPLESIZES,NUMSUBSETSPERSAMPLESIZE,
	MEASURES,AA_SIM_RESULTS,ATESTRESULTSFILENAME,
	LARGEDIFFINDICATOR,TIMEPOINTS=NULL,TIMEPOINTSCALE=NULL,GRAPHNAME=NULL)

aa_sampleSizeSummary(FILEPATH,SAMPLESIZES,MEASURES,ATESTRESULTSFILENAME,
	SUMMARYFILENAME,TIMEPOINTS=NULL,TIMEPOINTSCALE=NULL)

aa_graphSampleSizeSummary(FILEPATH,MEASURES,MAXSAMPLESIZE,SMALL,MEDIUM,
	LARGE,SUMMARYFILENAME,GRAPHOUTPUTFILE,TIMEPOINTS=NULL,
	TIMEPOINTSCALE=NULL,GRAPHLABEL=NULL)

}
\arguments{
  \item{FILEPATH}{Directory where the results of the simulation runs, in folders or in single CSV file format, can be found}
  \item{SAMPLESIZES}{The sample sizes chosen (i.e. in our case, this would be an array containing 1,5,50,100,300,800)}
  \item{NUMSUBSETSPERSAMPLESIZE}{The number of subsets for each sample size (i.e in the tutorial case, 20)}
  \item{RESULTFILENAME}{Name of the simulation results file (e.g. "trackedCells_Close.csv"). In the current version, XML and CSV files can be processed. Only required if running the first method (to process results directly). If performing this analysis over multiple timepoints, it is assumed that the timepoint follows the file name, e.g. trackedCells_Close_12.csv.}
  \item{ALTFILENAME}{In some cases, it may be relevant to read from a further results file if the initial file contains no results. This filename is set here. In the current version, XML and CSV files can be processed. Only required if running the first method (to process results directly)}
  \item{OUTPUTFILECOLSTART}{Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates. Only required if running the first method (to process results directly)}
  \item{OUTPUTFILECOLEND}{Column number in the simulation results file where the last output measure is. Only required if running the first method.}
  \item{MEASURES}{An array containing the names of the simulation output measures to be analysed. For example, in the tutorial simulation, we tracked a cells Velocity and Displacement.  Our array would contain these two strings}
  \item{AA_SIM_RESULTS}{Either - A: The name of the summary CSV file to be created by the first method (aa_summariseReplicateRuns) or B: The name of the provided CSV file that summarises the results of all runs for this analysis.}
  \item{ATESTRESULTSFILENAME}{Name of the file that will contain the A-Test scores for each sample size (created by aa_getATestResults).}
  \item{LARGEDIFFINDICATOR}{The A-Test determines there is a large difference between two sets if the result is greater than 0.2 either side of the 0.5 line.  Should this not be suitable, this can be changed here}
  \item{SUMMARYFILENAME}{Name of the file generated by aa_sampleSizeSummary, listing the maximum and median A-Test results for each sample size.}
  \item{MAXSAMPLESIZE}{The highest number of samples used.  In our example case, this would be set to 300}
  \item{SMALL}{The figure (>0.5) which is deemed a "small difference" between two sets being compared.  Vargha-Delaney set this value to 0.56 - but this can be altered here}
  \item{MEDIUM}{The figure (>0.5) which is deemed a "medium difference" between two sets being compared.  Vargha-Delaney set this value to 0.66 - but this can be altered here}
  \item{LARGE}{The figure (>0.5) which is deemed a "large difference" between two sets being compared.  Vargha-Delaney set this value to 0.73 - but this can be altered here}
  \item{GRAPHOUTPUTFILE}{Filename that should be given to the generated summary graph. This must have a PDF file extension}
  \item{TIMEPOINTS}{Implemented so this method can be used when analysing multiple simulation timepoints. If only analysing one timepoint, this should be set to NULL. If not, this should be an array of timepoints, e.g. c(12,36,48,60)}
  \item{TIMEPOINTSCALE}{Implemented so this method can be used when analysing multiple simulation timepoints. Sets the scale of the timepoints being analysed, e.g. "Hours"}
  \item{GRAPHNAME}{Used internally by the getATestResults method when producing graphs for multiple timepoints. Should not be set in function call.}
  \item{GRAPHLABEL}{Used internally by the getATestResults method when producing graphs for multiple timepoints. Should not be set in function call.}
  
}
\references{This technique is described by Read et al (2012) in their paper: Techniques for Grounding Agent-Based Simulations in the Real Domain: a case study in Experimental Autoimmune Encephalomyelitis."  The A-Test is described by by Vargha & Delaney (2000): "A critique and improvement of the CL Common Language Effect Size Statistics of McGraw and Wong"}

\examples{
# THE CODE IN THIS EXAMPLE IS THE SAME AS THAT USED IN THE TUTORIAL, AND
# THUS YOU NEED TO DOWNLOAD THE TUTORIAL DATA SET AND SET FILEPATH
# CORRECTLY TO RUN THIS

##---- Firstly, declare the parameters required for the four functions ----
library(XML)
library(spartan)

# The directory where you have extracted the example simulation results.
FILEPATH <- "/home/user/Downloads/AA_ABM/"
# The sample sizes that are to be analysed, contained within an array
SAMPLESIZES <- c(1,5,50,100,300)
# The simulation output measures to be analysed, again contained within an array
MEASURES<-c("Velocity","Displacement")
# The number of subsets used. By default use 20, as performed by Read et al in
# their published technique
NUMSUBSETSPERSAMPLESIZE<-20
# The output file containing the simulation results from that simulation run. Note
# there should be no file extension
RESULTFILENAME<-"trackedCells_Close.csv"
# Not used in this case, but this is useful in cases where two result files may
# exist (for example if tracking cells close to an area, and those further away
# two output files could be used). Here, results in a second file are processed
# if the first is blank or does not exist.
ALTFILENAME<-NULL
# Use this if simulation results are in CSV format.
# The column within the csv results file where the results start. This is useful
# as it restricts what is read in to R, getting round potential errors where the
# first column contains an agent label (as R does not read in CSV files where the
# first column contains duplicates)
OUTPUTFILECOLSTART<-10
# Use this if simulation results are in CSV format.
# Last column of the output measure results
OUTPUTFILECOLEND<-11
# File either A: created by method 1 to contain the median of each output measure 
# of each simulation run in that subset, or B: The name of the provided single 
# CSV file containing the simulation responses
AA_SIM_RESULTS<-"AA_SimResponses.csv"
# The results of the A-Test comparisons of the twenty subsets for each sample size
# are stored within an output file. This parameter sets the name of this file.
# Note no file extension. Current versions of spartan output to CSV files
ATESTRESULTSFILENAME<-"AA_ATest_Scores.csv"
# A summary file is created containing the maximum and median
# A-Test values for each sample size. This parameter sets the name of this file.
SUMMARYFILENAME<-"AA_ATestMaxAndMedians"
# The A-Test value either side of 0.5 which should be considered a 'large difference'
# between two sets of results. Use of 0.23 was taken from the Vargha-Delaney
# publication but can be adjusted here as necessary.
LARGEDIFFINDICATOR<-0.23
# A-Test values above 0.5 (no difference) which should be considered as small,
# medium, and large differences between two result sets. Used in the graph
# summarising all sample sizes.
SMALL<-0.56
MEDIUM<-0.66
LARGE<-0.73
# Name of the graph which summarises the analysis results for all sample sizes.
# Current versions of spartan output to pdf. Note no file extension
GRAPHOUTPUTFILE<-"AA_ATestMaxes.pdf"
# Timepoints being analysed. Must be NULL if no timepoints being analysed, or else
# be an array of timepoints. Scale sets the measure of these timepoints
TIMEPOINTS<-NULL; TIMEPOINTSCALE<-NULL
# Example Timepoints:
#TIMEPOINTS<-c(12,36,48,60); TIMEPOINTSCALE<-"Hours"

\dontrun{
# DONTRUN IS SET SO THIS IS NOT EXECUTED WHEN PACKAGE IS COMPILED - BUT THIS
# HAS BEEN TESTED WITH THE TUTORIAL DATA

##--- NOW RUN THE FOUR METHODS IN THIS ORDER ----

# A: RUN WHEN PROCESSING FOLDER STRUCTURE RESULTS FOR STOCHASTIC SIMULATIONS
aa_summariseReplicateRuns(FILEPATH,SAMPLESIZES,MEASURES,RESULTFILENAME,
	ALTFILENAME,OUTPUTFILECOLSTART,OUTPUTFILECOLEND,AA_SIM_RESULTS,
	TIMEPOINTS=TIMEPOINTS,TIMEPOINTSCALE=TIMEPOINTSCALE)

# B: GET A-TEST SCORES FOR ALL SAMPLE SIZES. PRODUCES A PLOT FOR ALL SAMPLE SIZES
aa_getATestResults(FILEPATH,SAMPLESIZES,NUMSUBSETSPERSAMPLESIZE,MEASURES,
	AA_SIM_RESULTS,ATESTRESULTSFILENAME,LARGEDIFFINDICATOR,
	TIMEPOINTS=TIMEPOINTS,TIMEPOINTSCALE=TIMEPOINTSCALE)

# C: SUMMARISE THESE RESULTS, OBTAINING MAX AND MIN FOR ALL SAMPLE SIZES
aa_sampleSizeSummary(FILEPATH,SAMPLESIZES,MEASURES,ATESTRESULTSFILENAME,
	SUMMARYFILENAME,TIMEPOINTS=TIMEPOINTS,TIMEPOINTSCALE=TIMEPOINTSCALE)

# D: GRAPH THE SUMMARY OF ALL SAMPLE SIZES
aa_graphSampleSizeSummary(FILEPATH,MEASURES,300,SMALL,MEDIUM,LARGE,
	SUMMARYFILENAME,GRAPHOUTPUTFILE,TIMEPOINTS=TIMEPOINTS,
	TIMEPOINTSCALE=TIMEPOINTSCALE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Aleatory}
\keyword{Analysis}
