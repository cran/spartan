\name{Technique 1: Aleatory Analysis}
\alias{aa_analyse_all_sample_sizes}
\alias{aa_sampleSizeSummary}
\alias{aa_graphSampleSizeSummary}
\title{Technique 1: Aleatory Analysis}
\description{
Aleatory uncertainty is caused by inherent stochasticity within a simulation. To reduce this, a number of replicate simulation runs need to be performed, thus achieving a more representative result. This technique indicates the number of simulation runs necessary to reduce this uncertainty. This follows the method described by Read et al in the reference below. To use this, you should have chosen the number of runs you want to compare (for example, 1,5,50,100,300,500,and 800). For each sample size, you should choose a number of subsets (for example, 20). Then, you should create this number of subsets of runs for each sample size. So, choosing the values from the examples, we have 20 sets where the simulation was run once, 20 sets where each set contains the results of 5 runs, right through to 20 sets where each contains the results of 800 runs. This method then looks at each sample size used, and (a) generates the median distribution for all output measures for each of the subsets (b) goes through each of the subsets (20 in the example case), comparing the median of each measure with the respective median in the first subset, using the Vargha-Delaney A-Test (reference below) to give an indication of how different the results are, (c) for each sample size, creates graphes showing how different the results of each subset are (i.e. the A-Test result). A summary of the A-Test results is also output as a Comma Separated Value file, an example of which can be found in the data folder of this package (AA_Example_ATestMaxAndMedians.csv). A full tutorial on using this technique, along with example simulation output to use, can be found on the project website.\cr
\cr
\bold{Note} that to use this analysis tool, simulation output needs to be structured in a set format. This is shown in a figure (AA_Folder_Struc.png) within the extdata folder of this package, and described in detail in the tutorial. By default, this R package expects the results to be organised as follows. The parameter FILEPATH should point to a directory which contains a folder that in turn contains the results for the sample sizes being analysed. For example, if the sample sizes being analysed were 1, 5, 50, 100, 300, 500, and 800, the folder specified by FILEPATH would contain seven folders, one for each of these sample sizes. The folder for each sample size then contains one folder for each of the result subsets. In the example case, this would be 20 folders, numbered 1-20. Each of these folders will contain the number of simulation runs performed for the sample size being analysed. For example, if the uncertainty of 5 simulation runs is being examined, each of the 20 folders will contain the results from 5 simulation runs. If 100 runs were being examined, each of the 20 folders would contain the results from 100 runs, and so on. The folders containing the results from each run should be numbered from 1 to the number of runs performed. For a clearer explanation, see figure AA_Folder_Struc.png in the extdata folder of this package, or the tutorial.\cr
\cr
There are three methods available:\cr
\bold{aa_analyse_all_sample_sizes}: Independently examines each sample size to be examined, and determines how 'different' the results of each of the subsets are.  As a first step, the algorithm needs to go through each subset and create a set of medians which can then be compared with another of the 20 subsets. Therefore, the algorithm takes each of the 20 sets in turn, producing a file containing the median output measures from each simulation run in that set. This may be easier to understand with the use of an example. Lets say we are analysing the uncertainty in 5 simulation runs. So our sample size is 5. For this sample size, we will have 20 sets of results, each containing 5 runs. For each of these 20 sets, a file is produced containing the median output for all measures for each of the 5 runs. (so if we had two output measures, Velocity and Displacement, a file would be created within each of the 20 sets containing the median Velocity and Displacement measures for each of the 5 runs). For an example, see the extdata folder of this package - where there is a zip file (Example_Simulation_Replicate_and_Medians.zip) that contains an example of one of these 20 subsets, the 5 simulation runs, and the generated Medians.csv file. Once this process has been completed, the median results for subsets 2-20 are compared with those in subset 1 using the Vargha-Delaney A-Test, with these results stored in a CSV file. The A-Test results are then graphed, showing how different each of the 20 subsets are. An example graph can be seen in the extdata folder of this package (AA_5Samples.pdf).\cr
\bold{aa_sampleSizeSummary}: This takes each sample size to be examined in turn, and iterates through all 20 A-Test results, determining the median and maximum A-Test score observed for each sample size. A CSV file is created summarising the median and maximum A-Test scores for all sample sizes. \cr
\bold{aa_graphSampleSizeSummary}: Produces a full graph of the data generated by the above method (by full, we mean the y-axis (the A-Test score) goes from 0-1, and the x axis contains all sample sizes examined), making it easy to see how uncertainty reduces with an increase in sample size. An example can be seen in the extdata folder of this package (AA_Results.pdf).
}
\usage{
aa_analyse_all_sample_sizes(FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE,
	RESULTFILEFORMAT, RESULTFILENAME, ALTFILENAME, OUTPUTFILECOLSTART, 
	OUTPUTFILECOLEND, MEASURES, MEDIANSFILEFORMAT,MEDIANSFILENAME, 
	ATESTRESULTSFILENAME, LARGEDIFFINDICATOR)
aa_sampleSizeSummary(FILEPATH,SAMPLESIZES,MEASURES,ATESTRESULTSFILENAME,
	SUMMARYFILENAME)
aa_graphSampleSizeSummary(FILEPATH,MEASURES,MAXSAMPLESIZE,SMALL,MEDIUM,
	LARGE,SUMMARYFILENAME,GRAPHOUTPUTFILE,TIMEPOINT,TIMEPOINTSCALE)

}
\arguments{
  \item{FILEPATH}{Directory where the results of the simulation runs can be found}
  \item{SAMPLESIZES}{The sample sizes chosen (i.e. in our case, this would be an array containing 1,5,50,100,300,800)}
  \item{NUMSUBSETSPERSAMPLESIZE}{The number of subsets for each sample size (i.e in the tutorial case, 20)}
  \item{RESULTFILEFORMAT}{The file format output by the simulation. Spartan can process XML or CSV output formats}
  \item{RESULTFILENAME}{Name of the simulation results file (e.g. "trackedCells_Close.csv")}
  \item{ALTFILENAME}{In some cases, it may be relevant to read from a further results file if the initial file contains no results. This filename is set here}
  \item{OUTPUTFILECOLSTART}{Used for CSV file formats. Column number in the results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates}
  \item{OUTPUTFILECOLEND}{Used for CSV file formats. Column number in the results file where the last output measure is}
  \item{MEASURES}{An array containing the names of the simulation output measures to be analysed. For example, in our simulation, we tracked a cells Velocity, Length, Displacement, and Meandering Index.  Our array would contain these four strings}
  \item{MEDIANSFILEFORMAT}{File format for the median distribution files produced from simulation results. Can be either XML or CSV}
  \item{MEDIANSFILENAME}{A name for the files generated by aa_analyse_all_sample_sizes, which contain the results of median of each output measure for every run performed in the subset. (e.g. Medians.csv).  For example, where the sample size is 5, each of the 20 subsets will contain 5 runs.  aa_analyse_all_sample_sizes creates a file within each of the 20 subsets, containing the median output measures from each of the 5 runs.}
  \item{ATESTRESULTSFILENAME}{Name of the file contained within each sample size which contains the A-Test results for each of the 20 subsets (created by aa_analyse_all_sample_sizes). For one timepoint, by default this could be ATests}
  \item{LARGEDIFFINDICATOR}{The A-Test determines there is a large difference between two sets if the result is greater than 0.2 either side of the 0.5 line.  Should this not be suitable, this can be changed here}
  \item{SUMMARYFILENAME}{Name of the file generated by aa_sampleSizeSummary, listing the A-Test results for each sample size. Example - ATestsMaxAndMedians for one timepoint}
  \item{MAXSAMPLESIZE}{The highest number of samples used.  In our example case, this would be set to 800}
  \item{SMALL}{The figure (>0.5) which is deemed a "small difference" between two sets being compared.  Vargha-Delaney set this value to 0.56 - but this can be altered here}
  \item{MEDIUM}{The figure (>0.5) which is deemed a "medium difference" between two sets being compared.  Vargha-Delaney set this value to 0.66 - but this can be altered here}
  \item{LARGE}{The figure (>0.5) which is deemed a "large difference" between two sets being compared.  Vargha-Delaney set this value to 0.73 - but this can be altered here}
  \item{GRAPHOUTPUTFILE}{Filename that should be given to the generated summary graph. This must have a PDF file extension}
  \item{TIMEPOINT}{Implemented so this method can be used when analysing multiple simulation timepoints.  NULL in this case.  See Technique 1: Aleatory Analysis Over Time}
  \item{TIMEPOINTSCALE}{Implemented so this method can be used when analysing multiple simulation timepoints.  NULL in this case.  See Technique 1: Aleatory Analysis Over Time}
  
}
\references{This technique is described by Read et al (2012) in their paper: Techniques for Grounding Agent-Based Simulations in the Real Domain: a case study in Experimental Autoimmune Encephalomyelitis."  The A-Test is described by by Vargha & Delaney (2000): "A critique and improvement of the CL Common Language Effect Size Statistics of McGraw and Wong"}

\examples{
# THE CODE IN THIS EXAMPLE IS THE SAME AS THAT USED IN THE TUTORIAL, AND
# THUS YOU NEED TO DOWNLOAD THE TUTORIAL DATA SET AND SET FILEPATH
# CORRECTLY TO RUN THIS

##---- Firstly, declare the parameters required for the four functions ----
library(XML)
# Import the package
FILEPATH <- "/media/FreeAgent/spartan_tool/spartan_test/AA/"
# The directory where you have extracted the example simulation results.
# Make sure this ends with a '/'
SAMPLESIZES <- c(1,5,50,100,300)
# The sample sizes that are to be analysed, contained within an array
MEASURES<-c("Velocity","Displacement")
# The simulation output measures to be analysed, again contained within an array
NUMSUBSETSPERSAMPLESIZE<-20
# The number of subsets used. By default use 20, as performed by Read et al in
# their published technique
RESULTFILEFORMAT<-"csv"
# Format of simulation output files. Can be csv or xml
RESULTFILENAME<-"trackedCells_Close_Endpoint"
# The output file containing the simulation results from that simulation run
# This can be CSV or XML format. Don't specify file extension
ALTFILENAME<-NULL
# Not used in this case, but this is useful in cases where two result files may
# exist (for example if tracking cells close to an area, and those further away
# two output files could be used). Here, results in a second file are processed
# if the first is blank or does not exist. Don't specify file extension
OUTPUTFILECOLSTART<-10
# Specify this if importing from a CSV file
# The column within the csv results file where the results start. This is useful
# as it restricts what is read in to R, getting round potential errors where the
# first column contains an agent label (as R does not read in CSV files where the
# first column contains duplicates)
OUTPUTFILECOLEND<-11
# Specify this if importing from a CSV file
# Last column of the output measure results
MEDIANSFILEFORMAT<-"csv"
# Format of the median distribution file produced. Either csv or xml
MEDIANSFILENAME<-"Medians"
# For each of the twenty subsets, a file is created containing the median of each
# output measure, of each simulation run in that subset. This parameter sets the
# name of this file.
ATESTRESULTFILENAME<-"ATests"
# The results of the A-Test comparisons of the twenty subsets for each sample size
# are stored within a csv file. This parameter sets the name of this file. There
# is no need to set a file extension
SUMMARYFILENAME<-"ATestMaxAndMedians"
# In this example, we will have created an EgSet_ATests.csv file for sample sizes
# 1,5,50,100 and 300. A summary file is created containing the maximum and median
# A-Test values for each sample size. This parameter sets the name of this file.
# There is no need to set a file extension
LARGEDIFFINDICATOR<-0.23
# The A-Test value either side of 0.5 which should be considered a "large difference"
# between two sets of results. Use of 0.23 was taken from the Vargha-Delaney
# publication but can be adjusted here as necessary.
SMALL<-0.56
MEDIUM<-0.66
LARGE<-0.73
# A-Test values above 0.5 (no difference) which should be considered as small,
# medium, and large differences between two result sets. Used in the graph
# summarising all sample sizes.
GRAPHOUTPUTFILE<-"ATestMaxes"
# Name of the graph which summarises the analysis results for all sample sizes
TIMEPOINTS<-NULL; TIMEPOINTSCALE<-NULL
# Not used in this case, but when a simulation is analysed at multiple timepoints
# (see later in tutorial 1)

\dontrun{
# DONTRUN IS SET SO THIS IS NOT EXECUTED WHEN PACKAGE IS COMPILED - BUT THIS
# HAS BEEN TESTED WITH THE TUTORIAL DATA

##--- NOW RUN THE FOUR METHODS IN THIS ORDER ----
# A - Calculate the medians & A-Test Files/Graphs for each subset of each 
# sample size

aa_analyse_all_sample_sizes(FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE, 
	RESULTFILEFORMAT, RESULTFILENAME, ALTFILENAME, OUTPUTFILECOLSTART, 
	OUTPUTFILECOLEND, MEASURES, MEDIANSFILEFORMAT,MEDIANSFILENAME, 
	ATESTRESULTFILENAME, LARGEDIFFINDICATOR)

# B - NOW DO THE SUMMARY FOR EACH SAMPLE SIZE
aa_sampleSizeSummary(FILEPATH,SAMPLESIZES,MEASURES,ATESTRESULTFILENAME,
	SUMMARYFILENAME)

# C - GRAPH THE RESULTS FOR ALL SAMPLE SIZES
aa_graphSampleSizeSummary(FILEPATH,MEASURES, 300,SMALL, MEDIUM, LARGE, 
	SUMMARYFILENAME, GRAPHOUTPUTFILE, TIMEPOINTS,TIMEPOINTSCALE)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Aleatory}
\keyword{Analysis}
