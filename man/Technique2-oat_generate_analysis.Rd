\name{Technique 2: One-At-A-Time - Perform Analysis of Results}
\alias{oat_processParamSubsets}
\alias{oat_csv_result_file_analysis}
\alias{oat_graphATestsForSampleSize}
\alias{oat_plotResultDistribution}
\alias{oat_countResponsesOfDesiredValue}
\title{One-At-A-Time - Perform Analysis of Results}
\description{The robustness of a simulation to parameter alteration can be determined through the use of this approach. Following the method described by Read et al in the reference below, the value of each parameter is adjusted independently, with the remaining parameters staying unchanged from their calibrated value. Distributions of simulation responses under the perturbed parameter condition are compared with those at simulation baseline/calibrated values using the Vargha-Delaney A-Test. This test gives an indication of how different the two sets of results are. The set of A-Test results for each parameter is output to a CSV file for reference. Finally, a graph for each parameter is produced, showing the A-Test result for each parameter value and each simulation output measure. In addition, from Version 2.0, an additional method has been introduced that can be applied to stochastic simulations, capable of producing a count of the number of responses for a particular parameter that are equal to a certain criteria (such as true or false). This may provide additional information concerning how a simulation is behaving.\cr
\cr
\bold{Note 1:} From Spartan 2.0, you can specify your simulation data in two ways:\cr
\bold{A} - Set folder structure (as in previous versions of Spartan): This is shown in figure OAT_Folder_Struc.png within the extdata folder of this package, and described in detail in the tutorial.\cr
\bold{B} - Single CSV file Input. From Spartan 2.0, you can specify all your results in a single CSV file. An example of this file can be found in the extdata folder of the package, named OAT_Medians.csv. Each row of this CSV file should contain the parameters upon which the simulation was run and the simulation response under those conditions. This may be a median value of those responses. There may be duplicate results for a parameter set, where the simulation has been run a number of times under the same condition (required for stochastic simulations).\cr
\bold{Note 2:} From Spartan 2.0, performing this analysis at multiple timepoints is now performed using the same method calls below. There are no additional method calls for timepoint analysis - the timepoints are specified in each method call.\cr 
\bold{Note 3:} From Spartan 2.0, this method can also process parameter values that are specified as a list, rather than an increment between a minimum and maximum value. This may be useful for analysing specific values over a large range.\cr
\cr
This technique consists of five methods:\cr
\bold{oat_processParamSubsets}: This method should only be used for stochastic simulations where the data is provided in the set folder structure (as in previous versions of Spartan). Each parameter, and all values that it has been assigned, are examined in turn. For each replicate run under those parameter conditions, the median of the simulation response is calculated. These medians for each simulation replicate, of each parameter set, are stored in a CSV file, creating the same single CSV file format that can also be provided as Spartan input. This file is named as stated in parameter CSV_FILE_NAME. This method can be performed for a number of simulation timepoints, producing these statistics for each timepoint taken.\cr
\bold{oat_csv_result_file_analysis}: This method takes either the CSV file created in the previous method or that provided by the user and analyses the impact that a change in a single parameter value has had on simulation response. This is performed by comparing the distribution of responses for a perturbed parameter condition with the distribution under baseline/calibrated conditions. This produces a CSV file, in the directory stated in FILEPATH, named as stated by parameter ATESTRESULTSFILENAME, containing the A-Test scores for all parameter conditions under which the simulation was run. An example of this file can be seen in the extdata folder of this package (OAT_ATestScores.csv). This method can be performed for a number of simulation timepoints, producing these statistics for each timepoint taken.
\bold{oat_graphATestsForSampleSize}: This takes each parameter in turn and creates a plot showing A-Test score against parameter value.  This makes it easy to determine how the effect that changing the parameter has had on simulation results.  Two examples can be found in the extdata folder of this package (OAT_chemoLowerLinearAdjust_Displacement.pdf and OAT_chemoUpperLinearAdjust.pdf).\cr
\bold{oat_plotResultDistribution}: Only applicable for stochastic simulations where the results are provided in the folder strutcure: this takes each parameter in turn, and creates a boxplot for each output measure, showing the result distribution for each value of that parameter. An example can be found in the extdata folder of this package (chemoLowerLinearAdjust_DisplacementBP.pdf).\cr
\bold{oat_countResponsesOfDesiredValue}: Counts the number of simulation responses where a output response equals a desired result, for a specified parameter. Outputs this information as a CSV file.
} 
\usage{
oat_processParamSubsets(FILEPATH,PARAMETERS,NUMRUNSPERSAMPLE,
	MEASURES,RESULTFILENAME,ALTERNATIVEFILENAME,
	OUTPUTCOLSTART,OUTPUTCOLEND,CSV_FILE_NAME,BASELINE,
	PMIN=NULL,PMAX=NULL,PINC=NULL,PARAMVALS=NULL,
	TIMEPOINTS=NULL,TIMEPOINTSCALE=NULL)

oat_csv_result_file_analysis(FILEPATH,CSV_FILE_NAME,PARAMETERS,
	BASELINE,MEASURES,ATESTRESULTFILENAME,PMIN=NULL,
	PMAX=NULL,PINC=NULL,PARAMVALS=NULL,TIMEPOINTS=NULL,
	TIMEPOINTSCALE=NULL)

oat_graphATestsForSampleSize(FILEPATH,PARAMETERS,MEASURES,
	ATESTSIGLEVEL,ATESTRESULTFILENAME,BASELINE,
	PMIN=NULL,PMAX=NULL,PINC=NULL,PARAMVALS=NULL,
	TIMEPOINTS=NULL,TIMEPOINTSCALE=NULL)

oat_plotResultDistribution(FILEPATH,PARAMETERS,MEASURES,
	MEASURE_SCALE,CSV_FILE_NAME,BASELINE,PMIN=NULL,
	PMAX=NULL,PINC=NULL,PARAMVALS=NULL,
	TIMEPOINTS=NULL,TIMEPOINTSCALE=NULL)

oat_countResponsesOfDesiredValue(FILEPATH,PARAMETERS,
	RESULTFILENAME,OUTPUTCOLSTART,OUTPUTCOLEND,
	PARAMETER,NUMRUNSPERSAMPLE,MEASURE,DESIREDRESULT,
	OUTPUTFILENAME,BASELINE,PMIN=NULL,PMAX=NULL,
	PINC=NULL,PARAMVALS=NULL,TIMEPOINTS=NULL,
	TIMEPOINTSCALE=NULL)

}
\arguments{
  \item{FILEPATH}{Directory where either the simulation runs or single CSV file result can be found}
  \item{PARAMETERS}{Array containing the names of the parameters of which parameter samples will be generated}
  \item{PMIN}{Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space}
  \item{PMAX}{Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space}
  \item{PINC}{Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10}
  \item{PARAMVALS}{Array containing a list of strings for each parameter, each string containing comma separated values that should be assigned to that parameter. Thus sampling can be performed for specific values for each parameter, rather than a uniform incremented value. This replaces the PMIN, PMAX, and PINC where this method is used}
  \item{NUMRUNSPERSAMPLE}{The number of runs performed for each parameter subset. This figure is generated through Aleatory Analysis}
  \item{MEASURES}{Array containing the names of the output measures which are used to analyse the simulation}
  \item{RESULTFILENAME}{Name of the simulation results file (e.g. "trackedCells_Close.csv"). In the current version, XML and CSV files can be processed. Only required if running the first method (to process results directly). If performing this analysis over multiple timepoints, it is assumed that the timepoint follows the file name, e.g. trackedCells_Close_12.csv.}
  \item{ALTERNATIVEFILENAME}{In some cases, it may be relevant to read from a further results file if he initial file contains no results. This filename is set here. Only required if running the first method (to process results directly).}
  \item{OUTPUTCOLSTART}{Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates. Only required if running the first method (to process results directly)}
  \item{OUTPUTCOLEND}{Column number in the simulation results file where the last output measure is. Only required if running the first method.}
  \item{CSV_FILE_NAME}{If oat_processParamSubsets is used, this analyses the results of replicate simulation runs and creates a file containing the median value of each measure for every run. This specifies what that file should be called (e.g. Medians.csv). If the CSV file is provided, this should contain the name of the provided file.}
  \item{BASELINE}{Array containing the values assigned to each of these parameters in the calibrated baseline}
  \item{ATESTRESULTFILENAME}{File name of the ATests result summary file created by oat_analyseAllParams. For one timepoint, this could be ATests.csv}
  \item{ATESTSIGLEVEL}{The A-Test determines if there is a large difference between two sets if the result is greater than 0.21 either side of the 0.5 line. Should this not be suitable, this can be changed here}
  \item{MEASURE_SCALE}{An array containing the measure used for each of the output measures (i.e. microns, microns/min).  Used to label graphs}
  \item{TIMEPOINTS}{Implemented so this method can be used when analysing multiple simulation timepoints. If only analysing one timepoint, this should be set to NULL. If not, this should be an array of timepoints, e.g. c(12,36,48,60)}
  \item{TIMEPOINTSCALE}{Implemented so this method can be used when analysing multiple simulation timepoints. Sets the scale of the timepoints being analysed, e.g. "Hours"}
  \item{PARAMETER}{Parameter of interest when counting simulation responses that meet a specific requirement}
  \item{MEASURE}{The measure of interest when counting simulation responses that meet a specific requirement}
  \item{DESIREDRESULT}{The specific requirement to match when counting simulation responses}
  \item{OUTPUTFILENAME}{CSV file name to contain the counts where simulation responses meet a specific requirement}
}
\references{This technique is described by Read et al (2011) in their paper: Techniques for Grounding Agent-Based Simulations in the Real Domain: a case study in Experimental Autoimmune Encephalomyelitis"
}
\examples{
# THE CODE IN THIS EXAMPLE IS THE SAME AS THAT USED IN THE TUTORIAL, AND
# THUS YOU NEED TO DOWNLOAD THE TUTORIAL DATA SET AND SET FILEPATH
# CORRECTLY TO RUN THIS

##---- Firstly, declare the parameters required for the four functions ----
### FIRST DECLARE THE PARAMETERS REQUIRED FOR THIS ANALYSIS:
# A: THE ROOT FILE PATH. EITHER WHERE THE SIMULATION RESPONSES ARE, OR
# WHERE A CSV FILE SUMMARISING THESE RESPONSES IS LOCATED
FILEPATH<-"/home/kieran/Downloads/OAT/RANGE/"
# B: EITHER (i) THE NAME OF THE FILE CONTAINING ALL THE SIMULATION OUTPUT
# OR (ii) THE NAME OF THE FILE THAT WILL BE CREATED TO SUMMARISE SIMULATION
# RESPONSES
CSV_FILE_NAME<-"OAT_Medians.csv"
# C: THE SIMULATION PARAMETERS BEING EXPLORED
PARAMETERS<-c("chemoLowerLinearAdjust","chemoUpperLinearAdjust")
# E: PARAMETER VALUE INFORMATION
# YOU CAN SPECIFY THIS IN TWO WAYS: (i) THE MINIMUM AND MAXIMUM OF EACH
# PARAMETER, AND THE INCREMENT OVER WHICH THE SAMPLING WAS INCREASED
# (ii) A STRING LIST OF VALUES THAT PARAMETER WAS ASSIGNED IN SIMULATION
# EXAMPLE OF (i):
PMIN<-c(0.015,0.10)
PMAX<-c(0.08,0.50)
PINC<-c(0.005,0.05)
PARAMVALS<-NULL
# EXAMPLE OF (ii)
#PARAMVALS<-c("0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.055,0.06,0.065,0.07,0.075,0.08",
#		"0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5")
# PMIN<NULL; PMAX<-NULL; PINC<-NULL
# F: BASELINE VALUES FOR ALL PARAMETERS (CALIBRATION VALUES)
BASELINE<-c(0.04,0.2)
# G: SIMULATION OUTPUT MEASURES
MEASURES<-c("Velocity","Displacement")
# H: NAME TO GIVE THE CSV FILE CONTAINING THE ATEST RESULTS FOR 
# ROBUSTNESS ANALYSIS
ATESTRESULTFILENAME<-"OAT_ATestScores.csv"
# I: A-TEST RESULT VALUE EITHER SIDE OF 0.5 AT WHICH THE DIFFERENCE BETWEEN 
#TWO SETS OF RESULTS IS SIGNIFICANT
ATESTSIGLEVEL<-0.21
# J: IF USING SPARTAN TO PROCESS THE RESULTS FROM REPLICATE RUNS, IN THE SET 
# FOLDER STRUCTURE DESCRIBED IN THE TUTORIAL PAPER, ENTER THE NUMBER OF 
# REPLICATE RUNS PERFORMED FOR EACH PARAMETER CONDITION
NUMRUNSPERSAMPLE<-300
# K: AGAIN IF PROCESSING INDIVIDUAL RUN RESULTS, NOT A FILE SUMMARISING SIM
# BEHAVIOUR, ENTER THE SIMULATION RESULT FILE NAME
RESULTFILENAME<-"trackedCells_Close.csv"
# L: USEFUL IN CASES WHERE TWO RESULT FILES MAY EXIST, AND WHERE A SECOND IS 
# PROCESSED 
# IF THERE ARE NO RESPONSES IN THE FIRST
ALTERNATIVEFILENAME<-NULL
# M: USE THIS IF SIM RESULTS ARE IN CSV FORMAT (ALSO ACCEPTS XML)
# THE COLUMN WITHIN THE CSV RESULTS FILE WHERE OUTPUT RESPONSES START. USEFUL 
# AS IT RESTRICTS WHAT IS READ INTO R, GETTING AROUND POTENTIAL ERRORS WHERE 
# THE FIRST COLUMN DUPLICATES
OUTPUTCOLSTART<-10
# N: COLUMN WHERE OUTPUT RESPONSES END
OUTPUTCOLEND<-11
# O: USED WHERE A SIMULATION IS BEING ANALYSED AT MULTIPLE TIMEPOINTS. THIS IS 
# AN ADDENDUM TO OUR R JOURNAL ARTICLE, AND INSTRUCTIONS TO DO THIS CAN BE 
# FOUND ON OUR WEBSITE
TIMEPOINTS<-NULL
TIMEPOINTSCALE<-NULL
# EXAMPLE OF TIMEPOINT STRUCTURE
#TIMEPOINTS<-c(12,36,48,60)
#TIMEPOINTSCALE<-"Hours"
# NOW RUN THE METHODS

\dontrun{
# DONTRUN IS SET SO THIS IS NOT EXECUTED WHEN PACKAGE IS COMPILED - BUT THIS
# HAS BEEN TESTED WITH THE TUTORIAL DATA

# A - FOR STOCHASTIC SIMULATIONS IN THE SET FOLDER STRUCTURE, GENERATE THE 
# MEDIAN SET FOR EACH SET OF RUNS FOR THE PARAMETER VALUE
oat_processParamSubsets(FILEPATH,PARAMETERS,NUMRUNSPERSAMPLE,MEASURES,
	RESULTFILENAME,ALTERNATIVEFILENAME,OUTPUTCOLSTART,OUTPUTCOLEND,
	CSV_FILE_NAME,BASELINE,PMIN,PMAX,PINC,PARAMVALS,TIMEPOINTS,TIMEPOINTSCALE)


# B - RUN THE ATEST FOR EACH PARAMETER VALUE, AND EACH PARAMETER
# USES EITHER THE CSV FILE GENERATED IN THE METHOD ABOVE OR ONE THAT IS 
# SUPPLIED
oat_csv_result_file_analysis(FILEPATH,CSV_FILE_NAME,PARAMETERS,BASELINE,
	MEASURES,ATESTRESULTFILENAME,PMIN,PMAX,PINC,PARAMVALS,
	TIMEPOINTS,TIMEPOINTSCALE)

# C - GRAPH THE RESULTS FOR ALL MEASURES FOR EACH PARAMETER
oat_graphATestsForSampleSize(FILEPATH,PARAMETERS,MEASURES,ATESTSIGLEVEL,
	ATESTRESULTFILENAME,BASELINE,PMIN,PMAX,PINC,PARAMVALS,TIMEPOINTS,
	TIMEPOINTSCALE)

# D - GRAPH THE DISTRIBUTION OF THE RESULTS FOR THIS MEASURE, FOR
# THIS PARAMETER
oat_plotResultDistribution(FILEPATH,PARAMETERS,MEASURES,MEASURE_SCALE,
	CSV_FILE_NAME,BASELINE,PMIN,PMAX,PINC,PARAMVALS,TIMEPOINTS,
	TIMEPOINTSCALE)

# E - COUNT THE NUMBER OF TIMES A PARAMETER (SUCH AS vcamSlope 
# PRODUCES AN OUTPUT RESPONSE OF 0 FOR AREA
oat_countResponsesOfDesiredValue(FILEPATH,PARAMETERS,RESULTFILENAME,OUTPUTCOLSTART,
	OUTPUTCOLEND,"vcamSlope",NUMRUNSPERSAMPLE,"Area",0,OUTPUTFILENAME,
	BASELINE,PMIN,PMAX,PINC,PARAMVALS,TIMEPOINTS,TIMEPOINTSCALE)
}
}
\keyword{OAT}
\keyword{Parameter}
\keyword{Analysis}
\keyword{A-Test}
