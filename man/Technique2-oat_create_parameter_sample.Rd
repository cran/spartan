\name{Technique 2: One-At-A-Time - Generate Parameter Value Sets}
\alias{oat_parameter_sampling}
\title{Technique 2: One-At-A-Time - Generate Parameter Value Sets}
\description{The robustness of a simulation to parameter alteration can be determined through the use of this approach. Following the method described by Read et al in the reference below, the value of each parameter is adjusted independently, with the remaining parameters staying unchanged from their calibrated value. This method within the toolkit creates a set of simulation parameter sets to enable such an analysis to be performed. One CSV file is created for each parameter being examined (with the filename being [Parameter Name]_Values.csv). Each CSV file will contain the parameters for runs that need to be performed. For each set of parameters, the simulation should be run for the number of times determined by Aleatory Analysis (Technique 1). Once this has been completed, the results can be analysed using the next method included within Technique 2 of this package.  Two examples of the files produced exist in the data folder of this package (OAT_Sampling_chemoThreshold_Values.csv and OAT_Sampling_maxVCAMProababilityCutoff.csv)}
\usage{
oat_parameter_sampling(FILEPATH, PARAMETERS, BASELINE, PMIN=NULL, 
	PMAX=NULL, PINC=NULL, PARAMVALS=NULL)
}
\arguments{
  \item{FILEPATH}{Directory where the parameter samples should be output to}
  \item{PARAMETERS}{Array containing the names of the parameters of which parameter samples will be generated}
  \item{BASELINE}{Array containing the values assigned to each of these parameters in the calibrated baseline}
  \item{PMIN}{Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space}
  \item{PMAX}{Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space}
  \item{PINC}{Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10}
  \item{PARAMVALS}{Array containing a list of strings for each parameter, each string containing comma separated values that should be assigned to that parameter. Thus sampling can be performed for specific values for each parameter, rather than a uniform incremented value. This replaces the PMIN, PMAX, and PINC where this method is used.}
}
\references{This technique is described by Read et al (2012) in their paper: Techniques for Grounding Agent-Based Simulations in the Real Domain: a case study in Experimental Autoimmune Encephalomyelitis"}

\examples{
##- Firstly, declare the parameters required for the function -
# A: THE ROOT FILE PATH. 
# WHERE THE PARAMETER SAMPLES SHOULD BE STORED
FILEPATH<-"/home/kieran/Downloads/OAT/"
# B: THE SIMULATION PARAMETERS BEING EXPLORED
PARAMETERS <- c("thresholdBindProbability","chemoThreshold",
"chemoUpperLinearAdjust","chemoLowerLinearAdjust",
"maxVCAMeffectProbabilityCutoff","vcamSlope")
# C: BASELINE VALUES FOR ALL PARAMETERS (CALIBRATION VALUES). 
# THIS SETS THE VALUES OF THE OTHER PARAMETERS WHEN ANOTHER 
# IS THAT OF FOCUS IN THE ANALYSIS
BASELINE<-c(50,0.3,0.2,0.04,0.60,1.0)
# E: PARAMETER VALUE INFORMATION
# YOU CAN SPECIFY THIS IN TWO WAYS: (i) THE MINIMUM AND 
# MAXIMUM OF EACH PARAMETER, AND THE INCREMENT OVER WHICH THE 
# SAMPLING WAS INCREASED (ii) A STRING LIST OF VALUES THAT 
# PARAMETER WAS ASSIGNED IN SIMULATION
# EXAMPLE OF (i):
#PMIN<-c(0,0.10,0.10,0.015,0.1,0.25)
#PMAX<-c(100,0.9,0.50,0.08,1.0,5.0)
#PINC<-c(10,0.1,0.05,0.005,0.05,0.25)
# EXAMPLE OF (ii)
#PARAMVALS<-c("0,50,90","0.10,0.3,0.8","0.10,0.25,0.4",
#	"0.015,0.04,0.08","0.1,0.5,0.9","0.25,1.25,2.0,3.0,5.0")


##--NOW RUN THE METHOD, CHOOSING EITHER PMIN PMAX PINC OR PARAMVALS
\dontrun{
oat_parameter_sampling(FILEPATH,PARAMETERS,BASELINE,PMIN=NULL,
	PMAX=NULL,PINC=NULL,PARAMVALS=PARAMVALS)
oat_parameter_sampling(FILEPATH,PARAMETERS,BASELINE,PMIN=PMIN,
	PMAX=PMAX,PINC=PINC,PARAMVALS=NULL)
}
}
\keyword{OAT}
\keyword{Parameter}
\keyword{Sampling}
