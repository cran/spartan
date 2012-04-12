\name{Technique 2: One-At-A-Time - Generate Parameter Value Sets}
\alias{oat_parameter_sampling}
\title{Technique 2: One-At-A-Time - Generate Parameter Value Sets}
\description{The robustness of a simulation to parameter alteration can be determined through the use of this approach. Following the method described by Read et al in the reference below, the value of each parameter is adjusted independently, with the remaining parameters staying unchanged from their calibrated value. This method within the toolkit creates a set of simulation parameter sets to enable such an analysis to be performed. One CSV file is created for each parameter being examined (with the filename being [Parameter Name]_Values.csv). Each CSV file will contain the parameters for runs that need to be performed. For each set of parameters, the simulation should be run for the number of times determined by Aleatory Analysis (Technique 1). Once this has been completed, the results can be analysed using the next method included within Technique 2 of this package.  Two examples of the files produced exist in the data folder of this package (OAT_Sampling_chemoThreshold_Values.csv and OAT_Sampling_maxVCAMProababilityCutoff.csv)}
\usage{
oat_parameter_sampling(FILEPATH, PARAMETERS, BASELINE, PMIN, PMAX, 
       PINC)
}
\arguments{
  \item{FILEPATH}{Directory where the parameter samples should be output to}
  \item{PARAMETERS}{Array containing the names of the parameters of which parameter samples will be generated}
  \item{BASELINE}{Array containing the values assigned to each of these parameters in the calibrated baseline}
  \item{PMIN}{Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space}
  \item{PMAX}{Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space}
  \item{PINC}{Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10}
}
\references{This technique is described by Read et al (2012) in their paper: Techniques for Grounding Agent-Based Simulations in the Real Domain: a case study in Experimental Autoimmune Encephalomyelitis"}

\examples{
##---- Firstly, declare the parameters required for the function ----
# A - THE FILEPATH WHERE RESULTS SHOULD BE OUTPUT TO
FILEPATH<-"/media/FreeAgent/package_Test_Data/OAT/Sampling/"
# B - THE NAMES OF THE PARAMETERS TO BE ANALYSED
PARAMETERS <- c("thresholdBindProbability","chemoThreshold",
"chemoUpperLinearAdjust","chemoLowerLinearAdjust",
"maxVCAMeffectProbabilityCutoff","vcamSlope")
# C - THE BASELINE VALUES OF EACH PARAMETER (FROM CALIBRATION IF
# POSSIBLE
BASELINE<-c(50,0.3,0.2,0.04,0.60,1.0)
# D - THE MINIMUM VALUE OF EACH PARAMETER
PMIN<-c(0,0.10,0.10,0.015,0.1,0.25) 
# E - THE MAXIMUM VALUE OF EACH PARAMETER
PMAX<-c(100,0.9,0.50,0.08,1.0,5.0)
# F - THE INCREMENT APPLIED TO EACH PARAMETER VALUE IN THE RANGE
PINC<-c(10,0.1,0.05,0.005,0.05,0.25)

##---- NOW RUN THE METHOD ----
\dontrun{
oat_parameter_sampling(FILEPATH,PARAMETERS,BASELINE,PMIN,PMAX,
	PINC)
}
}
\keyword{OAT}
\keyword{Parameter}
\keyword{Sampling}
