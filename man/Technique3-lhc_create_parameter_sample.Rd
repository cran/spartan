\name{Technique 3: Latin-Hypercube: Generate Parameter Value Sets}
\alias{lhc_generate_lhc_sample}
\title{LHC}
\description{Though Technique 2 does elucidate the effects of perturbations of one parameter, it cannot show any non-linear effects which occur when two or more are adjusted simultaneously. A Global Sensitivity Analysis technique is needed to identify such effects, and to give an indication of the parameters which have the greatest influence on the simulation output. This technique uses the method described by Read et al in their paper reference below, which uses a latin-hypercube design as described by Saltelli and others to sample the parameter space. Ranges are set for each parameter, and all parameter values perturbed concurrently. This method creates the parameter value sets with which simulations should be run. This is output as a CSV file, an example of which can be seen in the data directory of this package (LHC_Parameters_for_Runs.csv). For each set of parameters, the simulation should be run for the number of times identified in Aleatory Analysis (Technique 1). Once this has been completed, the set of remaining methods within Technique 3 of this package can be used to analyse the results. Note: To run this, you will require the lhs library.}
\usage{
lhc_generate_lhc_sample(FILEPATH,PARAMETERS,NUMSAMPLES,PMIN,PMAX,ALGORITHM)
}
\arguments{
  \item{FILEPATH}{Directory where the parameter samples should be output to}
  \item{PARAMETERS}{Array containing the names of the parameters of which parameter samples will be generated}
  \item{NUMSAMPLES}{The number of parameter subsets to generate}
  \item{PMIN}{Array containing the minimum value that should be used for each parameter. Sets a lower bound on sampling space}
  \item{PMAX}{Array containing the maximum value that should be used for each parameter. Sets an upper bound on sampling space}
  \item{ALGORITHM}{Choice of algorithm to use to generate the hypercube. Can be set to either 'normal' or 'optimum'. Beware optimum can take a long time to generate an optimised parameter set (more than 24 hours in some circumstances)}
}
\references{This technique is described by Read et al (2011) in their paper: Techniques for Grounding Agent-Based Simulations in the Real Domain: a case study in Experimental Autoimmune Encephalomyelitis", and also in the Saltelli et al book: "Senstivity Analysis"}

\examples{

##-- Firstly, declare the parameters required for the function --
# A - THE FILEPATH WHERE RESULTS SHOULD BE OUTPUT TO
FILEPATH<-"/media/FreeAgent/package_Test_Data/LHC/Sampling/"
# B - THE NAMES OF THE PARAMETERS BEING SAMPLED
PARAMETERS <- c("thresholdBindProbability","chemoThreshold",
"chemoUpperLinearAdjust","chemoLowerLinearAdjust",
"maxVCAMeffectProbabilityCutoff","vcamSlope")
# C - Number of of samples to generate from the hypercube
NUMSAMPLES <- 65
# D - THE MINIMUM VALUE OF EACH PARAMETER
PMIN<-c(0,0.10,0.10,0.015,0.1,0.25) 
# E - THE MAXIMUM VALUE OF EACH PARAMETER
PMAX<-c(100,0.9,0.50,0.08,1.0,5.0)
# F - THE ALGORITHM TO USE TO GENERATE THE HYPERCUBE
ALGORITHM <- "normal"

\dontrun{
##---- NOW RUN THE METHOD ----
# Needs the lhs library
library(lhs)
lhc_generate_lhc_sample(FILEPATH,PARAMETERS,NUMSAMPLES,PMIN,PMAX,ALGORITHM)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{latin}
\keyword{hypercube}
\keyword{parameter}
\keyword{sampling}
