\name{Technique 4: eFAST - Generate Parameter Value Sets}
\alias{efast_generate_sample}
\title{eFAST}
\description{This technique analyses simulation results generated through parametering using the eFAST approach (extended Fourier Amplitude Sampling Test, Saltelli et al, reference below). This perturbs the value of all parameters at the same time, with the aim of partitioning the variance in simulation output between input parameters. Values for each parameter are chosen using fourier frequency curves through a parameters potential range of values. A selected number of values are selected from points along the curve. Though all parameters are perturbed simultaneously, the method does focus on one parameter of interest in turn, by giving this a very different sampling frequency to that assigned to the other parameters. Thus for each parameter of interest in turn, a sampling frequency is assigned to each parameter and values chosen at points along the curve. So a set of simulation parameters then exists for each parameter of interest.  As this is the case, this method can be computationally expensive, especially if a large number of samples is taken on the parameter search curve, or there are a large number of parameters. On top of this, to ensure adequate sampling each curve is also resampled with a small adjustment to the frequency, creating more parameter sets on which the simulation should be run. This attempts to limit any correlations and limit the effect of repeated parameter value sets being chosen. Thus, for a system where 8 parameters are being analysed, and 3 different sample curves used, 24 different sets of parameter value sets will be produced. Each of these 24 sets then contains the parameter values chosen from the frequency curves. This number of samples should be no lower than 65 (see the Marino paper for an explanation of how to select sample size). This method performs sampling of parameter space using the eFAST approach. In the data folder of this package, there are seven example files, produced for 7 parameters over one resample curve. These filenames all begin Curve1_[ParameterName].
}
\usage{
efast_generate_sample(FILEPATH, NUMCURVES, NUMSAMPLES, PARAMETERS, 
      PMIN, PMAX)
}
\arguments{
  \item{FILEPATH}{Directory where the parameter samples should be output to}
  \item{NUMCURVES}{The number of 'resamples' to perform (see eFAST documentation) - recommend using at least 3}
  \item{NUMSAMPLES}{The number of parameter subsets to generate - should be at least 65 for eFAST}
  \item{PARAMETERS}{Array containing the names of the parameters of which parameter samples will be generated.  For eFAST, remember to add a parameter named 'Dummy'}
  \item{PMIN}{Array containing the minimum value that should be used for each parameter and the dummy. Sets a lower bound on sampling space}
  \item{PMAX}{Array containing the maximum value that should be used for each parameter and the dummy. Sets an upper bound on sampling space}
}
\references{
For detailed information on how eFAST works, see either of the following: (a) Marino et al (2008): "A methodology for performing global uncertainty and sensitivity analysis in systems biology", (b) Saltelli et al (2000): "Sensitivity Analysis".  MATLAB code is also available via an associated site stated in (a)
}
\examples{
# THE CODE IN THIS EXAMPLE IS THE SAME AS THAT USED IN THE TUTORIAL, AND
# THUS YOU NEED TO DOWNLOAD THE TUTORIAL DATA SET AND SET FILEPATH
# CORRECTLY TO RUN THIS

##----Firstly, declare the parameters required for the function--
# A - FILEPATH OF WHERE THE PARAMETER DESIGN SHOULD BE OUTPUT TO
FILEPATH<-"/media/FreeAgent/package_Test_Data/eFAST/Sampling/"
# B - NUMBER OF RESAMPLES (SEARCH CURVES) TO EMPLOY
NUMCURVES <- 3
# C: NUMBER OF TIMES TO SAMPLE EACH CURVE 
# (i.e. NUMBER OF PARAMETER SUBSETS)
NUMSAMPLES <- 65
# D: PARAMETERS, IN ORDER, TRY TO AVOID SPACES, AND REMEMBER DUMMY
PARAMETERS<-c("thresholdBindProbability","chemoThreshold",
"chemoUpperLinearAdjust","chemoLowerLinearAdjust",
"maxVCAMeffectProbabilityCutoff","vcamSlope","Dummy")
# E: ARRAY OF THE MINIMUM VALUES OF EACH PARAMETER (AND DUMMY)
PMIN<-c(0,0.1,0.10,0.015,0.1,0.25,1) 
# F: ARRAY OF THE MAXIMUM VALUES OF EACH PARAMETER (AND DUMMY)
PMAX<-c(100,0.9,0.50,0.08,1.0,5.0,10)

\dontrun{
# DONTRUN IS SET SO THIS IS NOT EXECUTED WHEN PACKAGE IS COMPILED - BUT THIS
# HAS BEEN TESTED WITH THE TUTORIAL DATA

##---- NOW RUN THE METHOD ----
efast_generate_sample(FILEPATH,NUMCURVES,NUMSAMPLES,PARAMETERS,
PMIN,PMAX)
}
}
\keyword{eFAST}
\keyword{fourier}
\keyword{amplitude}
\keyword{sampling}
\keyword{parameter}
\keyword{analysis}
