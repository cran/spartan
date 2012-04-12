lhc_generatePRCoEffs <-
function(FILEPATH,PARAMETERS,MEASURES,LHCSUMMARYFILENAME,CORCOEFFSOUTPUTFILE)
{
	if(file.exists(FILEPATH))
	{
		# LHCSUMMARYFILENAME IS LHCSummary.csv FOR 1 TIMEPOINT
		# CORCOEFFSOUTPUTFILE IS corCoefs.csv FOR 1 TIMEPOINT
		if(file.exists(paste(FILEPATH,"/",LHCSUMMARYFILENAME,sep="")))
		{
			LHCRESULTFILE<-read.csv(paste(FILEPATH,"/",LHCSUMMARYFILENAME,sep=""),header=TRUE)
			PARAMCOEFFSTRUCT<-NULL
			COEFFRESULTS<-NULL
			print("Generating Partial Rank Correlation Coefficients (lhc_generatePRCoEffs)")
		
			# NEED TO GENERATE A COEFFICIENT FOR EACH PARAMETER BEING EXAMINED
			for(k in 1:length(PARAMETERS))
			{
				PARAMNAME<-PARAMETERS[k]
		
				# GET COEFFICIENT SET
				COEFFDATA<-lhc_constructCoEffDataSet(LHCRESULTFILE,PARAMNAME,PARAMETERS)
				# GET PARAMETER RESULT
				COEFFPARAMCOL<-LHCRESULTFILE[,PARAMETERS[k]]
		
				PARAMRESULTS<-NULL
				# GET MEASURE RESULTS AND CALCULATE COEFFICIENTS FOR EACH PARAMETER
				for(l in 1:length(MEASURES))
				{
					COEFFMEASURERESULT<-LHCRESULTFILE[,MEASURES[l]]
					#PARAMCOEFF<-pcor.test(COEFFPARAMCOL,COEFFMEASURERESULT,COEFFDATA,method=c("s"))
					PARAMCOEFF<-pcor.test(COEFFPARAMCOL,COEFFMEASURERESULT,COEFFDATA,calcMethod=c("s"))
					PARAMRESULTS<-cbind(PARAMRESULTS,PARAMCOEFF$estimate,PARAMCOEFF$p.value)
				}
		
				COEFFRESULTS<-rbind(COEFFRESULTS,PARAMRESULTS)		
			}
			
			# NAME THE COLUMNS FOR EASE OF REFERENCE LATER
			COEFFRESULTSHEAD<-NULL	
			for(l in 1:length(MEASURES))
			{
				COEFFRESULTSHEAD<-cbind(COEFFRESULTSHEAD,
							(paste(MEASURES[l],"_Estimate",sep="")),
							(paste(MEASURES[l],"_PValue",sep="")))
			}
	
			# OUTPUT THE RESULTS FOR ALL PARAMETERS
			colnames(COEFFRESULTS)<-c(COEFFRESULTSHEAD)
			rownames(COEFFRESULTS)<-PARAMETERS
		
			coEffsResultsFile<-paste(FILEPATH,"/",CORCOEFFSOUTPUTFILE,sep="")
			write.csv(COEFFRESULTS,coEffsResultsFile,quote = FALSE)
		
			print(paste("File of Partial Rank Correlation Coefficients Generated. Output to ",coEffsResultsFile,sep=""))
		}
		else
		{
			print("LHC Summary file cannot be found. Are you sure you have run the method to generate it?")
		}
	}
	else
	{
		print("The directory specified in FILEPATH does not exist. No Partial Rank Correlation Coefficients Generated")
	}

}

