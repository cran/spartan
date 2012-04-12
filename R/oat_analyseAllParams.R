oat_analyseAllParams <-
function(FILEPATH,PARAMETERS,BASELINE,PMIN,PMAX,PINC,MEASURES,MEDIANSFILENAME,ATESTRESULTSFILENAME)
{
	if(file.exists(FILEPATH))
	{
		print("Contrasting Simulation Responses to Baseline (oat_analyseAllParams)")

		# CONSIDER EACH PARAMETER IN TURN
		for(PARAM in 1:length(PARAMETERS))
		{
			print(paste("Comparing results to baseline behaviour for parameter: ",PARAMETERS[PARAM],sep=""))

			if(file.exists(paste(FILEPATH,"/",PARAMETERS[PARAM],"/",toString(BASELINE[PARAM]),"/",MEDIANSFILENAME,sep="")))
			{
				# READ IN THE BASELINE VALUES ON WHICH THE ALTERATIONS WILL BE COMPARED
				BASELINEFILE = paste(FILEPATH,"/",PARAMETERS[PARAM],"/",toString(BASELINE[PARAM]),"/",MEDIANSFILENAME,sep="")
				BASELINERESULT <- read.csv(BASELINEFILE,header=TRUE,sep=",")
			
				# NOW COMPARE THE RESULTS FROM THE REST OF THE VALUES SET FOR THIS PARAMETER WITH THE BASELINE USING
				# THE A-TEST
				RESULTS<-NULL
				PARAMVAL<-PMIN[PARAM]
			
				while(PARAMVAL<=PMAX[PARAM])
				{
					if(file.exists(paste(FILEPATH,"/",PARAMETERS[PARAM],"/",toString(PARAMVAL),
						"/",MEDIANSFILENAME,sep="")))
					{
						# Read in the set to be compared to the BASELINE
						COMPAREDISTFILE<-paste(FILEPATH,"/",PARAMETERS[PARAM],"/",toString(PARAMVAL),
							"/",MEDIANSFILENAME,sep="")	
				
						# NOW LOOK AT EACH MEASURE IN TURN
						ATESTALLMEASURES<-NULL
						ATESTALLMEASURES<-c(PARAMVAL)
				
						for(l in 1:length(MEASURES))
						{
							if(file.exists(COMPAREDISTFILE))
							{
								COMPAREDIST<-read.csv(COMPAREDISTFILE,header=TRUE,sep=",")
			
								ATESTVAL<- atest(BASELINERESULT[,MEASURES[l]],COMPAREDIST[,MEASURES[l]])
								ATESTNORM<-normaliseATest(ATESTVAL)
					
								# ADD RESULTS FOR THIS SAMPLE TO RESULT VECTOR
								ATESTALLMEASURES<-cbind(ATESTALLMEASURES,ATESTVAL,ATESTNORM)
							}
							else
							{
								ATESTALLMEASURES<-cbind(ATESTALLMEASURES,1,1)
							}			
	
					
						}
				
						RESULTS<-rbind(RESULTS,ATESTALLMEASURES)
						PARAMVAL<-PARAMVAL+PINC[PARAM]
					}
					else
					{
						print(paste("No results found for parameter: ",PARAMETERS[PARAM], ", value: ",PARAMVAL,sep=""))
					}
				}


				# WRITE THE A-TEST RESULTS TO FILE (ATests.csv for 1 timepoint)
				RESULTSFILE = paste(FILEPATH,"/",PARAMETERS[PARAM],"/",ATESTRESULTSFILENAME,sep="")
			
				# GENERATE COLUMN HEADERS FOR EASE OF REFERENCE LATER
				COLHEADERS<-c("ParameterVal")
		
				for(l in 1:length(MEASURES))
				{
					COLHEADERS<-cbind(COLHEADERS,paste("ATest",MEASURES[l],sep=""),paste("ATest",MEASURES[l],"Norm",sep=""))
				}
				colnames(RESULTS)<-c(COLHEADERS)
				
				write.csv(RESULTS,RESULTSFILE,quote = FALSE,row.names=FALSE)

				print(paste("A-Test scores for Parameter ",PARAMETERS[PARAM]," output to ",RESULTSFILE,sep=""))
		
			}
			else
			{
				print(paste("No baseline behaviour result file could be found for parameter: ",PARAMETERS[PARAM],sep=""))
			}
		}
	}
	else
	{
		print("The directory specified in FILEPATH does not exist. No analysis completed")
	}

}

