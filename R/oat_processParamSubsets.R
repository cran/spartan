oat_processParamSubsets <-
function(FILEPATH,PARAMETERS,PMIN,PMAX,PINC,NUMRUNSPERSAMPLE,MEASURES,OUTPUTCOLSTART,OUTPUTCOLEND,RESULTSFILENAME,ALTERNATIVEFILENAME,MEDIANSFILENAME)
{
	# CREATE THE MEDIAN DISTRIBUTION OVER THE SET OF RUNS FOR EACH PARAMETER SET, FOR EACH PARAMETER (AS THIS IS ONE AT A TIME)
	# LIKE ALL THE OTHER FUNCTIONS, USES MEDIANFUNCTIONS.R TO CREATE THIS

	if(file.exists(FILEPATH))
	{
		print("Generating Median Responses (oat_processParamSubsets)")

		for(PARAM in 1:length(PARAMETERS))
		{
			# SET THE VALUE TO ITS LOWEST LIMIT
			PARAMVAL<-PMIN[PARAM]
		
			if(file.exists(paste(FILEPATH,"/",PARAMETERS[PARAM],sep="")))
			{
				# NOW WORK UPWARDS UNTIL THE UPPER LIMIT IS HIT FOR THIS PARAMETER
				while(PARAMVAL<=PMAX[PARAM])
				{
					if(file.exists(paste(FILEPATH,"/",PARAMETERS[PARAM],"/",toString(PARAMVAL),sep="")))
					{
						print(paste("Generating Median Results for Parameter: ",PARAMETERS[PARAM],", Value: ",PARAMVAL,sep=""))

						# CREATE THE START OF THE FILE ADDRESS WHERE THE RESULTS FOR THIS PARAMETER ARE
						SAMPLEFILEPATH <- paste(FILEPATH,"/",PARAMETERS[PARAM],"/",toString(PARAMVAL),"/",sep="")
		
						RESULTS<-NULL
		
						# NOW CALL THE MEDIAN FUNCTIONS METHOD TO GET THE DISTRIBUTION FOR THIS SET OF RUNS
						getMediansSubset(SAMPLEFILEPATH,NUMRUNSPERSAMPLE,OUTPUTCOLSTART,OUTPUTCOLEND,MEASURES,RESULTSFILENAME,ALTERNATIVEFILENAME,MEDIANSFILENAME)
		
						PARAMVAL<-PARAMVAL+PINC[PARAM]
					}
					else
					{
						print(paste("No results can be found for parameter: ",PARAMETERS[PARAM]," Value: ",PARAMVAL,sep=""))
					}
				}
			}
			else
			{
				print(paste("No results can be found for the parameter specified: ",PARAMETERS[PARAM],sep=""))
			}
		}
	}
	else
	{
		print("The directory specified in FILEPATH does not exist. No analysis completed")
	}
}

