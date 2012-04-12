lhc_process_sample_run_subsets <-
function(FILEPATH,NUMSAMPLES,NUMRUNSPERSAMPLE,MEASURES,RESULTFILEFORMAT,RESULTFILENAME,ALTERNATIVEFILENAME,OUTPUTCOLSTART,OUTPUTCOLEND,
		MEDIANSFILEFORMAT,MEDIANSFILENAME)
{
	if(file.exists(FILEPATH))
	{
		print("Generating Simulation Median Responses (lhc_process_sample_run_subsets)")
		
		MEDIANSFILEFORMAT<-tolower(MEDIANSFILEFORMAT)
		
		for(k in 1:NUMSAMPLES)
		{
			print(paste("Generating Median Simulation Responses for Parameter Set ",k,sep=""))
			MEDIANFILEPATH<-paste(FILEPATH,"/",k,"/",sep="")
			
			# CHECK WHETHER THIS HAS ALREADY BEEN DONE (SAVES TIME IN ERROR CIRCUMSTANCES)
			MEDIANRESULTSFILE = paste(MEDIANFILEPATH,"/",MEDIANSFILENAME,sep="")
			#print(MEDIANRESULTSFILE)
			
			
			if(!file.exists(paste(MEDIANRESULTSFILE,".",MEDIANSFILEFORMAT,sep="")))
			{
				getMediansSubset(MEDIANFILEPATH,NUMRUNSPERSAMPLE,MEASURES,RESULTFILEFORMAT,RESULTFILENAME,ALTERNATIVEFILENAME,
						OUTPUTCOLSTART,OUTPUTCOLEND,MEDIANSFILEFORMAT,MEDIANSFILENAME)
			}
		}
		
	}
	else
	{
		print("The directory specified in FILEPATH does not exist. No analysis performed")
	}
}

