lhc_process_sample_run_subsets <-
function(FILEPATH,NUMSAMPLES,NUMRUNSPERSAMPLE,OUTPUTCOLSTART,OUTPUTCOLEND,MEASURES,RESULTFILENAME,ALTERNATIVEFILENAME,MEDIANSFILENAME)
{
	if(file.exists(FILEPATH))
	{
		print("Generating Simulation Median Responses (lhc_process_sample_run_subsets)")

		for(k in 1:NUMSAMPLES)
		{
			print(paste("Generating Median Simulation Responses for Parameter Set ",k,sep=""))
			MEDIANFILEPATH<-paste(FILEPATH,"/",k,"/",sep="")
			
			# CHECK WHETHER THIS HAS ALREADY BEEN DONE (SAVES TIME IN ERROR CIRCUMSTANCES)
			MEDIANRESULTSFILE = paste(MEDIANFILEPATH,"/",MEDIANSFILENAME,sep="")
	
			if(!file.exists(MEDIANRESULTSFILE))
			{
				getMediansSubset(MEDIANFILEPATH,NUMRUNSPERSAMPLE,OUTPUTCOLSTART,OUTPUTCOLEND,MEASURES,
					RESULTFILENAME,ALTERNATIVEFILENAME,MEDIANSFILENAME)
			}
		}
		
	}
	else
	{
		print("The directory specified in FILEPATH does not exist. No analysis performed")
	}
}

