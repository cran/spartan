efast_generate_medians_for_all_parameter_subsets <-
function(FILEPATH,NUMCURVES,PARAMETERS,NUMSAMPLES,NUMRUNSPERSAMPLE,MEASURES,OUTPUTCOLSTART,OUTPUTCOLEND,RESULTFILENAME,ALTERNATIVEFILENAME,MEDIANSFILENAME)
{
	if(file.exists(FILEPATH))
	{
		print("Generating Simulation Median Response Sets (efast_generate_medians_for_all_parameter_subsets)")
		for(CURVE in 1:NUMCURVES)				# REPRESENTS THE CURVES
		{
			# NOW LOOK AT EACH PARAMETER OF INTEREST
			for(PARAM in 1:length(PARAMETERS))
			{
				print(paste("Generating Median Simulation Results for Curve: ",CURVE," Parameter: ",PARAM,sep=""))
	
				# NOW LOOK AT EACH OF THE RUNS FOR THIS PARAMETER SET
				for(j in 1:NUMSAMPLES)
				{
					SAMPLEFILEDIR<-paste(FILEPATH,"/",CURVE,"/",PARAM,"/",j,"/",sep="")
	
					if(!file.exists(paste(SAMPLEFILEDIR,MEDIANSFILENAME,sep="")))
					{
						getMediansSubset(SAMPLEFILEDIR,NUMRUNSPERSAMPLE,OUTPUTCOLSTART,OUTPUTCOLEND,MEASURES,RESULTFILENAME,ALTERNATIVEFILENAME,MEDIANSFILENAME)
						
					}
				}
		
			}
		}
	}
	else
	{
		print("The directory specified in FILEPATH does not exist. No analysis completed")
	}
}

