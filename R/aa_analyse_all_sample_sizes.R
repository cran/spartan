aa_analyse_all_sample_sizes <-
function(FILEPATH,SAMPLESIZES,NUMSUBSETSPERSAMPLESIZE,OUTPUTFILECOLSTART,OUTPUTFILECOLEND,MEASURES,RESULTFILENAME,ALTFILENAME,MEDIANSFILENAME,ATESTRESULTSFILENAME,LARGEDIFFINDICATOR)
{
	if(file.exists(FILEPATH))
	{
		print("Analysing each Individual Sample Size (aa_analyse_all_sample_sizes)")
		for(k in 1:length(SAMPLESIZES))
		{
			SAMPLEPROCESSING = SAMPLESIZES[k]

			print(paste("Processing Sample Size: ",SAMPLEPROCESSING,sep=""))

			if(file.exists(paste(FILEPATH,"/",SAMPLEPROCESSING,"/",sep="")))
			{
				# Now generate the medians for each of the dummy parameter sets
				for(l in 1:NUMSUBSETSPERSAMPLESIZE)
				{
					fileDir<-paste(FILEPATH,"/",SAMPLEPROCESSING,"/",l,"/",sep="")
					# Get the median distributions for each subset - in MedianFunctions.R
					getMediansSubset(fileDir,SAMPLEPROCESSING,OUTPUTFILECOLSTART,OUTPUTFILECOLEND,MEASURES,RESULTFILENAME,ALTFILENAME,MEDIANSFILENAME)
				}

				# Process the distributions for this sample size - producing the A-Test result for each subset
				aa_getATestResults(FILEPATH,SAMPLEPROCESSING,NUMSUBSETSPERSAMPLESIZE,MEASURES,MEDIANSFILENAME,ATESTRESULTSFILENAME)
			
				# Draw the graph for this sample size
				GRAPHOUTPUTNAME<-paste(SAMPLEPROCESSING,"Samples.pdf",sep="")
				# LAST TWO INPUTS ARE BOTH NULL - THIS IS A TIMEPOINT BEING ANALYSED (i.e. 6) AND THE SCALE OF THIS MEASURE (i.e. HOURS) - FOR GRAPH TITLE
				aa_graphATestsForSampleSize(FILEPATH,SAMPLEPROCESSING,MEASURES,NUMSUBSETSPERSAMPLESIZE,LARGEDIFFINDICATOR,ATESTRESULTSFILENAME,GRAPHOUTPUTNAME,NULL,NULL)
			}
			else
			{
				print(paste("Error: The sample size specified (",SAMPLEPROCESSING,") does not exist",sep=""))
			}
		}
	}
	else
	{
		print("Error: The directory specified in FILEPATH does not exist")
	}
}

