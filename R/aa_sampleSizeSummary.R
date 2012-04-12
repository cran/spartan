aa_sampleSizeSummary <-
function(FILEPATH,SAMPLESIZES,MEASURES,ATESTRESULTSFILENAME,SUMMARYFILENAME)
{
	ATESTMAXES <-NULL

	print("Producing Analysis Summary (aa_sampleSizeSummary)")

	for(k in 1:length(SAMPLESIZES))
	{
		SAMPLEPROCESSING = SAMPLESIZES[k]
		print(paste("Processing Sample Size: ",SAMPLEPROCESSING,sep=""))

		if(file.exists(paste(FILEPATH,"/",SAMPLEPROCESSING,sep="")))
		{
			SAMPLEAS<-c(toString(SAMPLEPROCESSING))

			if(file.exists(paste(FILEPATH,"/",SAMPLEPROCESSING,"/",ATESTRESULTSFILENAME,".csv",sep="")))
			{

				# get the max and median results for this sample for graphing later		
				ATESTS <- read.csv(paste(FILEPATH,"/",SAMPLEPROCESSING,"/",ATESTRESULTSFILENAME,".csv",sep=""),header=TRUE)
	
				# WORK THESE OUT FOR EACH MEASURE
				for(l in 1:length(MEASURES))
				{
					MEASURELABEL<-paste("ATest",MEASURES[l],"Norm",sep="")
					MEDIANATESTMEASUREVAL <- median(ATESTS[MEASURELABEL][,1])
					MEASURELABEL<-paste("ATest",MEASURES[l],"Norm",sep="")
					MAXATESTMEASUREVAL <- max(ATESTS[MEASURELABEL][,1])
					SAMPLEAS<-cbind(SAMPLEAS,MAXATESTMEASUREVAL,MEDIANATESTMEASUREVAL)
				}
				ATESTMAXES<-rbind(ATESTMAXES,SAMPLEAS)
			}
			else
			{
				print("Error: File specified in ATESTRESULTSFILENAME does not exist. Have you run the procedure to generate this file?")
			}
		}
		else
		{
			print(paste("Error: Cannot find the results for this sample size (",SAMPLEPROCESSING,"). Check your FILEPATH and SAMPLESIZES parameters",sep=""))
		}
	}
		
	# GENERATE COL HEADER FOR THE OUTPUT FILE
	OUTPUTCOLHEADS<-c("SampleSize")

	for(l in 1:length(MEASURES))
	{
		OUTPUTCOLHEADS<-cbind(OUTPUTCOLHEADS,paste(MEASURES[l],"MaxA",sep=""),paste(MEASURES[l],"MedianA",sep=""))
	}

	colnames(ATESTMAXES)<-c(OUTPUTCOLHEADS)

	# NOW OUTPUT THESE FOR GRAPHING LATER
	# SUMMARY FILENAME SOMETHING LIKE ATESTMAXANDMEDIANS.CSV FOR ONE TIMEPOINT
	RESULTFILE = paste(FILEPATH,"/",SUMMARYFILENAME,".csv",sep="")
	# WRITE OUT SO HAVE THE TABLE IF NECESSARY LATER
	write.csv(ATESTMAXES,RESULTFILE,quote = FALSE,row.names=FALSE)

	print(paste("Summary file of all A-Test results output to ",FILEPATH,"/",SUMMARYFILENAME,".csv",sep=""))
}

