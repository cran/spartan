aa_getATestResults <-
function(FILEPATH,SAMPLEPROCESSING,NUMSUBSETSPERSAMPLESIZE,MEASURES,MEDIANSFILENAME,ATESTRESULTSFILENAME)
{
	# import the first distribution - this will be used as the comparison set
	
	FILEADDRESS = paste(FILEPATH,SAMPLEPROCESSING,"/",toString(1),"/",MEDIANSFILENAME,sep="")
	COMPARISONSET <- read.csv(FILEADDRESS,header=TRUE,sep=",")

	RESULTS<-NULL

	for(m in 2:NUMSUBSETSPERSAMPLESIZE)
	{
		# Get the set that will be compared with set one
		FILEADDRESS = paste(FILEPATH,SAMPLEPROCESSING,"/",toString(m),"/",MEDIANSFILENAME,sep="")

		if(file.exists(FILEADDRESS))
		{
			SAMPLEMEDIANS <- read.csv(FILEADDRESS,header=TRUE,sep=",")

			# start the output, label the first column with the sample set number
			ALLATESTRESULTS<-c(toString(m))
	
			# Now perform the analysis for each measure
			# THEN NORMALISE (PUT ABOVE 0.5) AS DIRECTION DOES NOT MATTER
			for(l in 1:length(MEASURES))
			{
				# ATEST IS IN ATESTR.R, AS IS NORMALISEATEST
				ATESTMEASURERESULT<-atest(COMPARISONSET[MEASURES[l]][,1],SAMPLEMEDIANS[MEASURES[l]][,1])
				# the [,1] is added so the data is extracted	
				ATESTMEASURENORM <- normaliseATest(ATESTMEASURERESULT)				
				ALLATESTRESULTS<-cbind(ALLATESTRESULTS,ATESTMEASURERESULT,ATESTMEASURENORM)
			}

			# ADD TO THE SET OF ALL RESULTS SO FAR FOR THIS SAMPLE SIZE
			RESULTS<-rbind(RESULTS,ALLATESTRESULTS)
		}
	}

	# OUTPUT THE RESULTS FOR EACH SUBSET TO THE FILE
	RESULTSFILE = paste(FILEPATH,"/",SAMPLEPROCESSING,"/",ATESTRESULTSFILENAME,sep="")

	# GENERATE COLUMN HEADINGS
	ATESTRESULTSHEADER<-c("Sample")

	for(l in 1:length(MEASURES))
	{
		ATESTRESULTSHEADER<-cbind(ATESTRESULTSHEADER,paste("ATest",MEASURES[l],sep=""),paste("ATest",MEASURES[l],"Norm",sep=""))
	}

	colnames(RESULTS)<-c(ATESTRESULTSHEADER)	

	write.csv(RESULTS,RESULTSFILE,quote = FALSE,row.names=FALSE)		
}

