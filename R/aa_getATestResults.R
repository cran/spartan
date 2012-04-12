aa_getATestResults <-
function(FILEPATH,SAMPLEPROCESSING,NUMSUBSETSPERSAMPLESIZE,MEASURES,MEDIANSFILEFORMAT,MEDIANSFILENAME,ATESTRESULTSFILENAME)
{
	# import the first distribution - this will be used as the comparison set
	# However need to check whether this is a CSV or XML file
	FILEADDRESS = paste(FILEPATH,"/",SAMPLEPROCESSING,"/",toString(1),"/",MEDIANSFILENAME,sep="")
	
	if(MEDIANSFILEFORMAT=="csv")
	{
		COMPARISONSET <- read.csv(paste(FILEADDRESS,".csv",sep=""),header=TRUE,sep=",")	
	}
	else if(MEDIANSFILEFORMAT=="xml")
	{
		COMPARISONSET<-xmlToDataFrame(paste(FILEADDRESS,".xml",sep=""))
	}
	
	RESULTS<-NULL

	for(m in 2:NUMSUBSETSPERSAMPLESIZE)
	{
		# Get the set that will be compared with set one
		FILEADDRESS = paste(FILEPATH,"/",SAMPLEPROCESSING,"/",toString(m),"/",MEDIANSFILENAME,sep="")


		if(file.exists(paste(FILEADDRESS,".csv",sep="")) | file.exists(paste(FILEADDRESS,".xml",sep="")))
		{
			if(MEDIANSFILEFORMAT=="csv")
			{
				SAMPLEMEDIANS <- read.csv(paste(FILEADDRESS,".csv",sep=""),header=TRUE,sep=",")
			}
			else if(MEDIANSFILEFORMAT=="xml")
			{
				# XML Median Set
				SAMPLEMEDIANS<-xmlToDataFrame(paste(FILEADDRESS,".xml",sep=""))
			}
				
			# start the output, label the first column with the sample set number
			ALLATESTRESULTS<-c(toString(m))
	
			# Now perform the analysis for each measure
			# THEN NORMALISE (PUT ABOVE 0.5) AS DIRECTION DOES NOT MATTER
			for(l in 1:length(MEASURES))
			{
				# ATEST IS IN ATESTR.R, AS IS NORMALISEATEST
				ATESTMEASURERESULT<-atest(as.numeric(as.matrix(COMPARISONSET[MEASURES[l]][,1])),
						as.numeric(as.matrix(SAMPLEMEDIANS[MEASURES[l]][,1])))
				# the [,1] is added so the data is extracted	
				ATESTMEASURENORM <- normaliseATest(ATESTMEASURERESULT)				
				ALLATESTRESULTS<-cbind(ALLATESTRESULTS,ATESTMEASURERESULT,ATESTMEASURENORM)
			}

			# ADD TO THE SET OF ALL RESULTS SO FAR FOR THIS SAMPLE SIZE
			RESULTS<-rbind(RESULTS,ALLATESTRESULTS)
		}
	}

	# OUTPUT THE RESULTS FOR EACH SUBSET TO THE FILE
	RESULTSFILE = paste(FILEPATH,"/",SAMPLEPROCESSING,"/",ATESTRESULTSFILENAME,".csv",sep="")

	# GENERATE COLUMN HEADINGS
	ATESTRESULTSHEADER<-c("Sample")

	for(l in 1:length(MEASURES))
	{
		ATESTRESULTSHEADER<-cbind(ATESTRESULTSHEADER,paste("ATest",MEASURES[l],sep=""),paste("ATest",MEASURES[l],"Norm",sep=""))
	}

	colnames(RESULTS)<-c(ATESTRESULTSHEADER)	

	write.csv(RESULTS,RESULTSFILE,quote = FALSE,row.names=FALSE)		
}

