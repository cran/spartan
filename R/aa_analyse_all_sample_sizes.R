aa_analyse_all_sample_sizes <-
function(FILEPATH,SAMPLESIZES,NUMSUBSETSPERSAMPLESIZE,RESULTFILEFORMAT,RESULTFILENAME,ALTFILENAME,OUTPUTFILECOLSTART,OUTPUTFILECOLEND,
		MEASURES,MEDIANSFILEFORMAT,MEDIANSFILENAME,ATESTRESULTSFILENAME,LARGEDIFFINDICATOR)
{
	# FIRSTLY COULD DO WITH CHECKING INPUT, AS WINDOWS AND LINUX DIFFER ON SLASHES
	# TO HAVE ACCEPTED THE STRING, THE USER WILL HAVE HAD TO USE ESCAPE CHARACTERS (ELSE R WOULD HAVE COMPLAINED)
	# RATHER THAN USE ESCAPE \\, CHANGE THIS TO /
	opSys<-.Platform$OS.type
	if(opSys=="windows")
	{
		# REPLACE THE SLASH IN THE FILEPATH IF PRESENT
		FILEPATH<-gsub("\\\\","/",FILEPATH)
		
	}

	if(file.exists(FILEPATH))
	{
		print("Analysing each Individual Sample Size (aa_analyse_all_sample_sizes)")
		for(k in 1:length(SAMPLESIZES))
		{
			SAMPLEPROCESSING = SAMPLESIZES[k]

			print(paste("Processing Sample Size: ",SAMPLEPROCESSING,sep=""))			

			if(file.exists(paste(FILEPATH,"/",SAMPLEPROCESSING,sep="")))
			{		
				# Now generate the medians for each of the dummy parameter sets
				for(l in 1:NUMSUBSETSPERSAMPLESIZE)
				{
					fileDir<-paste(FILEPATH,"/",SAMPLEPROCESSING,"/",l,"/",sep="")
					# Get the median distributions for each subset - in MedianFunctions.R
					
					# QUICK ERROR CHECK HERE - MAKE SURE THE USER HASN'T SPECIFIED AN EXTENSION ON THE RESULTFILENAME 
					# AND MEDIANSFILENAME, AS UNNECESSARY. IF THEY HAVE, REMOVE IT
					if((substr(RESULTFILENAME,nchar(RESULTFILENAME)-3,nchar(RESULTFILENAME)) == ".csv") | 
							(substr(RESULTFILENAME,nchar(RESULTFILENAME)-3,nchar(RESULTFILENAME)) == ".xml"))
					{
						RESULTFILENAME<-substr(RESULTFILENAME,0,nchar(RESULTFILENAME)-4)
					}
					if((substr(MEDIANSFILENAME,nchar(MEDIANSFILENAME)-3,nchar(MEDIANSFILENAME)) == ".csv") | 
							(substr(MEDIANSFILENAME,nchar(MEDIANSFILENAME)-3,nchar(MEDIANSFILENAME)) == ".xml"))
					{
						MEDIANSFILENAME<-substr(MEDIANSFILENAME,0,nchar(MEDIANSFILENAME)-4)
					}
					
					getMediansSubset(fileDir,SAMPLEPROCESSING,MEASURES,RESULTFILEFORMAT,RESULTFILENAME,ALTFILENAME,
							OUTPUTFILECOLSTART,OUTPUTFILECOLEND,MEDIANSFILEFORMAT,MEDIANSFILENAME)
					
				}

				# Process the distributions for this sample size - producing the A-Test result for each subset
				aa_getATestResults(FILEPATH,SAMPLEPROCESSING,NUMSUBSETSPERSAMPLESIZE,MEASURES,MEDIANSFILEFORMAT,
						MEDIANSFILENAME,ATESTRESULTSFILENAME)
			
				# Draw the graph for this sample size
				GRAPHOUTPUTNAME<-paste(SAMPLEPROCESSING,"Samples.pdf",sep="")
				# LAST TWO INPUTS ARE BOTH NULL - THIS IS A TIMEPOINT BEING ANALYSED (i.e. 6) AND THE SCALE OF THIS MEASURE (i.e. HOURS) - FOR GRAPH TITLE
				aa_graphATestsForSampleSize(FILEPATH,SAMPLEPROCESSING,MEASURES,NUMSUBSETSPERSAMPLESIZE,LARGEDIFFINDICATOR,
						ATESTRESULTSFILENAME,GRAPHOUTPUTNAME,NULL,NULL)
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

