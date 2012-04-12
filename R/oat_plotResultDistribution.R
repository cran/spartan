oat_plotResultDistribution <-
function(FILEPATH,PARAMETERS,PMIN,PMAX,PINC,MEASURES,MEASURE_SCALE,MEDIANSFILEFORMAT,MEDIANSFILENAME,TIMEPOINT,TIMEPOINTSCALE)
{
	if(file.exists(FILEPATH))
	{
		print("Plotting result distribution for each parameter (oat_plotResultDistribution)")

		for(PARAM in 1:length(PARAMETERS))
		{
			print(paste("Creating Output Responses Box Plot Graph for Parameter ",PARAMETERS[PARAM],sep=""))

			ALLRESULTS<-NULL

			for(l in 1:length(MEASURES))
			{
				PARAMVAL<-PMIN[PARAM]
				COMBINEDRESULTS<-NULL
	
				while(PARAMVAL<=PMAX[PARAM])	
				{
					# Open the medians file - this may be XML or CSV
					# Set the path to the file (without extension)
					VALUERESULTSPATH<-paste(FILEPATH,"/",PARAMETERS[PARAM],"/",toString(PARAMVAL),
						"/",MEDIANSFILENAME,sep="")

					if(file.exists(paste(VALUERESULTSPATH,".csv",sep="")) | file.exists(paste(VALUERESULTSPATH,".xml",sep="")))
					{
						if(MEDIANSFILEFORMAT=="csv")
						{
							RESULTS<-read.csv(paste(VALUERESULTSPATH,".csv",sep=""),header=TRUE)
						}
						else if(MEDIANSFILEFORMAT=="xml")
						{
							RESULTS<-xmlToDataFrame(paste(VALUERESULTSPATH,".xml",sep=""))
						}
					
						LABEL<-NULL
						RUNRESULTS<-NULL
		
						for(k in 1:length(as.numeric(as.matrix(RESULTS[,MEASURES[l]]))))
						{
							LABEL<-rbind(LABEL,PARAMVAL[1])
						}
	
						RUNRESULTS<-cbind(as.numeric(as.matrix(RESULTS[,MEASURES[l]])),LABEL)
		
						COMBINEDRESULTS<-rbind(COMBINEDRESULTS,RUNRESULTS)
					}
							
					PARAMVAL<-PARAMVAL+PINC[PARAM]
				}

				if(!is.null(COMBINEDRESULTS))
				{
					colnames(COMBINEDRESULTS)<-c(MEASURES[l],"Run")
	
					# BOXPLOT THE MEASURE
					if(is.null(TIMEPOINT))
					{
						GRAPHFILE = paste(FILEPATH,"/",PARAMETERS[PARAM],"/",PARAMETERS[PARAM],"_",
							MEASURES[l],"BP.pdf",sep="")
						GRAPHTITLE<-paste("Distribution of ",MEASURES[l], " Responses \n when altering parameter ",
							PARAMETERS[PARAM],sep="")
					}
					else
					{
						GRAPHFILE = paste(FILEPATH,"/",PARAMETERS[PARAM],"/",PARAMETERS[PARAM],"_",
							MEASURES[l],"BP_",TIMEPOINT,".pdf",sep="")
						GRAPHTITLE<-paste("Distribution of ",MEASURES[l], " Responses \n when altering parameter ",
							PARAMETERS[PARAM]," at Timepoint ",TIMEPOINT," ",TIMEPOINTSCALE,sep="")
					}
		
					pdf(GRAPHFILE)			
		
					# GENERATE YLABEL BASED ON PARAMETER MEASURE
					YLABEL<-paste("Median ",PARAMETERS[PARAM]," (",MEASURE_SCALE[PARAM],")",sep="")
					MEASURESLAB<-MEASURES[l]
				
					boxplot(COMBINEDRESULTS[,1]~COMBINEDRESULTS[,2],ylab=YLABEL,xlab="Parameter Value",main=GRAPHTITLE)
				
					dev.off()
				}
			}
			print(paste("Box Plot Generated and output as ",GRAPHFILE,sep=""))
		}
	}
	else
	{
		print("The directory specified in FILEPATH does not exist. No graph created")
	}
}

