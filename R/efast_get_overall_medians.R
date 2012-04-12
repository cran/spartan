efast_get_overall_medians <-
function(FILEPATH,NUMCURVES,PARAMETERS,NUMSAMPLES,MEASURES,MEDIANSFILEFORMAT,MEDIANSFILENAME,CURVERESULTSFILENAME)
{
	if(file.exists(FILEPATH))
	{
		print("Calculating overall medians responses for each parameter set (efast_get_overall_medians)")
		# CURVERESULTSFILENAME IS ALLRESULTS.CSV FOR ONE TIMEPOINT
		# ALL RESULTS - WILL HOLD THE MEDIAN VALUE RESULTS FOR ALL RUNS
		# IN ORDER ON ROW - 
		for(CURVE in 1:NUMCURVES)			# CURVE
		{
			ALLRESULTS<-NULL

			print(paste("Generating results summary for Curve ",CURVE,sep=""))

			for(SAMPLE in 1:NUMSAMPLES)
			{
				ALLRESULTSROW<-NULL
				
				for(PARAM in 1:length(PARAMETERS))
				{
					# READ IN THE MEDIANS FILE FOR THIS PARAMETER SET
					MEDIANSFILEPATH<-paste(FILEPATH,"/",CURVE,"/",PARAM,"/",SAMPLE,sep="")
					
					if(file.exists(paste(MEDIANSFILEPATH,"/",MEDIANSFILENAME,".csv",sep="")) | 
							file.exists(paste(MEDIANSFILEPATH,"/",MEDIANSFILENAME,".xml",sep="")))
					{
						if(MEDIANSFILEFORMAT=="csv")
						{
							MODELRESULT<-read.csv(paste(MEDIANSFILEPATH,"/",MEDIANSFILENAME,".csv",sep=""),header=TRUE)
						}
						else if(MEDIANSFILEFORMAT=="xml")
						{
							# XML Median Set
							MODELRESULT<-xmlToDataFrame(paste(MEDIANSFILEPATH,"/",MEDIANSFILENAME,".xml",sep=""))
						}
		
						# FOR THIS PARAMETER SET, GENERATE THE OVERALL MEDIANS FOR EACH OF MEASURE
						MEDIANSFORALLMEASURES<-NULL
						
						for(l in 1:length(MEASURES))
						{
							MEASURE_RESULT<-as.matrix(MODELRESULT[MEASURES[l]])
							MEASUREMEDIAN<-median(as.numeric(MEASURE_RESULT),na.rm=TRUE)
							MEDIANSFORALLMEASURES<-cbind(MEDIANSFORALLMEASURES,MEASUREMEDIAN[[1]])
							
							#MEASUREMEDIAN <- median(ALLMEDIANS[MEASURES[l]][,1],na.rm=TRUE)
							#MEDIANSFORALLMEASURES<-cbind(MEDIANSFORALLMEASURES,MEASUREMEDIAN)
						}
		
						# APPEND THESE TO THE ALL RESULTS NEW ROW
						ALLRESULTSROW<-cbind(ALLRESULTSROW,MEDIANSFORALLMEASURES)				
					}
					else
					{
						print(paste("No simulation responses found for Curve ",CURVE," Parameter ",PARAM," Sample Set ",SAMPLE,sep=""))
					}
				}
				
				#print(ALLRESULTSROW)
				if(!is.null(ALLRESULTSROW))
				{
					ALLRESULTS<-rbind(ALLRESULTS,ALLRESULTSROW)
				}
	
			}
	
			# NOW WRITE THE RESULTS FOR THIS CURVE TO A FILE
			ALLRESULTSFILE = paste(FILEPATH,"/",CURVE,"/",CURVERESULTSFILENAME,".csv",sep="")
	
			# GENERATE THE COLHEADERS FOR PRESENTATION SAKE
			COLHEADERS<-NULL
			for(p in 1:length(PARAMETERS))
			{
				for(m in 1:length(MEASURES))
				{
					COLHEADERS<-cbind(COLHEADERS,paste(PARAMETERS[p],"_Median",MEASURES[m],sep=""))
				}
			}
	
			colnames(ALLRESULTS)<-COLHEADERS
			write.csv(ALLRESULTS,ALLRESULTSFILE,quote = FALSE,row.names=FALSE)
			print(paste("Summary file for Curve ",CURVE," Complete. Output to ",ALLRESULTSFILE,sep=""))
		}
	}
	else
	{
		print("The directory specified in FILEPATH does not exist. No analysis completed")
	}
}

