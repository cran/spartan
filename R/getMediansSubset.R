getMediansSubset <-
function(FILEPATH,NUMRUNSPERSAMPLE,MEASURES,RESULTFILEFORMAT,RESULTFILENAME,ALTFILENAME,OUTPUTFILECOLSTART,OUTPUTFILECOLEND,
		MEDIANSFILEFORMAT,MEDIANSFILENAME)
{
	
	RESULTS<-NULL
	RESULTFILEFORMAT<-tolower(RESULTFILEFORMAT)

	for(i in 1:NUMRUNSPERSAMPLE)
	{
		FILEADDRESS = paste(FILEPATH,toString(i),"/",RESULTFILENAME,sep="")
		#print(FILEADDRESS)
	
		if(file.exists(paste(FILEADDRESS,".",RESULTFILEFORMAT,sep="")))
		{
			# CHECK WHAT KIND OF INPUT FILE IS BEING DEALT WITH
			if(RESULTFILEFORMAT=="csv")
			{
				# DEALING WITH A CSV FILE
				# import model result
				if(OUTPUTFILECOLSTART>1)
				{
					import<-read.csv(paste(FILEADDRESS,".csv",sep=""),colClasses=c(rep('NULL',OUTPUTFILECOLSTART-1),rep(NA,OUTPUTFILECOLEND-OUTPUTFILECOLSTART+1)))
				}	
				else
				{
					import<-read.csv(paste(FILEADDRESS,".csv",sep=""),colClasses=c(rep(NA,OUTPUTFILECOLEND)))
				}

				#com <- paste("cut -d, -f",OUTPUTFILECOLSTART,"-",OUTPUTFILECOLEND," ",FILEADDRESS,".csv",sep="")
				#import<-read.csv(pipe(com))
				MODELRESULT<-data.frame(import)
			}else if(RESULTFILEFORMAT=="xml")
			{
				MODELRESULT<-xmlToDataFrame(paste(FILEADDRESS,".xml",sep=""))
			}
			
			if(nrow(MODELRESULT)>0)
			{
				MEDIANSFORALLMEASURES<-NULL
	
				# NOW GET THE MEDIANS FOR EACH MEASURE
				for(q in 1:length(MEASURES))
				{
					# STORE JUST THE RESULTS FOR THAT MEASURE (sapply CAN THEN BE USED)
					MEASURE_RESULT<-as.matrix(MODELRESULT[MEASURES[q]])
					MEASUREMEDIAN<-median(as.numeric(MEASURE_RESULT))
					MEDIANSFORALLMEASURES<-cbind(MEDIANSFORALLMEASURES,MEASUREMEDIAN[[1]])
				}
				RESULTS<-rbind(RESULTS,MEDIANSFORALLMEASURES)
			}
			else		
			{
				if(!is.null(ALTFILENAME))
				{
					# USE THE ALTERNATIVE ON THIS OCCASION
					# ASSUMES IN SAME FORMAT AS ORIGINAL
					FILEADDRESS = paste(FILEPATH,toString(i),"/",ALTFILENAME,sep="")
					# import model result - again checking input type
					
					if(RESULTFILEFORMAT=="csv")
					{
						# DEALING WITH A CSV FILE
						com <- paste("cut -d, -f",OUTPUTFILECOLSTART,"-",OUTPUTFILECOLEND," ",FILEADDRESS,".csv",sep="")
						import<-read.csv(pipe(com))
						MODELRESULT<-data.frame(import)
					}
					else if(RESULTFILEFORMAT=="xml")
					{
						MODELRESULT<-xmlToDataFrame(paste(FILEADDRESS,".xml",sep=""))
					}
					
					if(nrow(MODELRESULT)>0)
					{
						MEDIANSFORALLMEASURES<-NULL
		
						# NOW GET THE MEDIANS FOR EACH MEASURE
						for(q in 1:length(MEASURES))
						{
							# STORE JUST THE RESULTS FOR THAT MEASURE (AS, INCASE OF XML, THIS WILL NEED TO BE
							# TRANSFORMED TO A MATRIX
							# IT WOULD BE NICE TO SPECIFY THE XML FIELD TYPES BUT THIS IS DIFFICULT TO DO WHEN INPUT
							# NEEDS TO BE GENERIC
							MEASURE_RESULT<-as.matrix(MODELRESULT[MEASURES[q]])
							MEASUREMEDIAN<-median(as.numeric(MEASURE_RESULT))
							# MEASUREMEDIAN<-sapply(MEASURE_RESULT,median)
							MEDIANSFORALLMEASURES<-cbind(MEDIANSFORALLMEASURES,MEASUREMEDIAN[[1]])
						}
						RESULTS<-rbind(RESULTS,MEDIANSFORALLMEASURES)
					}
				}
			}
		}
		else
		{
			print(paste("File ",FILEADDRESS," does not exist",sep=""))
		}
	}

	# OUTPUT IF THE RESULTS ARE NOT BLANK
	if(!is.null(RESULTS))
	{
		colnames(RESULTS)<-MEASURES
		RESULTSFILE = paste(FILEPATH,"/",MEDIANSFILENAME,sep="")

		if(MEDIANSFILEFORMAT=="csv")
		{
			write.csv(RESULTS,paste(RESULTSFILE,".csv",sep=""),quote = FALSE,row.names=FALSE)
		}
		else if(MEDIANSFILEFORMAT=="xml")
		{
			RESULTSDF<-data.frame(RESULTS)
			
			xml <- xmlTree()
			xml$addTag("simResults", close=FALSE)
			for (i in 1:nrow(RESULTSDF)) {
				xml$addTag("runMedians", close=FALSE)
				for (j in names(RESULTSDF)) {
					xml$addTag(j, RESULTSDF[i, j])
				}
				xml$closeTag()
			}
			xml$closeTag()	
			
			saveXML(xml,file=paste(RESULTSFILE,".xml",sep=""),indent=FALSE)
		}
	}
}

