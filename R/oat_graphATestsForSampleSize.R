oat_graphATestsForSampleSize <-
function(FILEPATH,PARAMETERS,PMIN,PMAX,PINC,MEASURES,ATESTSIGLEVEL,ATESTRESULTSFILENAME,TIMEPOINT,TIMEPOINTSCALE)
{
	for(PARAM in 1:length(PARAMETERS))
	{
		# Where the A-Test Results are (ATests.csv for 1 timepoint)
		ATESTS <- read.csv(paste(FILEPATH,"/",PARAMETERS[PARAM],"/",ATESTRESULTSFILENAME,sep=""),header=TRUE)

		# Where the resulting graph should go 
		if(is.null(TIMEPOINT))
		{
			GRAPHFILE <- paste(FILEPATH,"/",PARAMETERS[PARAM],"/",PARAMETERS[PARAM],".pdf",sep="")
			GRAPHTITLE <- paste("A-Test Scores when adjusting parameter \n",PARAMETERS[PARAM],sep="")
		}
		else
		{
			GRAPHFILE <- paste(FILEPATH,"/",PARAMETERS[PARAM],"/",PARAMETERS[PARAM],"_",TIMEPOINT,".pdf",sep="")
			GRAPHTITLE <- paste("A-Test Scores when adjusting parameter \n",PARAMETERS[PARAM],"at Timepoint: ",TIMEPOINT," ",TIMEPOINTSCALE,sep="")
		}
		pdf(GRAPHFILE,width=10,height=7)
		par(xpd=NA,oma=c(0,0,0,9))

		# NOW PLOT THE MEASURES
		# START WITH THE FIRST
		MEASURELABEL<-paste("ATest",MEASURES[1],sep="")
		plot(ATESTS$ParameterVal,ATESTS[,MEASURELABEL],type="o",main=GRAPHTITLE,lty=1,ylim=c(0,1),pch=1,xlab = "Parameter Value",ylab = "A Test Score",xaxt="n")
	
		if(length(MEASURES)>1)
		{
			# NOW ADD THE REST OF THE MEASURES
			for(l in 2:length(MEASURES))
			{
				MEASURELABEL<-paste("ATest",MEASURES[l],sep="")
				lines(ATESTS$ParameterVal,ATESTS[,MEASURELABEL],type="o",lty=5,pch=l)
			}
		}
		
		axis(1,at=seq(PMIN[PARAM],PMAX[PARAM],by=PINC[PARAM]))
		legend(par("usr")[2],par("usr")[4],title="Measures",MEASURES,pch=1:length(MEASURES),lty=1,xjust=0,yjust=2.0)
		par(xpd=FALSE)

		abline(a=0.5,b=0,lty=4)
		text((PMAX[PARAM]+PMIN[PARAM])/2, 0.52, "no difference", col = "blue") 
		abline(a=(0.5+ATESTSIGLEVEL),b=0,lty=4)
		text((PMAX[PARAM]+PMIN[PARAM])/2,(0.5+ATESTSIGLEVEL+0.02), "large difference", col = "blue") 
		abline(a=(0.5-ATESTSIGLEVEL),b=0,lty=4)
		text((PMAX[PARAM]+PMIN[PARAM])/2,(0.5-ATESTSIGLEVEL-0.02), "large difference", col = "blue") 

		dev.off()
	}
}

