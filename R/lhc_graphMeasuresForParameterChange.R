lhc_graphMeasuresForParameterChange <-
function(FILEPATH,PARAMETERS,MEASURES,MEASURE_SCALE,CORCOEFFSOUTPUTFILE,LHCSUMMARYFILENAME,TIMEPOINT,TIMEPOINTSCALE)
{
	# LHCSUMMARYFILENAME IS LHCSummary.csv FOR 1 TIMEPOINT
	# CORCOEFFSOUTPUTFILE IS corCoefs.csv FOR 1 TIMEPOINT
	CORCOEFFS<-read.csv(paste(FILEPATH,"/",CORCOEFFSOUTPUTFILE,sep=""),header=TRUE)
	LHCRESULTFILE<-read.csv(paste(FILEPATH,"/",LHCSUMMARYFILENAME,sep=""))

	# CREATE A GRAPH FOR EACH PARAMETER, FOR EACH MEASURE
	for(p in 1:length(PARAMETERS))
	{
		for(m in 1:length(MEASURES))
		{
			# CREATE LABELS
			yLabel<-paste("Median Value Across Runs ",MEASURE_SCALE[m],sep="")
			xLabel<-"Parameter Value"
			# CREATE CORRELATION LABEL FOR ABOVE GRAPH
			correlationLab<-paste(MEASURES[m],"_Estimate",sep="")
			# GET THE CORRELATION FIGURE
			corrResult<-CORCOEFFS[p,correlationLab]

			# Where the resulting graph should go
			if(is.null(TIMEPOINT))
			{
				GRAPHFILE <- paste(FILEPATH,"/",PARAMETERS[p],"_",MEASURES[m],".pdf",sep="")
				GRAPHTITLE <- paste("LHC Analysis for Parameter: ",PARAMETERS[p],"\nMeasure: ",MEASURES[m],
				"\nCorrelation Coefficient: ",toString(signif(corrResult,3)),sep="")
			}
			else
			{
				GRAPHFILE <- paste(FILEPATH,"/",PARAMETERS[p],"_",MEASURES[m],"_",TIMEPOINT,".pdf",sep="")
				GRAPHTITLE <- paste("LHC Analysis for Parameter: ",PARAMETERS[p],"\nMeasure: ",MEASURES[m],
				"\nCorrelation Coefficient: ",toString(signif(corrResult,3))," Timepoint: ",TIMEPOINT," ",TIMEPOINTSCALE,sep="")
			}

			pdf(GRAPHFILE)

			plot(LHCRESULTFILE[,PARAMETERS[p]],LHCRESULTFILE[,MEASURES[m]],type="p",pch=4,xlab=xLabel,ylab=yLabel,
				main=GRAPHTITLE)
			
			# NO LEGEND NECESSARY AS STATED IN THE HEADER
			par(xpd=FALSE)

			dev.off()
			
		}
	}
}

