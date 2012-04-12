aa_graphSampleSizeSummary <-
function(FILEPATH,MEASURES,MAXSAMPLESIZE,SMALL,MEDIUM,LARGE,SUMMARYFILENAME,GRAPHOUTPUTFILE,TIMEPOINT,TIMEPOINTSCALE)
{
	# NOW DRAW THE GRAPH
	aTestResults <- read.csv(paste(FILEPATH,"/",SUMMARYFILENAME,sep=""),header=TRUE)

	# Where the resulting graph should go (ATESTMAXES.PDF USED IF ONE TIMEPOINT)
	graphFile = paste(FILEPATH,"/",GRAPHOUTPUTFILE,sep="")
	pdf(graphFile,width=10,height=7)
	par(xpd=NA,oma=c(0,0,0,9))

	# NOW PLOT FOR EACH MEASURE
	# THE PLOT BEGINS WITH THE FIRST MEASURE
	measureLabel<-paste(MEASURES[1],"MaxA",sep="")
	plot(aTestResults$SampleSize,aTestResults[measureLabel][,1],type="o",lty=1,ylim=c(0.5,1.0),pch=1,xlab = "Sample Size",ylab = "A Test Score",xaxt="n",yaxt="n")

	# NOW DO ALL OTHER MEASURES
	for(l in 2:length(MEASURES))
	{
		measureLabel<-paste(MEASURES[l],"MaxA",sep="")
		lines(aTestResults$SampleSize,aTestResults[measureLabel][,1],type="o",lty=5,pch=l)
	}

	# NOW COMPLETE GRAPH - TITLE DEPENDING ON WHETHER THIS IS ONE TIMEPOINT OR MANY
	if(is.null(TIMEPOINT))
	{
		title("Maximum A-Test Scores for each Sample Size")
	}
	else
	{
		title(paste("Maximum A-Test Scores for each Sample Size \n Timepoint: ",TIMEPOINT," ",TIMEPOINTSCALE,sep=""))
	}
	
	axis(1,at=seq(0,MAXSAMPLESIZE,by=100))
	axis(2, at=seq(0.5,1.0, by=0.05))
	legend(par("usr")[2],par("usr")[4],title="MEASURES",MEASURES,pch=1:length(MEASURES),lty=1,xjust=0,yjust=2.0)
	par(xpd=FALSE)

	# ADD THE LINES TO SHOW WHERE THE A-TEST EFFECTS ARE
	abline(h=SMALL,lty=4)
	text(MAXSAMPLESIZE/2,SMALL-0.01, "SMALL effect", col = "blue") 
	abline(h=LARGE,lty=4)
	text(MAXSAMPLESIZE/2, LARGE+0.01, "LARGE effect", col = "blue") 
	abline(h=MEDIUM,lty=4)
	text(MAXSAMPLESIZE/2,MEDIUM+0.02, "MEDIUM effect", col = "blue") 
	dev.off()
}

