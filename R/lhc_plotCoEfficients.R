lhc_plotCoEfficients<-function(FILEPATH, CORCOEFFSOUTPUTFILE, MEASURES, PRINTOPT)
{
	# READ IN THE COEFFICIENTS FILE
	COEFFSRESULTSFILENAME<-paste(FILEPATH,"/",CORCOEFFSOUTPUTFILE,sep="")
	COEFFS<-read.csv(COEFFSRESULTSFILENAME,header=TRUE)
	
	# Check the Measures and Parameters for Spaces - R will have replaced these with a dot
	MEASURES<-table_header_check(MEASURES)
	
	# COLUMN 1 HAS PARAMETER NAMES, THEN FOLLOWS FOR EACH MEASURE - THE PRCC AND THE P VALUE
	# WE'RE GOING TO GRAPH ALL THE PRCC'S ON ONE GRAPH
	
	if(PRINTOPT=="INDIVIDUAL")
	{
		# INDIVIDUAL PLOTS FOR EACH MEASURE
		
		for(i in 1:length(MEASURES))
		{
			print("Producing Partial Rank Correlation Coefficient Plots for Each Measure")
			pdf(paste(FILEPATH,"/PRCC_Measure_",MEASURES[i],".pdf",sep=""))
			
			par(xpd=NA,mar=c(10,4,2,2))
			
			barplot(COEFFS[,i+1],ylim=c(-1,1),col="black",
					main=paste("PRCC Values for Measure: ",MEASURES[i],sep=""),
					ylab="Partial Rank Correlation Coefficient",
					names.arg=seq(1,nrow(COEFFS),by=1))
			
			leg<-NULL
			for(v in seq(1,nrow(COEFFS),by=1))
			{
				leg<-cbind(leg,toString(v))
			}
			
			par(xpd=TRUE)
			legend(0.9,-1.25, COEFFS[,1],pch=leg)
			
			par(xpd=FALSE)
			
			dev.off()

			
		}
		
	} else if(PRINTOPT=="ALL")
	{
		print("Producing Partial Rank Correlation Coefficient Summary Plot of All Measures")
		
		# ALL PRCCS FOR ALL MEASURES, ON ONE PLOT
		# Make the data frame for the plot
		# FIRST OF ALL WE NEED TO REMOVE THE P VALUES SO WE CAN AUTOMATE THIS
		
		pdf(paste(FILEPATH,"/PRCC_AllMeasures.pdf",sep=""))
		
		par(xpd=NA,mar=c(10,4,2,2))
		
		PRCCS<-NULL
		for(p in seq(2,ncol(COEFFS),by=2))
		{
			PRCCS<-cbind(PRCCS,COEFFS[,p])
		}
		
		# NOW MAKE THE DATA FRAME
		d<-data.frame(row.names=levels(COEFFS[,1]),PRCCS)
		colnames(d)<-MEASURES
		d <- do.call(rbind, d)
		barplot(d, beside = TRUE, ylim=c(-1,1.4), legend.text = rownames(d),
				args.legend = list(x = "topright", bty="n"),names.arg=seq(1,nrow(COEFFS),by=1),
				main="PRCC Values for All Measures",ylab="Partial Rank Correlation Coefficient")
		
		leg<-NULL
		for(v in seq(1,nrow(COEFFS),by=1))
		{
			leg<-cbind(leg,toString(v))
		}
		
		par(xpd=TRUE)
		legend(0.9,-1.25, COEFFS[,1],pch=leg)
		
		par(xpd=FALSE)
		
		dev.off()
		
		
	}
	
	
	
	
	
	
}
