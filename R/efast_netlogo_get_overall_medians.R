efast_netlogo_get_overall_medians<-function(FILEPATH,NUMCURVES,PARAMETERS,NUMSAMPLES,MEASURES,TIMEPOINTS=NULL,TIMEPOINTSCALE=NULL)
{
	# Check the parameter and measures strings
	PARAMETERS<-table_header_check(PARAMETERS)
	MEASURES<-table_header_check(MEASURES)

	# Call the spartan function
	efast_get_overall_medians(FILEPATH,NUMCURVES,PARAMETERS,NUMSAMPLES,MEASURES,TIMEPOINTS,TIMEPOINTSCALE)
}
