#################### REFINITIVE DATASTREAM SCRIPT ##############################

library(DatastreamDSWS2R)

############################# PRE-LOADING #######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing utility script (dependencies and functions)
source("utility.R")




DatastreamUsername= nh90@st-andrews.ac.uk 
DatastreamPassword= PASSWORD123


mydsws <- dsws$new()
myData <- mydsws$snapshotRequest(instrument = c("U:F","U:BA","U:CAT","U:CVX","U:KO","U:XOM","@INTC","U:JNJ","U:IBM"),
                                 datatype = c("NAME", "EXNAME", "GEOGN", "TR1N","TR2N","TR3N", "TR4N","TR5N"), requestDate = "0D")
