##### API Refinitiv Datastream ################################################


############################# PRE-LOADING #######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing utility script (dependencies and functions)
source("utility.R")


working_excel <- read_excel("~/OneDrive - University of St Andrews/working_excel.xlsx")



