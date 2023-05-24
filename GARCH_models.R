############################# PRE-LOADING ######################################

# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing utility script (dependencies and functions)
source("api_yahoo_finance.R")




# fitting GARCH(1,1) model



