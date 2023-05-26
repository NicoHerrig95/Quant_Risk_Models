############################# PRE-LOADING ######################################


# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


# sourcing initialisation parameters
source("initialisation.R")

# sourcing API script 
if (data_source == "yahoo_finance"){
  source("api_yahoo_finance.R")
} 





# fitting GARCH(1,1) model



