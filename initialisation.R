################################################################################   
########################## INITIALISATION SCRIPT ###############################
################################################################################   


# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


# REPRODUCIBILITY
set.seed(0911) # setting seed


# DESCRIPTION:
# Choosing stocks to be observed.
# Setting parameters for model evaluation.


# Either "yahoo_finance" or "refinitiv" 
# -> data is either diretly downloaded from yahoo finance, or local .xlsx file
# is used (from Refinitiv). The later is for usage in the dissertation
data_source <- "yahoo_finance"


#------------------------- INSTRUMENTS AND TIME FRAME ------------------- ######
################################################################################ 


# stocks:
stocks <- c("^GSPC", "^FTSE", "^STOXX50E")



# start of testing period (separation of train set and test set)
test_start <- "2021-01-01" # changing depending on new period for data
test_end <- "2022-12-31"

# period -> should be "covid" or "pre_covid"
period = "covid"


#------------------------- MODEL & VaR SPECIFIC PARAMETERS --------------------#
################################################################################ 


# list of models used
models <- c("historical",
            "CMM",
            "GARCH",
            "LSTM_MDN_vanilla",
            "LSTM_MDN_reg",
            "LSTM_MDN_3C")


# VaR alpha-vele 
alpha = 0.99


# lookback period (d)
d = 250 # changing to around 1 year!

# Value of reference Portfolio for calculating one-day losses
Pf <- 1000000



#------------------------- PARAMETERS DATA DOWNLOAD  --------------------------#
################################################################################ 

##### time parameters (starting date and end date) relevant for YAHOO API ######
# this indicates the date for the orginial data to be downloaded from YAHOO FINANCE
start = "2001-01-01"
end = "2023-05-10"




#---------------- PARAMETERS NEURAL NET BACKEND IMPORT  -----------------------#
################################################################################ 

# boolean, indicating if results shall be sourced as .csv
source_NNet_results = T


# list for import names from NNet script backend
import_names = c("GSPC", "FTSE", "EUSTOXX")

# model names in backend
NNet_names = c("vanilla", "regularized","C3")












print("--------- module initialisation - finished ---------")



