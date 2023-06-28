############# Initialisation Script ############################################

# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


# DESCRIPTION:
# Choosing stocks to be observed.
# Setting parameters for model evaluation.



# Either "yahoo_finance" or "refinitiv" 
# -> data is either diretly downloaded from yahoo finance, or local .xlsx file
# is used (from Refinitiv). The later is for usage in the dissertation
data_source <- "yahoo_finance"


############# Instruments and time parameters ################################## 
# stocks:
stocks <- c("JPM", "AAPL", "DAX")


##### time parameters (starting date and end date) relevant for YAHOO API ######
# this indicates the date for the orginial data to be downloaded from YAHOO FINANCE
start = "2011-01-01"
end = "2023-05-10"

# Value of (imaginary) the Portfolio
Pf <- 1000000

# list of models used
models <- c("historical", "CMM", "GARCH", "LSTM_NNet")

# start of testing period (separation of train set and test set)
test_start <- "2021-01-01" # changing depending on new period for data
test_end <- "2022-12-31"


###### General parameters (usage in all models) ###############################  
alpha = 0.99


# lookback period (d)
d = 250 # changing to around 1 year!




print("--------- module initialisation - finished ---------")



