############# Initialisation Script ############################################


# Either "yahoo_finance" or "refinitiv" 
# -> data is either diretly downloaded from yahoo finance, or local .xlsx file
# is used (from Refinitiv). The later is for usage in the dissertation
data_source <- "refinitiv"


############# Instruments and time parameters ################################## 
# stocks:
stocks <- c("JPM", "AAPL")

# Value of the Portfolio
Pf <- 1000000


# start of testing period (separation of train set and test set)
test_date <- "2021-01-01" # changing depending on new period for data



############# YAHOO API - time parameters (starting date and end date) #########
start = "2019-12-02"
end = "2020-12-01"



###### General parameters (usage in all models) ###############################  
alpha = 0.99


################### traditional models (mean-var approach & empirical VaR)####
# lookback period (d)
d = 150 # changing to around 1 year!


##############################   GARCH MODELS ############################## 





