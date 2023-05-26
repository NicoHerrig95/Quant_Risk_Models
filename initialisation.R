############# Initialisation Script ############################################



data_source <- "yahoo_finance"


# Instruments and time parameters
# stocks:
stocks <- c("JPM", "AAPL")

# time parameters (starting date and )
start = "2019-12-02"
end = "2020-12-01"



###### General parameters (usage in all models) #####
alpha = 0.99


###### parameters for traditional models (mean-var approach & empirical VaR)####
# lookback period (d)
d = 60


#### parameters for GARCH models ####
p = 1
q = 1




