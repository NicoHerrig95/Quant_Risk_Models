# working script R ####

# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing utility script (dependencies and functions)
source("utility.R")


# parameters for stock downloads (tickers and time span)
time_span <- c("2019-12-02", "2020-12-01")
stocks <- c("JPM", "AAPL")



# downloading stock data from yahoo finance
for(i in 1 : length(stocks)){
  assign(stocks[i],
         get.hist.quote(stocks[i],
                        start = time_span[1],
                        end = time_span[2],
                        quote = "Close") %>% as.data.frame())
  
}



discrete_returns(AAPL)
log_returns(AAPL)



