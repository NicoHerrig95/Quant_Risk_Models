# Data download from Yahoo Finance####


# stock downloads (tickers and time span)
stocks <- c("JPM", "AAPL")
time_span <- c("2019-12-02", "2020-12-01")





############################# PRE-LOADING #######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing utility script (dependencies and functions)
source("utility.R")





######################### Data download and transformation #####################

# downloading stock data from yahoo finance
for(i in 1 : length(stocks)){
  # download quotes from yahoo finance and assigning to
  # data frame by referring to ticker
  # list of tickers is stored in object "stocks" 
  assign(stocks[i],
         get.hist.quote(stocks[i],
                        start = time_span[1],
                        end = time_span[2],
                        quote = "Close") %>% as.data.frame())
  
}


# adding discrete and logarithmic returns
for (i in 1 : length(stocks)){
  assign(stocks[i],
         eval(as.name(stocks[i])) %>% 
           # discrete returns
           mutate(R = discrete_returns(eval(as.name(stocks[i])))) %>% 
           # log returns
           mutate(r = log_returns(eval(as.name(stocks[i])))) %>% 
           # date 
           mutate(date = rownames(AAPL) %>% as.Date, .before = Close)
  )}













