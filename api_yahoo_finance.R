# Data download from Yahoo Finance####

############################# PRE-LOADING #######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


# sourcing utility script (dependencies and functions)
source("utility.R")



# bind from initialisation script
time_span <- c(start, end)
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


# adding returns and date variable
for (i in 1 : length(stocks)){
  assign(stocks[i],
         eval(as.name(stocks[i])) %>% 
           # discrete returns
           mutate(R = discrete_returns(eval(as.name(stocks[i]))$Close)) %>% 
           # log returns
           mutate(r = log_returns(eval(as.name(stocks[i]))$Close)) %>% 
           # date 
           mutate(date = rownames(eval(as.name(stocks[i]))) %>% as.Date, .before = Close) %>% 
           remove_rownames()
         )}













