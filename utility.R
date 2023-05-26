##### Utility script #####

library(tseries)
library(tidyverse)
library(zoo)
library(lubridate)
library(quantmod)
library(fGarch)

############################# PRE-LOADING ######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing from initialisation script
source("initialisation.R")





############################# UTILITY FUNCTIONS #################################

##### return functions #####


# computing log returns
log_returns <- function(price){
  r <- rep(NA, length(price))
  
  for (i in 2 : length(r)){
    r[i] = log(price[i]) - log(price[i-1])
  }
  return(r)
}


# computing discrete returns
discrete_returns <- function(price){
  R <- rep(NA, length(price))
  
  for (i in 2 : length(R)){
    R[i] = (price[i] - price[i-1]) / price[i-1]
  }
 
  return(R)
}
