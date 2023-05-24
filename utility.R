##### Utility script #####

library(tseries)
library(tidyverse)
library(zoo)
library(lubridate)
library(quantmod)
library(fGarch)



##### return functions #####
log_returns <- function(df){
  r <- rep(0, dim(df)[1])
  
  for (i in 2 : length(r)){
    r[i] = log(df$Close[i]) - log(df$Close[i-1])
  }
  
  return(r)
}


discrete_returns <- function(df){
  R <- rep(0, dim(df)[1])
  
  for (i in 2 : length(R)){
    R[i] = (df$Close[i] - df$Close[i-1]) / df$Close[i-1]
  }
  
  return(R)
  
  
}
