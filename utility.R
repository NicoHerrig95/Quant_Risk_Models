##### Utility script #####

library(tseries)
library(tidyverse)
library(zoo)
library(lubridate)
library(quantmod)



time_span <- c("2019-12-02", "202-12-01")
stocks <- c("AAPL", "JPM")



AAPL <- get.hist.quote(ins)


