# Code chunks for later usage


## generating indicator columns for backtesting (not in usage atm)
for(i in 1:length(stocks)){
  assign(stocks[i],
         eval(as.name(stocks[i])) %>% 
           # generating binary indicator column for each model 
           mutate(I_CMM = ifelse(eval(as.name(stocks[i]))$R > eval(as.name(stocks[i]))$CMM_VaR, 0, 1)) %>% 
           mutate(I_non_par = ifelse(eval(as.name(stocks[i]))$R > eval(as.name(stocks[i]))$non_par_VaR, 0, 1)) %>% 
           mutate(I_GARCH = ifelse(eval(as.name(stocks[i]))$R > eval(as.name(stocks[i]))$GARCH_VaR, 0, 1)) %>% 
           mutate(I_Neural_Net = NA) # TBC
  )
}
