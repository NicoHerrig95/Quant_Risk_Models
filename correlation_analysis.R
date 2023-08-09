################################################################################  
######################### VOLATILITY CLUSERING EVALUATION ######################
################################################################################ 


# calculates the rolling volatility (in form of standatd deviation)
rolling_vola <- function(returns, window = 5){
  
  out = rep(NA, window)
  for(i in (1+window):length(returns)){
    rolling_vola = sd(returns[(i-window+1):i])
    out = c(out, rolling_vola)
  }
  
  return(out)
}


for(i in 1:length(test_sets)){
  
  
  # correlation CMM
  CMM_result = cor(rolling_vola(eval(as.name(test_sets[i]))$one_day_loss), 
                 eval(as.name(test_sets[i]))$CMM_VaR  ,use="complete.obs")
  
  # correlation Historical Simulations
  HS_result = cor(rolling_vola(eval(as.name(test_sets[i]))$one_day_loss), 
                   eval(as.name(test_sets[i]))$non_par_VaR  ,use="complete.obs")
  
  
  # correlation Historical Simulations
  GARCH_result = cor(rolling_vola(eval(as.name(test_sets[i]))$one_day_loss), 
                  eval(as.name(test_sets[i]))$GARCH_VaR  ,use="complete.obs")
  
  
  
  # correlation Model 1
  m1_result = cor(rolling_vola(eval(as.name(test_sets[i]))$one_day_loss), 
                  eval(as.name(test_sets[i]))$LSTM_MDN_vanilla  ,use="complete.obs") # VaR model 1
  
  # correlation Model 2
  m2_result = cor(rolling_vola(eval(as.name(test_sets[i]))$one_day_loss), 
                  eval(as.name(test_sets[i]))$LSTM_MDN_reg  ,use="complete.obs") # VaR model 2
  
  # correlation Model 3
  m3_result = cor(rolling_vola(eval(as.name(test_sets[i]))$one_day_loss), 
                  eval(as.name(test_sets[i]))$LSTM_MDN_3C  ,use="complete.obs") # VaR model 2
  
  
  
  cat(test_sets[i], "\n", "evaluation period: ",period,"\n")
  cat("Correlation LSTM-MDN1: ", m1_result, "\n")
  cat("Correlation LSTM-MDN2: ", m2_result, "\n")
  cat("Correlation LSTM-MDN3: ", m3_result, "\n")
  cat("Correlation CMM: ", CMM_result, "\n")
  cat("Correlation HS: ", HS_result, "\n")
  cat("Correlation GARCH: ", GARCH_result, "\n")
}







