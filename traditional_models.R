########## MEAN-VARIANCE MODEL AND HISTORICAL SIMULATIONS ####################### 


############################# PRE-LOADING ######################################


# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing yahoo_finance API script 
# parameters must be adjusted in the respective API script
source("api_yahoo_finance.R")




################### MEAN-VARIANCE MODEL (parametric) ###########################

# checking this up!!!!! 
# adds parametric VaR forecast to data frame
par_VaR_single <- function(df, alpha, d = 60){
  
  z = qnorm(1-alpha) # defining z_a
  df$par_VaR <-  NA # pre-allocating NA values
  
  # from d+1 to exclude the 0% return for first observation
  for (i in (d+1) : dim(df)[1]){ 
    
    # equation: mean + z_a * sigma
    df$par_VaR[i] = (df$R[(i-1):(i-d)] %>% mean()) + z * (df$R[(i-1):(i-d)] %>% sd())
  }
  
  return(df)
}


AAPL$par_VaR <- par_VaR_single(AAPL, d = 60, alpha = 0.99)$par_VaR





