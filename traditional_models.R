########## MEAN-VARIANCE MODEL AND HISTORICAL SIMULATIONS ####################### 


############################# PRE-LOADING ######################################


# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


# sourcing initialisation parameters
source("initialisation.R")

# sourcing API script 
# choosing data source based on parameters in initialisation script 
if (data_source == "yahoo_finance"){
  source("api_yahoo_finance.R")
} else if (data_source == "refinitiv") {
  source("refinitiv_data_sourcer.R")
}




################### MEAN-VARIANCE MODEL (parametric) ###########################


# parametric VaR forecast 
par_VaR <- function(returns, alpha, d){
  z = qnorm(1-alpha)
  par_VaR <- rep(NA, length(returns))
  
  for (i in (d+2) : length(returns)){
    mu = returns[(i-1):(i-d)] %>% mean()
    sigma = returns[(i-1):(i-d)] %>% sd()
    
    par_VaR[i] = mu + z * sigma
  }
  
  return(par_VaR)
}



################### HISTORICAL SIMULATIONS (non-parametric) #####################

non_par_VaR <- function(returns, alpha, d){
  z = qnorm(1-alpha)
  non_par_VaR <- rep(NA, length(returns))
  
  for (i in (d+2) : length(returns)){
    # empirical alpha quantile of the d prior returns
    non_par_VaR[i] = quantile(returns[(i-1):(i-d)],
                              probs = (1 - alpha),
                              na.rm = TRUE)
  }
  return(non_par_VaR)
}




# computing parametric and non-parametric approach
# and adding to respective data frame
for (i in 1 : length(stocks)){
  assign(stocks[i],
         eval(as.name(stocks[i])) %>% 
           # parametric approach
           mutate(par_VaR = par_VaR(eval(as.name(stocks[i]))$R,
                                    alpha = alpha,
                                    d = d)) %>% 
           # non-parametric approach (historical simulations)
           mutate(non_par_VaR = non_par_VaR(eval(as.name(stocks[i]))$R,
                                            alpha = alpha,
                                            d = d))
           
         
  )}














