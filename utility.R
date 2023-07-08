################################################################################  
############################## Utility script ##################################
################################################################################  

library(tseries)
library(tidyverse)
library(zoo)
library(lubridate)
library(quantmod)
library(fGarch)
library(Dowd)
library(readxl)

############################# PRE-LOADING ######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing from initialisation script
source("initialisation.R")


################################################################################  
############################# UTILITY FUNCTIONS #################################
################################################################################  

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





################################################################################  
####################### Backtesting utility ####################################
################################################################################  



# Kupiec Coverage test (1995)
# Tests for difference between expected number of overshoots (1-alpha) and
# empirical number of overshoots
# INPUT: vector of Losses, vector of VaR forecasts, alpha
# Output: Proportion of Failures (POF), p-value
Kupiec_Coverage_test <- function(Losses, VaR, a = alpha){
  
  # binary indicator, take value of 0 when realized Loss < VaR forecast
  # and 1 otherwise
  Indicator <- ifelse(Losses < VaR,0,1)
  
  # variable declaration (see equation from Kupiec(1995))
  I_alpha <- sum(Indicator)
  sample_T <- length(Indicator)
  alpha_exp <- 1 - a
  alpha_hat <- (1/sample_T) * I_alpha
  
  # Equation Kupiec (1995)
  # split up in several terms for readability
  A = (1-alpha_hat)/(1-alpha_exp)
  B = alpha_hat / alpha_exp
  
  POF = 2 * log((A^(sample_T - I_alpha)) * (B^I_alpha))
  
  #p-value 
  # POF ~ CHI_sqrd(df=1)
  p_val = pchisq(POF, df =1, lower.tail = F)
  
  # output
  n_overshoots = I_alpha
  sample = sample_T
  expected_overshoots = alpha_exp * sample_T
  proportion_overshoots = alpha_hat
  out = tibble::lst(expected_overshoots, n_overshoots, proportion_overshoots, sample, POF, p_val)
  return(out)
  
}



# Christoffersen’s Interval Forecast Tests
# Tests for independence of breaches
# INPUT: vector of Losses, vector of VaR forecasts
# Output: LRcci, p-value
Christoffersen_Interval_Forecast_Test <- function(Losses, VaR){
  
  # binary indicator, take value of 0 when realized Loss < VaR forecast
  # and 1 otherwise
  Indicator <- ifelse(Losses < VaR, 0, 1)
  
  n_z_z = 0
  n_z_o = 0
  n_o_z = 0
  n_o_o = 0
  
  for(i in 1: (length(Indicator) - 1)){
    if (Indicator[i] == 0 && Indicator[i+1] == 0){n_z_z = n_z_z + 1} else 
      if (Indicator[i] == 0 && Indicator[i+1] == 1){n_z_o = n_z_o + 1} else
        if (Indicator[i] == 1 && Indicator[i+1] == 0){n_o_z = n_o_z + 1} else
          if (Indicator[i] == 1 && Indicator[i+1] == 1){n_o_o = n_o_o + 1}
  }
  
  # calculating probabilities for each case
  pi_zero = (n_z_o) / (n_z_z + n_z_o)
  pi_one = (n_o_o) / (n_o_z + n_o_o)
  pi = (n_z_o + n_o_o) / (n_z_z + n_z_o + n_o_z + n_o_o)
  
  
  # Equation
  numerator = ((1 - pi)^(n_z_z+n_o_z)) * (pi ^ (n_z_o + n_o_o))
  denominator = ((1 - pi_zero)^n_z_z) * (pi_zero^n_z_o) * ((1 - pi_one)^n_o_z) * (pi_one ^ n_o_o)
  LRcci = -2 * log(numerator / denominator)
  
  
  #p-value 
  # POF ~ CHI_sqrd(df=1)
  p_val = pchisq(LRcci, df =1, lower.tail = F)
  
  out = tibble::lst(LRcci, p_val)
  return(out)
  
}




################################################################################  
############# UTILITY FUNCTIONS FOR GAUSSIAN MIXTURE SAMPLING ##################
################################################################################  


r_gaussmix_2c <- function(N, mu1, mu2, sigma1, sigma2, pi1, pi2){
  
  # check if pi parameters sum up to 1
  if(pi1 + pi2 < 0.99999){stop("Error")}
  
  
  U = runif(N) # Uniform sampling
  samples = rep(NA,N) # empty list for samples
  
  for(i in 1:N){
    if(U[i] < pi1){
      samples[i] = rnorm(1,mu1,sigma1)
    }else{
      samples[i] = rnorm(1,mu2,sigma2)
    }
  }
  return(samples)
}

r_gaussmix_3c <- function(N, mu1, mu2, mu3, sigma1, sigma2, sigma3, pi1, pi2, pi3){
  
  if(pi1 + pi2 + pi3 < 0.99999){
    stop("Error")
    }
  
  U = runif(N) # Uniform sampling
  samples = rep(NA,N) # empty list for samples
  
  for(i in 1:N){
    
    if (U[i] < pi1){
      # if U < pi1 -> sample from Component 1
      samples[i] = rnorm(1,mu1,sigma1)
    } else if (U[i] < (pi1 + pi2)){
      # if U < (pi1 + p2) (so U lies between pi1 and pi2) 
      # -> sample from Component 2  
      samples[i] = rnorm(1,mu2,sigma2)
    } else {
      # if U > (pi1 + p2) -> sample from Component 3
      samples[i] = rnorm(1,mu3,sigma3)
    }
  }
  
  
  return(samples)
  
}

