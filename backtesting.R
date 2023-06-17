######################### BENCHMARKING #########################################

library(quarks) # alternative package for backtesting

############################# PRE-LOADING ######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


# sourcing initialisation parameters
source("GARCH_models.R")




############# Backtesting functions #############################################

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
  alpha_exp <- 1-a
  alpha_hat <- (1/sample_T) * I_alpha
  
  # Equation Kupiec (1995)
  # split up in several terms for readability
  A = (1-alpha_hat)/(1-alpha_exp)
  B = alpha_hat / alpha_exp
  
  POF = 2 * log(A^(sample_T - I_alpha) * (B^I_alpha))
  
  #p-value 
  # POF ~ CHI_sqrd(df=1)
  p_val = pchisq(POF, df =1, lower.tail = F)
  
  # output
  n_overshoots = I_alpha
  sample = sample_T
  expected_overshoots = alpha_exp
  out = tibble::lst(expected_overshoots, n_overshoots, sample, POF, p_val)
  return(out)
  
}



# Calculation of real returns and Value-at-Risk with a given portfolio value
## generating indicator columns for backtesting (not in usage atm)
for(i in 1:length(stocks)){
  assign(stocks[i],
         eval(as.name(stocks[i])) %>% 
           
           # mutiplying the return and forecast with negative Portfolio Value
           # negative values indicate returns et vice versa
           mutate(real_Loss = R * -Pf, .after = r) %>% 
           mutate(CMM_VaR = CMM_VaR * -Pf) %>% 
           mutate(non_par_VaR = non_par_VaR * -Pf) %>% 
           mutate(GARCH_VaR = GARCH_VaR * -Pf)
  )
}



########## generating test sets from full data tables ##########################  







