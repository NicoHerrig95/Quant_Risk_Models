######################### BENCHMARKING #########################################

library(quarks) # alternative package for backtesting

############################# PRE-LOADING ######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))



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




# function for calculating mean and max deviation between loss and VaR forecast
# INPUT: vector of Losses, vector of VaR forecasts
# Output: mean deviation & max deviation
deviation_function <- function(Losses, VaR){
  mean_dev = abs(Losses - VaR) %>% mean()
  max_dev = abs(Losses - VaR) %>% max()
  
  out = tibble::lst(mean_dev, max_dev)
  return(out)
  
  
}



######## Calculating Value-at-Risk and 1-day Loss of reference portfolio Pf #####
# subsetting stocks data frames
for(i in 1:length(stocks)){
  assign(stocks[i],
         eval(as.name(stocks[i])) %>%
           mutate(one_day_loss = -R * Pf, .after = r) %>% 
           mutate(CMM_VaR = -CMM_VaR * Pf) %>% 
           mutate(non_par_VaR = - non_par_VaR * Pf) %>% 
           mutate(GARCH_VaR = - GARCH_VaR * Pf) %>% 
           mutate(NNet_VaR = NA))
}



########## generating test sets from full data tables ##########################  
# generating names for test sets
test_sets <- paste0(stocks,"_test")

# subsetting stocks data frames
for(i in 1:length(test_sets)){
  assign(test_sets[i],
         eval(as.name(stocks[i])) %>% 
           # subsetting observations which lie within the testing time frame
           # i.e. observation where date > beginning of test period (=test_date)
           subset(., date >= test_date) %>% 
           dplyr::select(date,
                         one_day_loss,
                         CMM_VaR,
                         non_par_VaR,
                         GARCH_VaR,
                         NNet_VaR)
  )
}






###################### RUN BACKTESTING ######################################### 

# generating names for results data frames 
results <- paste0(stocks,"_backtested")

# storage object for backtesting results
results_backtesting <- list()

for (i in 1 : length(results)){
  results_backtesting[[results[i]]] <- data.frame(row.names = models) %>% 
    mutate(proportion_overshoots = NA,
           LRuc = NA, # Kupiec (1995) unconditional coverage test
           LRcci = NA, # Christoffersen (1998) Interval Forecast Test
           LRcc = NA, # conditional coverage (CC) mixed test -> LRcc ~ Chisq(df = 2)
           mean_dev = NA, # mean absolute deviation from real loss
           max_dev = NA) # max absolute deviation from real loss
}





for (i in 1 : length(test_sets)){
  # Historical Simulations
  # Unconditional Coverage Test for historical simulations
  results_backtesting[[i]]$LRuc[1] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                       VaR = eval(as.name(test_sets[i]))$non_par_VaR)$POF
  # historical simulation - proportion of overshoots
  results_backtesting[[i]]$proportion_overshoots[1] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                            VaR = eval(as.name(test_sets[i]))$non_par_VaR)$proportion_overshoots
  
  # LRcci
  results_backtesting[[i]]$LRcci[1] <- Christoffersen_Interval_Forecast_Test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                             VaR = eval(as.name(test_sets[i]))$non_par_VaR)$LRcci
  
  # LRcc (conditional coverage test)
  results_backtesting[[i]]$LRcc[1] <- results_backtesting[[i]]$LRuc[1] + results_backtesting[[i]]$LRcci[1]
  
  
  # mean dev & max dev
  results_backtesting[[i]]$mean_dev[1] <- deviation_function(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                               VaR = eval(as.name(test_sets[i]))$non_par_VaR)$mean_dev
  
  
  results_backtesting[[i]]$max_dev[1] <- deviation_function(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                             VaR = eval(as.name(test_sets[i]))$non_par_VaR)$max_dev
  

  
  
  
  
  
  
  
  
  # CMM
  # Unconditional Coverage Test for CMM
  results_backtesting[[i]]$LRuc[2] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                       VaR = eval(as.name(test_sets[i]))$CMM_VaR)$POF
  # CMM - proportion of overshoots
  results_backtesting[[i]]$proportion_overshoots[2] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                             VaR = eval(as.name(test_sets[i]))$CMM_VaR)$proportion_overshoots
  
  
  # LRcci
  results_backtesting[[i]]$LRcci[2] <- Christoffersen_Interval_Forecast_Test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                             VaR = eval(as.name(test_sets[i]))$CMM_VaR)$LRcci
  
  
  
  # LRcc (conditional coverage test)
  results_backtesting[[i]]$LRcc[2] <- results_backtesting[[i]]$LRuc[2] + results_backtesting[[i]]$LRcci[2]
  

  # mean dev & max dev
  results_backtesting[[i]]$mean_dev[2] <- deviation_function(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                             VaR = eval(as.name(test_sets[i]))$CMM_VaR)$mean_dev
  
  
  results_backtesting[[i]]$max_dev[2] <- deviation_function(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                            VaR = eval(as.name(test_sets[i]))$CMM_VaR)$max_dev
  
  
  
  
  
  
  
  # GARCH
  # Unconditional Coverage Test for GARCH(1,1)
  results_backtesting[[i]]$LRuc[3] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                VaR = eval(as.name(test_sets[i]))$GARCH_VaR)$POF
  # GARCH(1,1) - proportion of overshoots
  results_backtesting[[i]]$proportion_overshoots[3] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                             VaR = eval(as.name(test_sets[i]))$GARCH_VaR)$proportion_overshoots
  
  # LRcci
  results_backtesting[[i]]$LRcci[3] <- Christoffersen_Interval_Forecast_Test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                             VaR = eval(as.name(test_sets[i]))$GARCH_VaR)$LRcci

  # LRcc (conditional coverage test)
  results_backtesting[[i]]$LRcc[3] <- results_backtesting[[i]]$LRuc[3] + results_backtesting[[i]]$LRcci[3]
  

  
  # mean dev & max dev
  results_backtesting[[i]]$mean_dev[3] <- deviation_function(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                             VaR = eval(as.name(test_sets[i]))$GARCH_VaR)$mean_dev
  
  
  results_backtesting[[i]]$max_dev[3] <- deviation_function(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                            VaR = eval(as.name(test_sets[i]))$GARCH_VaR)$max_dev
  

}




print("--------- module backtesting - finished ---------")


