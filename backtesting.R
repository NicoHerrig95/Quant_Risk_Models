################################################################################  
######################### BENCHMARKING #########################################
################################################################################  



############################# PRE-LOADING ######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# sourcing initialisation parameters
source("GARCH_models.R")




# ------------------------------------------------------------------------------
######################### VERSION WITHOUT NNET RESULTS #########################


if(source_NNet_results == F){
  ######## Calculating Value-at-Risk and 1-day Loss of reference portfolio Pf #####
  # subsetting stocks data frames
  for(i in 1:length(stocks)){
    assign(stocks[i],
           eval(as.name(stocks[i])) %>%
             mutate(one_day_loss = -R * Pf, .after = r) %>% 
             mutate(CMM_VaR = -CMM_VaR * Pf) %>% 
             mutate(non_par_VaR = - non_par_VaR * Pf) %>% 
             mutate(GARCH_VaR = - GARCH_VaR * Pf) %>% 
             mutate(LSTM_MDN_vanilla = NA, # empty columns for LSTM models
                    LSTM_MDN_reg = NA,
                    LSTM_MDN_3C = NA))
  }
  
  
  
  # importing results from Neural Network backend if condition is T
  
  models <- c("historical",
              "CMM",
              "GARCH",
              "LSTM_MDN_vanilla",
              "LSTM_MDN_reg",
              "LSTM_MDN_3C")
  
  
  
  ########## generating test sets from full data tables ##########################  
  # generating names for test sets
  test_sets <- paste0(stocks,"_test")
  
  # subsetting stocks data frames
  for(i in 1:length(test_sets)){
    assign(test_sets[i],
           eval(as.name(stocks[i])) %>% 
             # subsetting observations which lie within the testing time frame
             # i.e. observation where date > beginning of test period 
             subset(., date >= test_start & date <= test_end) %>% 
             dplyr::select(date,
                           one_day_loss,
                           CMM_VaR,
                           non_par_VaR,
                           GARCH_VaR,
                           LSTM_MDN_vanilla,
                           LSTM_MDN_reg,
                           LSTM_MDN_3C)
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
             LRuc = NA, # Kupiec (1995) unconditional coverage test ~ Chisq(df = 1)
             LRcci = NA, # Christoffersen (1998) Interval Forecast Test ~ Chisq(df = 1)
             LRcc = NA) # conditional coverage (CC) mixed test -> LRcc ~ Chisq(df = 2)
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
    
  }
  
  
  print("--------- module backtesting - finished ---------")
}
# ------------------------------------------------------------------------------















# ------------------------------------------------------------------------------
######################### VERSION WITHOUT NNET RESULTS #########################


if(source_NNet_results == T){
  ######## Calculating Value-at-Risk and 1-day Loss of reference portfolio Pf #####
  # subsetting stocks data frames
  for(i in 1:length(stocks)){
    assign(stocks[i],
           eval(as.name(stocks[i])) %>%
             mutate(one_day_loss = -R * Pf, .after = r) %>% 
             mutate(CMM_VaR = -CMM_VaR * Pf) %>% 
             mutate(non_par_VaR = - non_par_VaR * Pf) %>% 
             mutate(GARCH_VaR = - GARCH_VaR * Pf) %>% 
             mutate(LSTM_MDN_vanilla = NA, # empty columns for LSTM models
                    LSTM_MDN_reg = NA,
                    LSTM_MDN_3C = NA))
  }
  
  
  
  
  # importing results from Neural Network backend if condition is T
  
  models <- c("historical",
              "CMM",
              "GARCH",
              "LSTM_MDN_vanilla",
              "LSTM_MDN_reg",
              "LSTM_MDN_3C")
  
  
  
  ########## generating test sets from full data tables ##########################  
  # generating names for test sets
  test_sets <- paste0(stocks,"_test")
  
  # subsetting stocks data frames
  for(i in 1:length(test_sets)){
    assign(test_sets[i],
           eval(as.name(stocks[i])) %>% 
             # subsetting observations which lie within the testing time frame
             # i.e. observation where date > beginning of test period 
             subset(., date >= test_start & date <= test_end) %>% 
             dplyr::select(date,
                           one_day_loss,
                           CMM_VaR,
                           non_par_VaR,
                           GARCH_VaR,
                           LSTM_MDN_vanilla,
                           LSTM_MDN_reg,
                           LSTM_MDN_3C)
    )
  }
  
  
  
  
  # ------------------------------------------------------------------------------
  # !!!!! SOURCING RESULTS FROM NNET SCRIPT !!!!
  # the script automatically assigns VaR forecasts to the empty columns defined 
  # above
  source("NNet_sourcer.R")
  # ------------------------------------------------------------------------------
  
  
  
  
  ###################### RUN BACKTESTING ######################################### 
  
  # generating names for results data frames 
  results <- paste0(stocks,"_backtested")
  
  # storage object for backtesting results
  results_backtesting <- list()
  
  for (i in 1 : length(results)){
    results_backtesting[[results[i]]] <- data.frame(row.names = models) %>% 
      mutate(proportion_overshoots = NA,
             LRuc = NA, # Kupiec (1995) unconditional coverage test ~ Chisq(df = 1)
             LRcci = NA, # Christoffersen (1998) Interval Forecast Test ~ Chisq(df = 1)
             LRcc = NA) # conditional coverage (CC) mixed test -> LRcc ~ Chisq(df = 2)
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
    
    
    
    
    
    
    # ----------------------NNET RESULTS ---------------------------------------
    # NNET vanilla
    # Unconditional Coverage Test 
    results_backtesting[[i]]$LRuc[4] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                              VaR = eval(as.name(test_sets[i]))$LSTM_MDN_vanilla)$POF
    # proportion of overshoots
    results_backtesting[[i]]$proportion_overshoots[4] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                               VaR = eval(as.name(test_sets[i]))$LSTM_MDN_vanilla)$proportion_overshoots
    
    # LRcci
    results_backtesting[[i]]$LRcci[4] <- Christoffersen_Interval_Forecast_Test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                               VaR = eval(as.name(test_sets[i]))$LSTM_MDN_vanilla)$LRcci
    
    # LRcc (conditional coverage test)
    results_backtesting[[i]]$LRcc[4] <- results_backtesting[[i]]$LRuc[4] + results_backtesting[[i]]$LRcci[4]
    
    
    
    
    
    
    
    
    
    
    
    # NNET regularized
    # Unconditional Coverage Test 
    results_backtesting[[i]]$LRuc[5] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                              VaR = eval(as.name(test_sets[i]))$LSTM_MDN_reg)$POF
    # proportion of overshoots
    results_backtesting[[i]]$proportion_overshoots[5] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                               VaR = eval(as.name(test_sets[i]))$LSTM_MDN_reg)$proportion_overshoots
    
    # LRcci
    results_backtesting[[i]]$LRcci[5] <- Christoffersen_Interval_Forecast_Test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                               VaR = eval(as.name(test_sets[i]))$LSTM_MDN_reg)$LRcci
    
    # LRcc (conditional coverage test)
    results_backtesting[[i]]$LRcc[5] <- results_backtesting[[i]]$LRuc[5] + results_backtesting[[i]]$LRcci[5]
    
    
    
    
    
    
    
    
    
    
    # NNET 3-Component
    # Unconditional Coverage Test 
    results_backtesting[[i]]$LRuc[6] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                              VaR = eval(as.name(test_sets[i]))$LSTM_MDN_3C)$POF
    # proportion of overshoots
    results_backtesting[[i]]$proportion_overshoots[6] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                               VaR = eval(as.name(test_sets[i]))$LSTM_MDN_3C)$proportion_overshoots
    
    # LRcci
    results_backtesting[[i]]$LRcci[6] <- Christoffersen_Interval_Forecast_Test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                               VaR = eval(as.name(test_sets[i]))$LSTM_MDN_3C)$LRcci
    
    # LRcc (conditional coverage test)
    results_backtesting[[i]]$LRcc[6] <- results_backtesting[[i]]$LRuc[6] + results_backtesting[[i]]$LRcci[6]
    
    # ------------------------------------------------------------------------------
  }
  
  
  print("--------- module backtesting - finished ---------")
}
# ------------------------------------------------------------------------------





