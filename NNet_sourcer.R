################################################################################
##################  IMPORTING RESULTS FROM  LSTM-MDN BACKEND ###################
################################################################################


# NOTE:
# THIS SCRIPT IS SOLELY FOR IMPORTING NEURAL NET RESULTS AS PART OF A UNIVERSITY
# PROJECT


current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# importing function for Monte-Carlo sampling from Gaussian Mixtures
# sourcing utility script
source("utility.R")

if(source_NNet_results == T){
  
  
  
  
  # generating file names for .csv import automatisation
  NNet_file_names <- list()
  for(x in import_names){
    for(y in NNet_names){
      for(z in period){
        string <- paste(x,y,z, sep = "_")
        NNet_file_names <- append(NNet_file_names, string) 
      }
    }
  }
  
  
  # names for result object generation
  NNet_results <- paste0(NNet_file_names, "_VaR")
  
  # !!!!!!!!!!!!!!!!!!!!!!!! ALTERED FILED DOWNLOAD!
  
  # unlisting the file names
  NNet_file_names <- NNet_file_names %>% unlist()
  # loading .csv files with results from NNet backend
  for(i in 1 : length(NNet_file_names)){
    assign(NNet_file_names[i], read_csv(paste0("NNet_results/", NNet_file_names[i] ,".csv"))[,-1])
    
  }
  
  
  # generating empty vectors for storage
  for(j in 1 : length(NNet_file_names)){
    assign(NNet_results[j],c())
  }
  
  
  
  # MC SAMPLING FROM GAUSSIAN MIXTURE
  # generating NNet VaR forecasts by Monte-Carlo method using samples from
  # respective Gaussian Mixture 
  # simulations
  for(j in 1 : length(NNet_results)){
    
    # simulating returns from 2-Component Gaussian Mixture
    if (grepl("C3", NNet_results[j]) == FALSE){
      for(i in 1 : nrow(eval(as.name(NNet_file_names[j])))){
        
        assign(NNet_results[j],
               c(eval(as.name(NNet_results[j])),
                 r_gaussmix_2c(N = n_samples_MC, # using function for 2C sampling
                               mu1 = eval(as.name(NNet_file_names[j]))$mu1[i],
                               mu2 = eval(as.name(NNet_file_names[j]))$mu2[i],
                               sigma1 = eval(as.name(NNet_file_names[j]))$sigma1[i],
                               sigma2 = eval(as.name(NNet_file_names[j]))$sigma2[i],
                               pi1 =  eval(as.name(NNet_file_names[j]))$pi1[i],
                               pi2 =  eval(as.name(NNet_file_names[j]))$pi2[i]) %>%
                   quantile(., probs = 1 - alpha) * - Pf))# deriving quantile & multiplying by -Pf
      }
    }
    # simulating returns from 3-Component Gaussian Mixture
    if (grepl("C3", NNet_results[j]) == TRUE){
      for(i in 1 : nrow(eval(as.name(NNet_file_names[j])))){
        
        assign(NNet_results[j],
               c(eval(as.name(NNet_results[j])),
                 r_gaussmix_3c(N = n_samples_MC, # using function for 3c sampling
                               mu1 = eval(as.name(NNet_file_names[j]))$mu1[i],
                               mu2 = eval(as.name(NNet_file_names[j]))$mu2[i],
                               mu3 = eval(as.name(NNet_file_names[j]))$mu3[i],
                               sigma1 = eval(as.name(NNet_file_names[j]))$sigma1[i],
                               sigma2 = eval(as.name(NNet_file_names[j]))$sigma2[i],
                               sigma3 = eval(as.name(NNet_file_names[j]))$sigma3[i],
                               pi1 =  eval(as.name(NNet_file_names[j]))$pi1[i],
                               pi2 =  eval(as.name(NNet_file_names[j]))$pi2[i],
                               pi3 =  eval(as.name(NNet_file_names[j]))$pi3[i]) %>%
                   quantile(., probs = 1 - alpha) * - Pf))# deriving quantile & multiplying by -Pf
        
        
        
        
        
      }
    }
  }
  
  
  
  # manually assigning results to data frames for backtesting
  # S&P 500
  `^GSPC_test`$LSTM_MDN_vanilla  <- eval(as.name(paste(sep = "_","GSPC_vanilla",period,"VaR")))
  `^GSPC_test`$LSTM_MDN_reg  <- eval(as.name(paste(sep = "_","GSPC_regularized",period,"VaR")))
  `^GSPC_test`$LSTM_MDN_3C  <- eval(as.name(paste(sep = "_","GSPC_C3",period,"VaR")))
  
  
  # FTSE
  `^FTSE_test`$LSTM_MDN_vanilla  <- eval(as.name(paste(sep = "_","FTSE_vanilla",period,"VaR")))
  `^FTSE_test`$LSTM_MDN_reg  <- eval(as.name(paste(sep = "_","FTSE_regularized",period,"VaR")))
  `^FTSE_test`$LSTM_MDN_3C  <- eval(as.name(paste(sep = "_","FTSE_C3",period,"VaR")))
  
  
  # EUROSTOXX50
  `^STOXX50E_test`$LSTM_MDN_vanilla  <- eval(as.name(paste(sep = "_","EUSTOXX_vanilla",period,"VaR")))
  `^STOXX50E_test`$LSTM_MDN_reg  <- eval(as.name(paste(sep = "_","EUSTOXX_regularized",period,"VaR")))
  `^STOXX50E_test`$LSTM_MDN_3C  <- eval(as.name(paste(sep = "_","EUSTOXX_C3",period,"VaR")))
  
}




