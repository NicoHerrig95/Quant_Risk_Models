################################################################################
##################  IMPORTING RESULTS FROM  LSTM-MDN BACKEND ###################
################################################################################


current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# importing function for Monte-Carlo sampling from Gaussian Mixtures
source("execution.R")
source("utility.R")

if(source_NNet_results == T){
  
### fill here later
  
}


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
# and generating empty NA vectors 
NNet_results <- paste0(NNet_file_names,
                       "_VaR")



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




# generating NNet VaR forecasts by Monte-Carlo method using samples from
# respective Gaussian Mixture 
# simulations
for(j in 1 : length(NNet_results)){
  
  # simulating returns from 2-Component Gaussian Mixture
  if (grepl("C3", NNet_results[j]) == FALSE){
    for(i in 1 : nrow(eval(as.name(NNet_file_names[j])))){
      
      assign(NNet_results[j],
             c(eval(as.name(NNet_results[j])),
               r_gaussmix_2c(N = 10000, # using function for 2C sampling
                             mu1 = eval(as.name(NNet_file_names[j]))$mu1[i],
                             mu2 = eval(as.name(NNet_file_names[j]))$mu2[i],
                             sigma1 = eval(as.name(NNet_file_names[j]))$sigma1[i],
                             sigma2 = eval(as.name(NNet_file_names[j]))$sigma2[i],
                             pi1 =  eval(as.name(NNet_file_names[j]))$pi1[i],
                             pi2 =  eval(as.name(NNet_file_names[j]))$pi2[i]) %>%
                 quantile(., probs = 1 - alpha) * - Pf))# deriving quantile & multiplying by -Pf
    }
  }
  # simulating returns from 2-Component Gaussian Mixture
  if (grepl("C3", NNet_results[j]) == TRUE){
    for(i in 1 : nrow(eval(as.name(NNet_file_names[j])))){
      
      assign(NNet_results[j],
             c(eval(as.name(NNet_results[j])),
               r_gaussmix_3c(N = 10000, # using function for 3c sampling
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




# run Kupiec test (LRuc)
Kupiec_Coverage_test(Losses = `^GSPC_test`$one_day_loss,
                     VaR = GSPC_C3_covid_VaR)


# run LRcci
Christoffersen_Interval_Forecast_Test(Losses = `^GSPC_test`$one_day_loss,
                                      VaR = GSPC_C3_covid_VaR)

