################################################################################
##################  IMPORTING RESULTS FROM  LSTM-MDN BACKEND ###################
################################################################################


current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# importing function for Monte-Carlo sampling from Gaussian Mixtures
source("execution.R")

#test_sets # list of test sets to call 
import_names = c("GSPC", "FTSE", "EUSTOXX")

GSPC_vanilla_covid <- read_csv("NNet_results/GSPC_vanilla_covid.csv")



GSPC_vanilla_covid$VaR = NA
# looping over function for MC based simulations from Gaussian Mixture
for (i in 1:nrow(GSPC_vanilla_covid)){
  GSPC_vanilla_covid$VaR[i] = r_gaussmix_2c(N = 10000,
                                            mu1 = GSPC_vanilla_covid$mu1[i],
                                            mu2 = GSPC_vanilla_covid$mu2[i],
                                            sigma1 = GSPC_vanilla_covid$sigma1[i],
                                            sigma2 = GSPC_vanilla_covid$sigma2[i],
                                            pi1 =  GSPC_vanilla_covid$pi1[i],
                                            pi2 =  GSPC_vanilla_covid$pi2[i]) %>%
    quantile(., probs = 1 - alpha) * - Pf # deriving quantile & multiplying by Portfolio value
  
}



xxx <- rep(NA, nrow(FTSE_vanilla_covid))
for (i in 1:nrow(FTSE_vanilla_covid)){
  xxx[i] =  r_gaussmix_2c(N = 10000,
                        mu1 = FTSE_vanilla_covid$mu1[i],
                        mu2 = FTSE_vanilla_covid$mu2[i],
                        sigma1 = FTSE_vanilla_covid$sigma1[i],
                        sigma2 = FTSE_vanilla_covid$sigma2[i],
                        pi1 =  FTSE_vanilla_covid$pi1[i],
                        pi2 =  FTSE_vanilla_covid$pi2[i]) %>% 
    quantile(., probs = 1 - alpha) * - Pf
  
  
}


# run Kupiec test (LRuc)
Kupiec_Coverage_test(Losses = `^GSPC_test`$one_day_loss,
                     VaR = GSPC_vanilla_covid$VaR)


# run LRcci
Christoffersen_Interval_Forecast_Test(Losses = `^GSPC_test`$one_day_loss,
                                      VaR = GSPC_vanilla_covid$VaR)
