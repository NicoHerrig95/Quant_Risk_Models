################################################################################  
######################### RESULTS OUT  #########################################
################################################################################ 

# script for output of resluts - specified for University project

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


# copy data frames
results_out <- results_backtesting
results_out_pval <- results_backtesting


# calculating p-values 
for ( i in 1:length(results_out)){
  
  # preparing results_out
  results_out[[i]]$proportion_overshoots = round(results_out[[i]]$proportion_overshoots, digits = 5)
  results_out[[i]]$LRuc = round(results_out[[i]]$LRuc, digits = 3)
  results_out[[i]]$LRcci = round(results_out[[i]]$LRcci, digits = 3)
  results_out[[i]]$LRcc = round(results_out[[i]]$LRcc, digits = 3)
  
  
  # preparing results_out_pval
  results_out_pval[[i]]$proportion_overshoots = round(results_out[[i]]$proportion_overshoots, digits = 5)
  results_out_pval[[i]]$LRuc = round(pchisq(results_out[[i]]$LRuc, df =1, lower.tail = F), digits = 3)
  results_out_pval[[i]]$LRcci = round(pchisq(results_out[[i]]$LRcci, df =1, lower.tail = F), digits = 3)
  results_out_pval[[i]]$LRcc = round(pchisq(results_out[[i]]$LRcc, df = 2, lower.tail = F), digits = 3)
}




# saving results as .csv

#folder_path
folder_results = paste(sep = "/", getwd(), "results_output/") 


for(i in 1 : length(stocks)){
  filename = paste0(folder_results,stocks[i],"_results_",period,".csv")
  write.csv(x = results_out[[i]], file = filename)
  
  filename_pvals = paste0(folder_results,stocks[i],"_p_vals_",period,".csv")
  write.csv(x = results_out_pval[[i]], file = filename_pvals)
}


  

  
  

