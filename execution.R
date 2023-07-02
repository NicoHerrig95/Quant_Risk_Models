# EXECUTION'

############################### Execution options ###############################

# running whole program
run_program = T

# printing additional information
add_info = T

# extracting data files to the data_files folder
extract_files = T


################################################################################





if (run_program == T){
  current_path = rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  source("backtesting.R")
  print("Exeuction successfull")
  cat(" .", "\n",".", "\n",".", "\n",".", "\n",".", "\n")
  print("----- RESULTS BACKTESTING -----")
  cat(" .", "\n",".", "\n")
  print(results_backtesting)
  
  
  # safing data files in the data_files folder
  if (extract_files == T) {
    # defining file path of data extraction file
    extraction_path = paste0(getwd(),"/data_files/")
    
    # function for storing the data tables as csv in respective data folder
    for (i in 1 : length(stocks)){
      write.csv(x = eval(as.name(stocks[i])),
                file = paste0(extraction_path, stocks[i],".csv"))
    }
    
  }
  
  
  # additional information
  if (add_info == T){
    print("----- ADDITIONAL INFORMATION OF TEST RUN -----")
    cat(" .", "\n")
    cat("time span for testing:","\n")
    cat(test_start, "until", test_end, "\n")
    cat(" .", "\n")
    cat("underlying alpha-level:", alpha, "\n")
    cat(" .", "\n")
    
    # data source
    if (data_source == "yahoo_finance"){
      cat("Data from Yahoo Finance", "\n")
    } else {
      cat("Data from Refinitiv Datastream", "\n")
    }
    cat(" .", "\n")

    # printing estimated distribution of returns
    for (i in 1 : length(stocks)){
      if (grepl("GED", best_models[i], fixed = TRUE) == TRUE){
        cat("Estimated distribution for returns of",stocks[i], ": GED", "\n")
      } else {
        cat("Estimated distribution for returns of",stocks[i], ": gaussian", "\n")
      }
    }
  }
}






