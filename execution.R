# EXECUTION'


# What shall be executed
run_program = T
add_info = T



if (run_program == T){
  current_path = rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  source("backtesting.R")
  print("Exeuction successfull")
  cat(" .", "\n",".", "\n",".", "\n",".", "\n",".", "\n")
  print("----- RESULTS BACKTESTING -----")
  cat(" .", "\n",".", "\n")
  print(results_backtesting)
  
  
  # additional information
  if (add_info == T){
    print("----- ADDITIONAL INFORMATION OF TEST RUN -----")
    cat(" .", "\n")
    cat("time span for testing:","\n")
    cat(test_date, "until", end, "\n")
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




