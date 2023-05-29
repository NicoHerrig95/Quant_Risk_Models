############################# PRE-LOADING ######################################


# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


# sourcing initialisation parameters
source("initialisation.R")

# sourcing API script 
if (data_source == "yahoo_finance"){
  source("api_yahoo_finance.R")
} 





#################### fitting GARCH models ######################################

# generating list of GARCH(1,1) models
GARCH_1_1_normal <- paste0(stocks, "_GARCH_1_1_normal")
GARCH_1_1_GED <- paste0(stocks, "_GARCH_1_1_GED")

# generating GARCH(1,1) models with normally distributed innovations
for (i in 1 : length(GARCH_1_1_normal)){
  
  assign(GARCH_1_1_normal[i],
         garchFit(formula = ~garch(1,1),
                  # "R[-1]" to exclude first NA return
                  data = eval(as.name(stocks[i]))$R[-1],
                  cond.dist = "norm") # normally distributed innovations
  )
}



# generating GARCH(1,1) models with GED distributed innovations
for (i in 1 : length(GARCH_1_1_GED)){
  
  assign(GARCH_1_1_GED[i],
         garchFit(formula = ~garch(1,1),
                  # "R[-1]" to exclude first NA return
                  data = eval(as.name(stocks[i]))$R[-1],
                  cond.dist = "ged") # normally distributed innovations
  )
}




#################### model selection ###########################################
# implementing automated model selection based on Information Criterion score
# here: AIC


if (GARCH_1_1_normal %>% length() == GARCH_1_1_GED %>% length()){
  best_models <- rep(NA, length(GARCH_1_1_normal))
  
  for (i in 1 : length(GARCH_1_1_normal)){
    
    # @fit$ics calls AIC score from S4 object (GARCH model)
    if (eval(as.name(GARCH_1_1_normal[1]))@fit$ics[i] <
        eval(as.name(GARCH_1_1_GED[1]))@fit$ics[i]){
      # assigning model with lower AIC score as best model
      best_models[i] <- GARCH_1_1_normal[i]
    } else {
      best_models[i] <- GARCH_1_1_GED[i]
    }
  }
}






