############################# PRE-LOADING ######################################


# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing prior script (traditional models)
source("traditional_models.R") 




################# specific functions for GARCH models ##########################

# function for predicting VaR with GARCH (1,1) model
# INPUTS: GARCH(1,1) model (garchFit object) and alpha-quantile for distribution
# OUTPUTS: VaR forecast for t+1
GARCH_VaR <- function(model, alpha_lvl = alpha){
  
  # deciding whether underlying distribution is normal or GED
  # GED case
  if (model@fit$params$cond.dist == "ged"){
    # storing shape parameter from GED distribution
    shape_param <- model@fit$par['shape']
    # storing (1-alpha) quantile from defined GED distribution
    quantile <- qged(p = (1 - alpha_lvl),
                     nu = shape_param) %>% unname()
    
    # normal case  
  } else if (model@fit$params$cond.dist == "norm"){
    quantile <- qnorm(p = (1 - alpha_lvl))
  }
  
  # storing the last element of sigma.t from garchFit object
  # this represents sigma_t as usage for the VaR forcast for t+1!
  sigma_t <- tail(model@sigma.t, n = 1)
  
  # storing the last element of residuals() from garchFit object
  # this represents epsilon_t as usage for the VaR forcast for t+1!
  epsilon_t <- tail(residuals(model), n = 1)
  
  # defining parameters for volatility equation
  a_0 <- model@fit$par['omega'] %>% unname()
  a_1 <- model@fit$par['alpha1'] %>% unname()
  b_1 <- model@fit$par['beta1'] %>% unname()
  
  # sigma^2_t+1
  sigma_sqrt_t_plus_one <- a_0 + a_1 * epsilon_t^2 + b_1 * sigma_t^2
  # sigma_t+1
  sigma_t_plus_one <- sqrt(sigma_sqrt_t_plus_one)
  
  # calculating VaR forecast for t+1
  VaR_forecast <- quantile * sigma_t_plus_one
  
  
  return(VaR_forecast)
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
                  # normally distributed innovations
                  cond.dist = "norm",
                  # no estimation of mu or skewness 
                  include.mean = FALSE,
                  include.skew = FALSE) 
  )
}



# generating GARCH(1,1) models with GED distributed innovations
for (i in 1 : length(GARCH_1_1_GED)){
  
  assign(GARCH_1_1_GED[i],
         garchFit(formula = ~garch(1,1),
                  # "R[-1]" to exclude first NA return
                  data = eval(as.name(stocks[i]))$R[-1],
                  # GED distributed innovations
                  cond.dist = "ged",
                  # no estimation of mean and skewness of GED distribution
                  include.mean = FALSE,
                  include.skew = FALSE) 
  )
}




#################### model selection ###########################################
# implementing automated model selection based on Information Criterion score
# here: AIC (AIC is sufficient as we aim for forecasting!)


if (GARCH_1_1_normal %>% length() == GARCH_1_1_GED %>% length()){
  best_models <- rep(NA, length(GARCH_1_1_normal))
  
  for (i in 1 : length(GARCH_1_1_normal)){
    
    # @fit$ics calls AIC score from S4 object (GARCH model)
    if (eval(as.name(GARCH_1_1_normal[i]))@fit$ics[1] <
        eval(as.name(GARCH_1_1_GED[i]))@fit$ics[1]){
      # assigning model with lower AIC score as best model
      best_models[i] <- GARCH_1_1_normal[i]
    } else {
      best_models[i] <- GARCH_1_1_GED[i]
    }
  }
}


# best models
cat("optimal models from automated model selection (AIC):", "\n", best_models)


###################### VaR Forecasts ###########################################


for(i in 1 : length(stocks)){
  
  # t_plus_one is initially index of row of
  # the beginning date from testing period
  t_plus_one <- which(eval(as.name(stocks[i]))$date >= test_date) %>% head(n = 1)
  
  # a storer vector for GARCH VaR forecasts with same length as data frame
  storer <- rep(NA, times = dim(eval(as.name(stocks[i])))[1]) 
  
  
  
  # LOOP for generating VaR forecast based on the respective rolling window
  repeat{
    # t (last observation of rolling window period)
    
    t <- t_plus_one - 1
    # first observation of rolling window period (t - d)
    s <- t - d
    # all return from 1,...,t
    returns <- eval(as.name(stocks[i]))[s:t,"R"]
    
    # (re-)fitting the GARCH(1,1) model to the data up until t
    # using the distribution of innovations chosen by the automated selection (AIC)
    working_model <- garchFit(formula = ~garch(1,1),
                              # using the returns from rolling window for model fitting
                              data = returns,
                              # chosen distribution from the respective "best_model" object
                              cond.dist = eval(as.name(best_models[i]))@fit$params$cond.dist,
                              # no estimation of mu or skewness 
                              include.mean = FALSE,
                              include.skew = FALSE) 
    
    
    # storing  VaR forecast in storer object
    storer[t_plus_one] <-  GARCH_VaR(model = working_model)
    
    # breaking condition of repeat loop
    if (t_plus_one == (dim(eval(as.name(stocks[i])))[1])){break}
    
    # indexing +1
    t_plus_one <- t_plus_one + 1
  }
  
  # assigning the forecast saved in storer object to respective data frame
  # as "GARCH_VaR"
  assign(stocks[i],eval(as.name(stocks[i])) %>%
           mutate(GARCH_VaR = storer))
  
}






