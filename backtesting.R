######################### BENCHMARKING #########################################

library(quarks) # alternative package for backtesting

############################# PRE-LOADING ######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


# sourcing initialisation parameters
source("traditional_models.R")
source("GARCH_models.R")




# Christoffersen test 
ChristoffersenBacktestForUnconditionalCoverage()
ChristoffersenBacktestForIndependence()


# combined tests (from quarks)
quarks::cvgtest() # alternative / addition to the christoffersen test

