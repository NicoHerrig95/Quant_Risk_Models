######################### BENCHMARKING #########################################


install.packages("quarks")
library(quarks) # alternative package for backtesting

# Christoffersen test 
ChristoffersenBacktestForUnconditionalCoverage()
ChristoffersenBacktestForIndependence()



# combined tests (from quarks)
quarks::cvgtest() # alternative / addition to the christoffersen test

