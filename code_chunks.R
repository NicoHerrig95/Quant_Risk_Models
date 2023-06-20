# Code chunks for later usage


# backtesting for Neural Network



# LSTM - Neural Network
# Unconditional Coverage Test for Neural Net
results_backtesting[[i]]$LRuc[4] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                          VaR = eval(as.name(test_sets[i]))$NNet_VaR)$POF
# NNet - proportion of overshoots
results_backtesting[[i]]$proportion_overshoots[4] <-  Kupiec_Coverage_test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                           VaR = eval(as.name(test_sets[i]))$NNet_VaR)$proportion_overshoots

# LRcci
results_backtesting[[i]]$LRcci[4] <- Christoffersen_Interval_Forecast_Test(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                                           VaR = eval(as.name(test_sets[i]))$NNet_VaR)$LRcci

# LRcc (conditional coverage test)
results_backtesting[[i]]$LRcc[4] <- results_backtesting[[i]]$LRuc[4] + results_backtesting[[i]]$LRcci[4]


# mean dev & max dev
results_backtesting[[i]]$mean_dev[4] <- deviation_function(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                           VaR = eval(as.name(test_sets[i]))$NNet_VaR)$mean_dev


results_backtesting[[i]]$max_dev[4] <- deviation_function(Losses = eval(as.name(test_sets[i]))$one_day_loss,
                                                          VaR = eval(as.name(test_sets[i]))$NNet_VaR)$max_dev