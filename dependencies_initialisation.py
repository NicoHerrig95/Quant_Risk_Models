# -*- coding: utf-8 -*-
"""
Project:Neural Network for VaR forecasting
Script name: dependencies
@author: nicoherrig
"""

# a list of stocks to be loaded
# subject to possible change
stocks = ["AAPL", "JPM"]


# start date and end date of testing period
# shall be the same as in the "initialisation.R" script
test_start_date = "2021-01-01"
test_end_date = "2022-12-31"



# Depdendencies
import numpy as np
import pandas as pd



# the file path for data loading 
file_path = "/Users/nicoherrig/Desktop/Dissertation/folder_git/Neural_network_VaR/data_files/"


# reading in data
Data = {}
for x in stocks:
    Data[x] = pd.read_csv(file_path + x + ".csv")
    # removing redundand columns
    Data[x].drop(columns = ["Unnamed: 0"], axis=1, inplace=True)



