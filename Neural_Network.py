#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Project:Neural Network for VaR forecasting
Script name: Model
@author: nicoherrig
"""

# sourcing parameters from dependencies
import dependencies_initialisation as loader

# importing packages
import pandas as pd
import tensorflow as tf
import numpy as np
import keras
#import mdn 

# sourcing objects and functions from dependencies_initialisation script
stocks = loader.stocks # list of stocks
data = loader.Data # stock data
test_start_date = pd.to_datetime(loader.test_start_date).date()
test_end_date = pd.to_datetime(loader.test_end_date).date()


# data subset for testing purposes
test_data = data["AAPL"]
test_data["date"] = pd.to_datetime(test_data["date"])
test_data["date"] = test_data["date"].dt.date
test_data = test_data[(test_data["date"] > test_start_date) &  (test_data["date"] < test_end_date)]
test_data.reset_index()
    

def table_transform(df, rolling_window = 10 ):
    # input data frame for training
    df_con = df.filter(["date","R"]).tail(-1).reset_index(drop = True)

    d = rolling_window # window size
    location = 2 # value for storage location

    # loop for generating columns for past returns
    for i in range(1, d+1):
        df_con.insert(loc = location, column = "R_t-"+str(i), value = np.nan)
        location = location + 1

    del location
    
    # pasting past returns into columns
    for i in range(0, len(df_con) - d):
        df_con.iloc[i+d, 2:] = df_con.loc[i:(i+d-1), "R"][::-1]
    
    
    return df_con



x = table_transform(df = data["AAPL"], rolling_window = 250)

