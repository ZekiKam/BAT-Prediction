# BAT Price Prediction with Random Forest

This project builds a supervised machine learning pipeline to predict the Basic Attention Token (BAT) price in USD from historical OHLCV data using a Random Forest regression model

## Overview

- Downloads daily BAT–USD data from Yahoo Finance via `yfinance`
- Cleans and structures the OHLCV time series with `pandas`
- Trains a multi‑output `RandomForestRegressor` to predict the next‑step OHLC prices
- Evaluates performance on a temporally ordered train/test split (80%/20%)

## Data

- **Source**: Yahoo Finance `BAT-USD` ticker 
- **Date range**: 2017‑09‑11 to 2024‑09‑20 
- **Raw columns**: 
  - `Date`  
  - `Open`  
  - `High`  
  - `Low`  
  - `Close`  
  - `Adj Close`  
  - `Volume`  

The notebook saves the downloaded data to `BAT_historical_data.csv` and reloads it as a `pandas` DataFrame, parsing `Date` as `datetime` and dropping any rows with missing values. 

## Problem Formulation

- **Features (X)**:  
  `['Open', 'High', 'Low', 'Close', 'Adj Close', 'Volume']` for each day
- **Targets (y)**:  
  `['Open', 'High', 'Low', 'Close']`, modeled as a **multi‑output regression** problem (predicting a 4D vector)

This formulation allows the model to jointly learn the relationships between different price components at the next time step.

## Model and Training

- **Algorithm**: `RandomForestRegressor` from `scikit-learn`
- **Key settings**:  
  - `random_state=42` -> reproducibility
  - Multi‑output regression handled natively by Scikit‑learn

### Train/Test Split

- The dataset is split into train and test sets 80%/20% 
- `shuffle=False` ensures temporal ordering is preserved: the model trains on earlier history and is tested on more recent data


