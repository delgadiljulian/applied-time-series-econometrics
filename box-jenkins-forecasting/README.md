# Box–Jenkins Forecasting and Seasonal Adjustment

Applied time series forecasting project based on the Box–Jenkins methodology using quarterly macroeconomic data from Argentina. The project compares forecasting performance between SARIMA models estimated on original series and ARMA models estimated on seasonally adjusted series using the X-13ARIMA-SEATS procedure.

---

## Overview

This project develops and evaluates multiple forecasting strategies for macroeconomic time series through:

- Box–Jenkins methodology
- SARIMA modeling
- Seasonal adjustment with X-13ARIMA-SEATS
- ARMA modeling on adjusted series
- Residual diagnostics and model validation
- Out-of-sample forecast comparison

The analysis emphasizes model identification, seasonal dynamics, forecast accuracy, and comparative econometric performance.

---

## Main Objectives

- Identify stochastic and seasonal structures in macroeconomic time series
- Estimate SARIMA and ARMA forecasting models
- Compare forecasting performance across methodologies
- Evaluate residual behavior and model adequacy
- Assess forecast accuracy using standard econometric metrics

---

## Variables Analyzed

The project includes quarterly macroeconomic series such as:

- Private consumption
- Exports
- Gross fixed investment
- Gross domestic product (GDP)

---

## Methodological Framework

### 1. Exploratory Analysis

- Time series visualization
- Trend and seasonality inspection
- Stationarity assessment

### 2. Identification Stage

- ACF and PACF diagnostics
- Seasonal structure analysis
- Candidate model selection

### 3. Estimation

Two complementary approaches are implemented:

#### SARIMA Models
Estimated directly on the original series.

#### ARMA + X-13 Models
Estimated on seasonally adjusted series obtained using the X-13ARIMA-SEATS procedure.

### 4. Diagnostic Evaluation

- Residual autocorrelation analysis
- Ljung–Box tests
- Residual normality inspection
- Forecast consistency checks

### 5. Forecast Evaluation

Forecast accuracy is evaluated using:

- RMSE
- MAE
- MAPE
- Theil’s U

---

## Repository Structure

```text
box-jenkins-forecasting/
│
├── README.md
│
├── report/
│   ├── box_jenkins_seasonality_forecasting_report.pdf
│   └── box_jenkins_forecasting_full_report.pdf
│
├── scripts/
│   └── box_jenkins_forecasting_analysis.R
│
├── figures/
│
├── data/
│   └── macroeconomic_quarterly_series.xlsx
│
├── outputs/
│
└── docs/
    └── box_jenkins_methodology_guidelines.pdf
```

## Tools and Libraries

### Language

- R

### Main Packages

- forecast
- seasonal
- tseries
- ggplot2
- readxl
- dplyr

---

## Academic Context

Graduate coursework project developed within:

- Time Series Analysis
- Applied Econometrics
- Forecasting Methods
- Applied Macroeconomics

Faculty of Economic Sciences  
Universidad de Buenos Aires (UBA)

---

## Notes

This repository is intended as a reproducible applied econometrics workflow for time series forecasting and seasonal adjustment analysis.

Some reports included in this repository correspond to collaborative academic work developed as part of graduate coursework activities.

---

## License

This project is released under the MIT License.
