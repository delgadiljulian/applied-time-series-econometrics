# Applied Time Series Econometrics

Applied econometrics and time series analysis projects developed during graduate studies in economics using R.

This repository contains applied work focused on macroeconomic forecasting, time series modeling, seasonal adjustment, and econometric analysis. The projects emphasize reproducible workflows, statistical diagnostics, and out-of-sample forecast evaluation using classical and modern time series methodologies.

---

## Main Topics

- Box–Jenkins methodology
- ARIMA and SARIMA models
- Seasonal adjustment with X-13ARIMA-SEATS
- Forecast evaluation and model comparison
- Macroeconomic time series analysis
- Stationarity and autocorrelation analysis
- Residual diagnostics and model validation
- Applied forecasting in R

---

## Repository Structure

```text
applied-time-series-econometrics/
│
├── box-jenkins/
│   ├── tp1-sarima-x13/
│   ├── tp2/
│   ├── tp3/
│   └── tp4/
│
├── forecasting/
│
├── tutorials/
│
├── figures/
│
└── README.md
```

## Current Projects

### Box–Jenkins Methodology and Seasonal Adjustment

Comparative forecasting analysis using:

- SARIMA models estimated on original macroeconomic series
- ARMA models estimated on seasonally adjusted series
- X-13 seasonal adjustment procedure
- Out-of-sample forecast evaluation

Analyzed variables include:

- Private consumption
- Exports
- Gross fixed investment
- Gross domestic product (GDP)

---

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

## Methodological Approach

The repository follows a reproducible econometric workflow based on:

1. Exploratory analysis of time series
2. ACF and PACF diagnostics
3. Model identification
4. Estimation and validation
5. Residual diagnostics
6. Forecast generation
7. Out-of-sample evaluation

Forecast accuracy is evaluated using metrics such as:

- RMSE
- MAE
- MAPE
- Theil’s U

---

## Academic Context

Projects developed as part of graduate coursework in:

- Applied Econometrics
- Time Series Analysis
- Forecasting Methods
- Applied Macroeconomics

Faculty of Economic Sciences — Universidad de Buenos Aires (UBA)

---

## License

This repository is released under the MIT License.
