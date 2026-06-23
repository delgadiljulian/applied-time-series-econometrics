# Applied Time Series Econometrics

Applied econometrics and time series analysis projects developed during graduate studies in economics using R.

This repository contains applied work focused on macroeconomic forecasting, time series modeling, seasonal adjustment, and econometric analysis. The projects emphasize reproducible workflows, statistical diagnostics, and out-of-sample forecast evaluation using classical and modern time series methodologies.

---

## Main Topics

- Box–Jenkins methodology
- ARIMA and SARIMA models
- Seasonal adjustment with X-13ARIMA-SEATS
- Cointegration and error correction models
- Foreign trade elasticities
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
├── box-jenkins-forecasting/
│   ├── data/
│   ├── docs/
│   ├── figures/
│   ├── outputs/
│   ├── report/
│   └── scripts/
│
├── trade-elasticities-cointegration/
│   ├── data/
│   ├── docs/
│   ├── figures/
│   ├── outputs/
│   ├── report/
│   └── scripts/
│
├── trade-elasticities-var-vec/
│   ├── data/
│   ├── docs/
│   ├── figures/
│   ├── outputs/
│   ├── report/
│   └── scripts/
│
├── local_clases_teorico_practicas/
│   └── course reference material used locally
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

### Trade Elasticities and Cointegration

Cointegration-based analysis of Argentina's foreign trade elasticities using:

- Unit root and integration order tests
- Engle-Granger cointegration methodology
- Gregory-Hansen cointegration tests with structural breaks
- Short-run models in first differences
- Error correction models when cointegration is supported

Analyzed variables include:

- Argentine GDP
- Exports
- Imports
- Multilateral real exchange rate
- Trade-partner GDP

### Trade Elasticities with VAR and VEC Models

Multivariate extension of the trade elasticities analysis using:

- Engle-Granger and Johansen cointegration tests
- VAR models in differences when cointegration is not supported
- VECM specifications when cointegration is supported
- Long-run and short-run elasticity comparison
- Article-style reporting with reproducible R scripts

Analyzed variables include:

- Argentine GDP
- Exports
- Imports
- Real exchange rate
- Global or trade-partner GDP
- Commodity price index as an external short-run control

---

## Tools and Libraries

### Language

- R

### Main Packages

- dynlm
- forecast
- ivreg
- lmtest
- lpirfs
- mFilter
- nlme
- readxl
- sandwich
- sarima
- seasonal
- strucchangeRcpp
- urca
- vars

Some project scripts also use project-specific data wrangling and reporting
utilities. The VAR/VEC project documents the course package and script checks in
`trade-elasticities-var-vec/README.md`.

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
8. Cointegration and VAR/VEC diagnostics where required

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
