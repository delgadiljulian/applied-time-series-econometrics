# Trade Elasticities and Cointegration

Applied time series econometrics project for estimating Argentina's foreign trade
elasticities using cointegration methods and short-run dynamic models.

The project corresponds to Practical Assignment 2 for the Time Series course in the
Master's Program in Applied Economics at the University of Buenos Aires.

---

## Team Members

- Andrea Chasi
- Julián Delgadillo Marín
- Christian Arias
- Leonardo Ávila

---

## Overview

This project studies the long-run and short-run relationships between Argentina's
foreign trade flows and their macroeconomic determinants.

The classical foreign trade equations relate:

- Imports to domestic GDP and the real exchange rate.
- Exports to global or trade-partner GDP and the real exchange rate.

When variables are measured in natural logarithms, the estimated coefficients can be
interpreted as trade elasticities. Because the relevant macroeconomic variables are
typically non-stationary, the project focuses on testing for cointegration before
interpreting long-run relationships.

---

## Main Objectives

- Build a quarterly macroeconomic dataset for Argentina from 2004Q1 to 2025Q4.
- Analyze unit roots, integration order, and seasonality in the relevant series.
- Estimate long-run import and export equations.
- Test for cointegration using the Engle-Granger methodology.
- Evaluate possible cointegration with structural breaks using the Gregory-Hansen test.
- Estimate short-run trade elasticities through models in first differences.
- Estimate error correction models when cointegration is supported.
- Compare results with selected empirical references on Argentina's trade elasticities.

---

## Variables

The assignment considers the following series:

- Argentine GDP at constant prices.
- Argentine exports at constant prices.
- Argentine imports at constant prices.
- Multilateral real exchange rate index.
- Trade-partner GDP index.

All core variables are expected to be analyzed in natural logarithms.

---

## Methodological Framework

The empirical workflow includes:

1. Time series visualization and exploratory analysis.
2. Unit root testing and integration order assessment.
3. Cointegration analysis through Engle-Granger tests.
4. Cointegration analysis with structural breaks through Gregory-Hansen tests.
5. Estimation of short-run models in first differences.
6. Estimation of error correction models when applicable.
7. Comparison with previous empirical literature.

---

## Repository Structure

```text
trade-elasticities-cointegration/
|
├── README.md
|
├── data/
|   └── .gitkeep
|
├── docs/
|   ├── .gitkeep
|   └── assignment_prompt.pdf
|
├── figures/
|   └── .gitkeep
|
├── outputs/
|   └── .gitkeep
|
├── report/
|   └── .gitkeep
|
└── scripts/
    └── .gitkeep
```

---

## Tools

The empirical work will be developed in R.

Likely packages include:

- `readxl`
- `dplyr`
- `ggplot2`
- `urca`
- `tseries`
- `dynlm`
- `lmtest`
- `sandwich`

The final package list will be updated once the empirical script is developed.

---

## Academic Context

Graduate coursework project developed within:

- Time Series Analysis
- Applied Econometrics
- Macroeconomic Forecasting and Modeling
- International Trade Elasticities

Faculty of Economic Sciences  
Universidad de Buenos Aires (UBA)

---

## License

This project is released under the MIT License.
