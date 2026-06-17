# Trade Elasticities with VAR and VEC Models

Applied time series econometrics project for estimating Argentina's foreign trade
elasticities using multivariate VAR and VEC methodologies.

The project corresponds to Practical Assignment 3 for the Time Series course in the
Master's Program in Applied Economics at the University of Buenos Aires.

---

## Overview

This assignment extends the previous trade elasticities analysis by incorporating
multivariate methods based on vector autoregressions and vector error correction
models. The goal is to estimate and compare short-run and long-run elasticities for
Argentina's foreign trade flows.

The classical trade equations relate:

- Imports to domestic GDP and the real exchange rate.
- Exports to global or trade-partner GDP and the real exchange rate.

When the variables are expressed in natural logarithms, the estimated coefficients
can be interpreted as elasticities.

---

## Main Objectives

- Reuse and document the data from the previous trade elasticities assignment.
- Analyze unit roots, integration order, and seasonality in logarithmic series.
- Test for cointegration using Engle-Granger and Johansen methodologies.
- Estimate long-run elasticities through cointegration relationships.
- Estimate short-run elasticities through ECM, VECM, or VAR models in differences.
- Compare results across methodologies and against the empirical literature.
- Produce a final article-style report in Word and PDF formats.
- Keep the R script reproducible and easy to follow for correction.

---

## Methodological Roadmap

1. Graph the series and run descriptive analysis.
2. Test for unit roots, integration order, and seasonality.
3. Estimate Engle-Granger cointegration relationships.
4. Estimate Johansen cointegration tests and VECM specifications.
5. Estimate models in differences if cointegration is not supported.
6. Compare long-run and short-run elasticities across methods.
7. Summarize economic interpretation, validity, and methodological caveats.

---

## Repository Structure

```text
trade-elasticities-var-vec/
|
├── README.md
|
├── data/
|   ├── raw/
|   └── processed/
|
├── docs/
|   └── assignment_prompt_tp3_2025.pdf
|
├── figures/
|
├── outputs/
|
├── report/
|
└── scripts/
```

---

## Expected Inputs

The assignment states that the data should be the same series used in Practical
Assignment 2:

- Argentine GDP.
- Argentine imports.
- Argentine exports.
- Real exchange rate.
- Global or trade-partner GDP.

All core variables should be analyzed in natural logarithms.

The main analysis-ready input is:

```text
trade-elasticities-var-vec/data/processed/trade_elasticities_var_vec_panel_transformed_2004_2025.csv
```

Raw source-level inputs are stored in `data/raw/`, while processed and
model-ready panels are stored in `data/processed/`.

---

## Expected Deliverables

- Final report in Word format.
- Final report in PDF format.
- R script that runs without interruption once the required series and packages are
  available.
- Supporting tables, figures, and model outputs used in the report.

---

## Tools

The empirical work will be developed in R.

Likely packages include:

- `readxl`
- `dplyr`
- `ggplot2`
- `urca`
- `vars`
- `tsDyn`
- `tseries`
- `lmtest`
- `sandwich`

The final package list should be updated once the main script is developed.

---

## Academic Context

Graduate coursework project developed within:

- Time Series Analysis
- Applied Econometrics
- Vector Autoregressions
- Cointegration and Error Correction Models
- International Trade Elasticities

Faculty of Economic Sciences  
Universidad de Buenos Aires (UBA)
