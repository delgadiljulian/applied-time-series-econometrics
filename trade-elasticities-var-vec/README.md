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

## Course Tools and R Packages

The empirical work is developed in R and follows the workflow used in the Time
Series course. The master script checks the availability of the course packages
and loads the auxiliary scripts provided in class.

### R packages used from the course workflow

- `dynlm`: dynamic linear models and class ADF workflows.
- `forecast`: ARIMA and forecasting tools.
- `ivreg`: instrumental variables routines.
- `lmtest`: regression diagnostics and hypothesis tests.
- `lpirfs`: local projections and impulse-response extensions.
- `mFilter`: Hodrick-Prescott filtering.
- `nlme`: GLS and maximum-likelihood models.
- `readxl`: Excel input handling.
- `sandwich`: robust covariance estimators.
- `sarima`: SARIMA models.
- `seasonal`: seasonal adjustment.
- `strucchangeRcpp`: structural-break tools.
- `urca`: unit roots, Engle-Granger, and Johansen cointegration.
- `vars`: VAR, VECM-related workflows, IRF, FEVD, and SVAR routines.

The assignment also references SVAR extensions. The script documents the
availability of `svars` as an optional CRAN package, while the implemented SVAR
checks rely on the available `vars` workflow.

### Class scripts used

The following scripts from the course are stored under `scripts/required/` and
are loaded by the master script:

- `Test.ADF_Ver.3.R`: class ADF unit-root test routine.
- `vcorr_res.R`: residual autocorrelation diagnostics for VAR models.
- `VAR_white_no_cross.R`: White no-cross heteroskedasticity test.
- `VAR_lag_exclusion_wald.R`: Wald test for joint lag exclusion in VAR systems.

The current load status of these scripts is exported by:

```text
outputs/00_setup/section_00_course_scripts_status.csv
```

The package availability check is exported by:

```text
outputs/00_setup/section_00_course_packages_status.csv
```

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
