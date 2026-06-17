# Data Directory

This folder contains the data workspace for Practical Assignment 3 on Argentina's
foreign trade elasticities using VAR and VEC models.

The assignment states that TP 3 uses the same data as TP 2. For that reason, this
folder starts from the cleaned data pipeline developed in
`trade-elasticities-cointegration`, but stores local copies so that TP 3 remains
self-contained.

## Folder Structure

```text
data/
|
├── raw/
|   ├── arg_trade_data_2004_2025.xls
|   ├── arg_itcrm_daily.xlsx
|   ├── arg_itcrm_methodology.pdf
|   ├── brazil_real_gdp_quarterly.csv
|   ├── usa_real_gdp_quarterly.csv
|   └── world_bank_commodity_index.xlsx
|
└── processed/
    ├── trade_elasticities_var_vec_panel_base_2004_2025.csv
    ├── trade_elasticities_var_vec_panel_transformed_2004_2025.csv
    ├── trade_elasticities_var_vec_panel_complete_cases_2004_2025.csv
    └── trade_elasticities_var_vec_panel_modeling_2004_2025.dta
```

## Raw Data

The `raw/` folder stores the original input files or source-level exports:

- `arg_trade_data_2004_2025.xls`: Argentina quarterly GDP, imports, and exports.
- `arg_itcrm_daily.xlsx`: Argentina daily multilateral real exchange rate index.
- `arg_itcrm_methodology.pdf`: ITCRM source methodology.
- `brazil_real_gdp_quarterly.csv`: Brazil real GDP quarterly series.
- `usa_real_gdp_quarterly.csv`: United States real GDP quarterly series.
- `world_bank_commodity_index.xlsx`: World Bank commodity price index.

These files should not be edited manually.

## Processed Data

The `processed/` folder stores analysis-ready datasets:

- `trade_elasticities_var_vec_panel_base_2004_2025.csv`

  Merged quarterly panel with the main level variables: Argentina GDP, imports,
  exports, ITCRM, Brazil GDP, US GDP, commodity prices, and partner-GDP index.

- `trade_elasticities_var_vec_panel_transformed_2004_2025.csv`

  Main starting dataset for TP 3. It includes levels, natural logarithms, first
  log differences, growth rates, and first lags. This is the recommended input
  for ADF tests, Engle-Granger replication, Johansen tests, VAR, and VEC models.

- `trade_elasticities_var_vec_panel_complete_cases_2004_2025.csv`

  Complete-case version of the transformed panel. Useful for quick checks, but
  the econometric script should still define explicit samples for each model.

- `trade_elasticities_var_vec_panel_modeling_2004_2025.dta`

  Modeling dataset inherited from the TP 2 pipeline. It may include additional
  variables such as residuals or model-specific fields used in Engle-Granger and
  ECM stages. This file is useful for replicating the TP 2 baseline before adding
  the Johansen/VEC block.

## Recommended Workflow

1. Use `trade_elasticities_var_vec_panel_transformed_2004_2025.csv` as the main
   input for the TP 3 script.
2. Replicate the relevant TP 2 Engle-Granger/ECM baseline using the transformed
   panel or the modeling `.dta` file when residual variables are needed.
3. Estimate Johansen tests and VEC/VAR models from the transformed panel, using
   explicit model-specific complete samples.
4. Export all new TP 3 tables and diagnostics to `../outputs/`.

