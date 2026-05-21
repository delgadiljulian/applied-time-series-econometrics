# =========================================================
# UNIVERSIDAD DE BUENOS AIRES (UBA)
# Facultad de Ciencias Económicas
# Maestría en Economía Aplicada
#
# Materia:
# Econometría / Series de Tiempo
#
# Proyecto:
# Elasticidades del Comercio Exterior Argentino
# (Largo y Corto Plazo)
#
# Integrantes:
# - Crhistian
# - Andrea
# - Julián Delgadillo Marín
#
# Período de análisis:
# 2004Q1 – 2025Q4
#
# Objetivo general:
# Estimar las elasticidades de largo y corto plazo
# del comercio exterior argentino utilizando modelos
# de cointegración y mecanismos de corrección de error.
#
# Variables principales:
# - PIB real Argentina
# - Exportaciones reales
# - Importaciones reales
# - Tipo de Cambio Real Multilateral (ITCRM)
# - PIB de socios comerciales (Brazil y EEUU)
# - Commodity Price Index (World Bank Pink Sheet)
#
# Fuentes de información:
# - INDEC
# - BCRA
# - FRED (Federal Reserve Economic Data)
# - World Bank Commodity Price Data (Pink Sheet)
#
# =========================================================
# 1. INICIALIZACIÓN DEL ENTORNO
# =========================================================

# Limpiar objetos previos.
rm(list = ls())

# Cerrar graficos abiertos.
graphics.off()

# Evitar notacion cientifica.
options(scipen = 999)

# =========================================================
# 2. CARGA DE PAQUETES
# =========================================================

# Manipulacion de datos
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

# Series de tiempo
library(zoo)
library(xts)
library(tsibble)

# Econometria y cointegracion
library(urca)
library(tseries)
library(dynlm)
library(lmtest)
library(sandwich)

# Visualización
library(ggplot2)
library(scales)

# Exportacion
library(writexl)
library(haven)

# =========================================================
# 3. DIRECTORIOS DE TRABAJO
# =========================================================

# Ruta principal del proyecto
project_dir <- "C:/Users/julla/GitHub/applied-time-series-econometrics/trade-elasticities-cointegration"

# Carpetas del proyecto
raw_data_dir       <- file.path(project_dir, "data/raw")
processed_data_dir <- file.path(project_dir, "data/processed")
output_dir         <- file.path(project_dir, "outputs")
figures_dir        <- file.path(project_dir, "figures")
scripts_dir        <- file.path(project_dir, "scripts")
report_dir         <- file.path(project_dir, "report")
docs_dir           <- file.path(project_dir, "docs")

# =========================================================
# 4. ARCHIVOS DE ENTRADA
# =========================================================

# ---------------------------------------------------------
# 4.1 Argentina
# ---------------------------------------------------------

# INDEC: PIB, importaciones y exportaciones reales
arg_trade_file <- file.path(
  raw_data_dir,
  "arg_trade_data_2004_2025.xls"
)

# BCRA: ITCRM diario
itcrm_file <- file.path(
  raw_data_dir,
  "arg_itcrm_daily.xlsx"
)

# ---------------------------------------------------------
# 4.2 Socios comerciales
# ---------------------------------------------------------

# FRED: PIB real trimestral de Estados Unidos
usa_gdp_file <- file.path(
  raw_data_dir,
  "usa_real_gdp_quarterly.csv"
)

# FRED: PIB real trimestral de Brasil
brazil_gdp_file <- file.path(
  raw_data_dir,
  "brazil_real_gdp_quarterly.csv"
)

# ---------------------------------------------------------
# 4.3 Commodities
# ---------------------------------------------------------

# Banco Mundial: indices mensuales de commodities
commodity_file <- file.path(
  raw_data_dir,
  "world_bank_commodity_index.xlsx"
)

# =========================================================
# 5. IMPORTACIÓN Y LIMPIEZA DE DATOS
# =========================================================

# ---------------------------------------------------------
# 5.1 Comercio exterior argentino
# ---------------------------------------------------------

# Objetivos:
# - Importar base de INDEC
# - Seleccionar variables relevantes
# - Construir fechas trimestrales
# - Renombrar variables
# - Verificar valores faltantes
# - Generar serie temporal

# Verificar que el archivo exista.
if (!file.exists(arg_trade_file)) {
  stop("No se encontro el archivo de comercio exterior argentino: ", arg_trade_file)
}

# Crear carpeta de datos procesados.
dir.create(processed_data_dir, recursive = TRUE, showWarnings = FALSE)

# Leer planilla INDEC sin encabezados.
arg_trade_raw <- read_excel(
  arg_trade_file,
  sheet = 1,
  col_names = FALSE,
  .name_repair = "minimal"
)

# Nombrar columnas manualmente.
names(arg_trade_raw) <- paste0("col_", seq_along(arg_trade_raw))

# Primera columna: nombres de variables.
variable_col <- names(arg_trade_raw)[1]

# Filas de años y trimestres.
year_row <- arg_trade_raw[4, ]
quarter_row <- arg_trade_raw[5, ]

# Mapa columna -> año/trimestre.
time_cols <- tibble(
  col_name = names(arg_trade_raw),
  col_id = seq_along(col_name),
  year = as.character(unlist(year_row, use.names = FALSE)),
  quarter_label = as.character(unlist(quarter_row, use.names = FALSE))
) %>%
  mutate(
    year = na_if(str_squish(year), ""),
    quarter_label = str_squish(quarter_label)
  ) %>%
  # Completar año dentro de cada bloque anual.
  fill(year, .direction = "down") %>%
  mutate(
    year = suppressWarnings(as.integer(year)),
    # Identificar trimestre; "Total" queda excluido.
    q = case_when(
      str_detect(quarter_label, "^1") ~ 1L,
      str_detect(quarter_label, "^2") ~ 2L,
      str_detect(quarter_label, "^3") ~ 3L,
      str_detect(quarter_label, "^4") ~ 4L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(year), !is.na(q))

# Convertir valores de Excel a numericos.
clean_numeric <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- str_squish(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  parse_number(
    x_chr,
    locale = locale(grouping_mark = ",", decimal_mark = ".")
  )
}

# Pasar a formato largo: trimestre-variable-valor.
arg_trade_long <- arg_trade_raw %>%
  # Nombre original de la variable.
  mutate(variable_raw = str_squish(as.character(.data[[variable_col]]))) %>%
  # Series necesarias para el TP.
  filter(variable_raw %in% c(
    "Producto Interno Bruto",
    "Importaciones FOB (bienes y servicios reales)",
    "Exportaciones FOB (bienes y servicios reales)"
  )) %>%
  # Mantener solo columnas trimestrales.
  select(variable_raw, all_of(time_cols$col_name)) %>%
  # De ancho a largo.
  pivot_longer(
    cols = -variable_raw,
    names_to = "col_name",
    values_to = "value_raw"
  ) %>%
  # Agregar calendario trimestral.
  left_join(time_cols, by = "col_name") %>%
  filter(!is.na(year), !is.na(q)) %>%
  mutate(
    # Nombres cortos para modelar.
    variable = case_when(
      variable_raw == "Producto Interno Bruto" ~ "gdp_real",
      variable_raw == "Importaciones FOB (bienes y servicios reales)" ~ "imports_real",
      variable_raw == "Exportaciones FOB (bienes y servicios reales)" ~ "exports_real"
    ),
    value = clean_numeric(value_raw),
    # Fechas trimestrales.
    quarter = paste0(year, "Q", q),
    quarter_date = as.Date(as.yearqtr(quarter, format = "%YQ%q")),
    stata_qdate = (year - 1960) * 4 + q - 1
  ) %>%
  select(quarter, year, q, quarter_date, stata_qdate, variable, value)

# Base final: una fila por trimestre.
arg_trade_processed <- arg_trade_long %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  arrange(year, q) %>%
  mutate(
    # Logs naturales.
    ln_gdp_real = log(gdp_real),
    ln_imports_real = log(imports_real),
    ln_exports_real = log(exports_real)
  )

# Revisar valores faltantes.
arg_trade_missing <- arg_trade_processed %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  )

print(arg_trade_missing)

# Exportar a Stata.
write_dta(
  arg_trade_processed,
  file.path(processed_data_dir, "arg_trade_data_2004_2025.dta")
)

# Exportar copia CSV.
write_csv(
  arg_trade_processed,
  file.path(processed_data_dir, "arg_trade_data_2004_2025.csv")
)

# Reabrir .dta exportado.
arg_trade_dta <- read_dta(
  file.path(processed_data_dir, "arg_trade_data_2004_2025.dta")
)

# Abrir en visor de RStudio.
View(arg_trade_dta)


# ---------------------------------------------------------
# 5.2 Tipo de Cambio Real Multilateral (ITCRM)
# ---------------------------------------------------------

# Objetivos:
# - Importar serie diaria del BCRA
# - Convertir fechas
# - Agregar frecuencia trimestral
# - Calcular promedio trimestral
# - Homogeneizar período de análisis

# Verificar que el archivo exista.
if (!file.exists(itcrm_file)) {
  stop("No se encontro el archivo de ITCRM diario: ", itcrm_file)
}

# Leer hoja diaria BCRA. La primera fila contiene la base del indice,
# por eso los nombres de columnas estan en la segunda fila.
itcrm_daily_raw <- read_excel(
  itcrm_file,
  sheet = "ITCRM y bilaterales",
  skip = 1
) %>%
  clean_names()

# Base diaria: conservar solo fecha e ITCRM multilateral.
itcrm_daily <- itcrm_daily_raw %>%
  transmute(
    date = as.Date(periodo),
    itcrm = as.numeric(itcrm)
  ) %>%
  filter(
    !is.na(date),
    !is.na(itcrm)
  ) %>%
  mutate(
    year = year(date),
    q = quarter(date),
    quarter = paste0(year, "Q", q),
    quarter_date = as.Date(as.yearqtr(quarter, format = "%YQ%q")),
    stata_qdate = (year - 1960) * 4 + q - 1
  )

# Base trimestral: promedio simple del ITCRM diario dentro de cada trimestre.
itcrm_quarterly <- itcrm_daily %>%
  filter(
    quarter_date >= as.Date("2004-01-01"),
    quarter_date <= as.Date("2025-10-01")
  ) %>%
  group_by(quarter, year, q, quarter_date, stata_qdate) %>%
  summarise(
    itcrm = mean(itcrm, na.rm = TRUE),
    itcrm_daily_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(year, q) %>%
  mutate(
    ln_itcrm = log(itcrm)
  ) %>%
  select(
    quarter,
    year,
    q,
    quarter_date,
    stata_qdate,
    itcrm,
    ln_itcrm,
    itcrm_daily_obs
  )

# Revisar valores faltantes.
itcrm_missing <- itcrm_quarterly %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  )

print(itcrm_missing)

# Exportar a Stata.
write_dta(
  itcrm_quarterly,
  file.path(processed_data_dir, "arg_itcrm_quarterly_2004_2025.dta")
)

# Exportar copia CSV.
write_csv(
  itcrm_quarterly,
  file.path(processed_data_dir, "arg_itcrm_quarterly_2004_2025.csv")
)

# Reabrir .dta exportado.
itcrm_dta <- read_dta(
  file.path(processed_data_dir, "arg_itcrm_quarterly_2004_2025.dta")
)

# Abrir en visor de RStudio.
View(itcrm_dta)


# ---------------------------------------------------------
# 5.3 PIB real de Estados Unidos
# ---------------------------------------------------------

# Objetivos:
# - Importar datos FRED
# - Limpiar columnas
# - Convertir fechas trimestrales
# - Renombrar variable
# - Verificar frecuencia y cobertura

# Verificar que el archivo exista.
if (!file.exists(usa_gdp_file)) {
  stop("No se encontro el archivo de PIB real de Estados Unidos: ", usa_gdp_file)
}

# Leer serie trimestral FRED: Real Gross Domestic Product (GDPC1).
usa_gdp_raw <- read_csv(
  usa_gdp_file,
  show_col_types = FALSE
) %>%
  clean_names()

# Base trimestral limpia.
usa_gdp_quarterly <- usa_gdp_raw %>%
  transmute(
    quarter_date = as.Date(observation_date),
    usa_gdp_real = as.numeric(gdpc1)
  ) %>%
  filter(
    !is.na(quarter_date),
    !is.na(usa_gdp_real),
    quarter_date >= as.Date("2004-01-01"),
    quarter_date <= as.Date("2025-10-01")
  ) %>%
  mutate(
    year = year(quarter_date),
    q = quarter(quarter_date),
    quarter = paste0(year, "Q", q),
    stata_qdate = (year - 1960) * 4 + q - 1,
    ln_usa_gdp_real = log(usa_gdp_real)
  ) %>%
  select(
    quarter,
    year,
    q,
    quarter_date,
    stata_qdate,
    usa_gdp_real,
    ln_usa_gdp_real
  ) %>%
  arrange(year, q)

# Revisar valores faltantes.
usa_gdp_missing <- usa_gdp_quarterly %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  )

print(usa_gdp_missing)

# Exportar a Stata.
write_dta(
  usa_gdp_quarterly,
  file.path(processed_data_dir, "usa_real_gdp_quarterly_2004_2025.dta")
)

# Exportar copia CSV.
write_csv(
  usa_gdp_quarterly,
  file.path(processed_data_dir, "usa_real_gdp_quarterly_2004_2025.csv")
)

# Reabrir .dta exportado.
usa_gdp_dta <- read_dta(
  file.path(processed_data_dir, "usa_real_gdp_quarterly_2004_2025.dta")
)

# Abrir en visor de RStudio.
View(usa_gdp_dta)


# ---------------------------------------------------------
# 5.4 PIB real de Brasil
# ---------------------------------------------------------

# Objetivos:
# - Importar datos FRED
# - Limpiar columnas
# - Convertir fechas trimestrales
# - Renombrar variable
# - Verificar frecuencia y cobertura

# Verificar que el archivo exista.
if (!file.exists(brazil_gdp_file)) {
  stop("No se encontro el archivo de PIB real de Brasil: ", brazil_gdp_file)
}

# Leer serie trimestral FRED: Real Gross Domestic Product for Brazil.
brazil_gdp_raw <- read_csv(
  brazil_gdp_file,
  show_col_types = FALSE
) %>%
  clean_names()

# Base trimestral limpia.
brazil_gdp_quarterly <- brazil_gdp_raw %>%
  transmute(
    quarter_date = as.Date(observation_date),
    brazil_gdp_real = as.numeric(ngdprsaxdcbrq)
  ) %>%
  filter(
    !is.na(quarter_date),
    !is.na(brazil_gdp_real),
    quarter_date >= as.Date("2004-01-01"),
    quarter_date <= as.Date("2025-10-01")
  ) %>%
  mutate(
    year = year(quarter_date),
    q = quarter(quarter_date),
    quarter = paste0(year, "Q", q),
    stata_qdate = (year - 1960) * 4 + q - 1,
    ln_brazil_gdp_real = log(brazil_gdp_real)
  ) %>%
  select(
    quarter,
    year,
    q,
    quarter_date,
    stata_qdate,
    brazil_gdp_real,
    ln_brazil_gdp_real
  ) %>%
  arrange(year, q)

# Revisar valores faltantes.
brazil_gdp_missing <- brazil_gdp_quarterly %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  )

print(brazil_gdp_missing)

# Exportar a Stata.
write_dta(
  brazil_gdp_quarterly,
  file.path(processed_data_dir, "brazil_real_gdp_quarterly_2004_2025.dta")
)

# Exportar copia CSV.
write_csv(
  brazil_gdp_quarterly,
  file.path(processed_data_dir, "brazil_real_gdp_quarterly_2004_2025.csv")
)

# Reabrir .dta exportado.
brazil_gdp_dta <- read_dta(
  file.path(processed_data_dir, "brazil_real_gdp_quarterly_2004_2025.dta")
)

# Abrir en visor de RStudio.
View(brazil_gdp_dta)


# ---------------------------------------------------------
# 5.5 Commodity Price Index
# ---------------------------------------------------------

# Objetivos:
# - Importar Pink Sheet del Banco Mundial
# - Seleccionar índice relevante
# - Convertir frecuencia mensual a trimestral
# - Construir índice promedio trimestral
# - Homogeneizar fechas

# Verificar que el archivo exista.
if (!file.exists(commodity_file)) {
  stop("No se encontro el archivo de commodities del Banco Mundial: ", commodity_file)
}

# Leer hoja de indices mensuales. La columna B contiene el Total Index,
# que resume el Pink Sheet en base 2010 = 100.
commodity_monthly_raw <- read_excel(
  commodity_file,
  sheet = "Monthly Indices",
  skip = 9,
  col_names = FALSE,
  .name_repair = "minimal"
)

# Base mensual: conservar solo periodo e indice agregado.
commodity_monthly <- commodity_monthly_raw %>%
  select(
    period = 1,
    commodity_price_index = 2
  ) %>%
  mutate(
    period = as.character(period),
    commodity_price_index = as.numeric(commodity_price_index)
  ) %>%
  filter(
    str_detect(period, "^\\d{4}M\\d{2}$"),
    !is.na(commodity_price_index)
  ) %>%
  mutate(
    year = as.integer(str_sub(period, 1, 4)),
    month = as.integer(str_sub(period, 6, 7)),
    month_date = make_date(year, month, 1),
    q = quarter(month_date),
    quarter = paste0(year, "Q", q),
    quarter_date = as.Date(as.yearqtr(quarter, format = "%YQ%q")),
    stata_qdate = (year - 1960) * 4 + q - 1
  )

# Base trimestral: promedio simple del indice mensual dentro de cada trimestre.
commodity_quarterly <- commodity_monthly %>%
  filter(
    quarter_date >= as.Date("2004-01-01"),
    quarter_date <= as.Date("2025-10-01")
  ) %>%
  group_by(quarter, year, q, quarter_date, stata_qdate) %>%
  summarise(
    commodity_price_index = mean(commodity_price_index, na.rm = TRUE),
    commodity_monthly_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(year, q) %>%
  mutate(
    ln_commodity_price_index = log(commodity_price_index)
  ) %>%
  select(
    quarter,
    year,
    q,
    quarter_date,
    stata_qdate,
    commodity_price_index,
    ln_commodity_price_index,
    commodity_monthly_obs
  )

# Revisar valores faltantes.
commodity_missing <- commodity_quarterly %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  )

print(commodity_missing)

# Exportar a Stata.
write_dta(
  commodity_quarterly,
  file.path(processed_data_dir, "world_bank_commodity_index_quarterly_2004_2025.dta")
)

# Exportar copia CSV.
write_csv(
  commodity_quarterly,
  file.path(processed_data_dir, "world_bank_commodity_index_quarterly_2004_2025.csv")
)

# Reabrir .dta exportado.
commodity_dta <- read_dta(
  file.path(processed_data_dir, "world_bank_commodity_index_quarterly_2004_2025.dta")
)

# Abrir en visor de RStudio.
View(commodity_dta)


# =========================================================
# 6. CONSTRUCCIÓN DEL PANEL FINAL
# =========================================================

# Objetivos:
# - Unificar todas las bases
# - Realizar merges por fecha
# - Verificar consistencia temporal
# - Revisar NA's
# - Construir base maestra

# Rutas de bases procesadas.
arg_trade_dta_file <- file.path(processed_data_dir, "arg_trade_data_2004_2025.dta")
itcrm_dta_file <- file.path(processed_data_dir, "arg_itcrm_quarterly_2004_2025.dta")
usa_gdp_dta_file <- file.path(processed_data_dir, "usa_real_gdp_quarterly_2004_2025.dta")
brazil_gdp_dta_file <- file.path(processed_data_dir, "brazil_real_gdp_quarterly_2004_2025.dta")
commodity_dta_file <- file.path(
  processed_data_dir,
  "world_bank_commodity_index_quarterly_2004_2025.dta"
)

# Verificar que todas las bases procesadas existan.
processed_files <- c(
  arg_trade_dta_file,
  itcrm_dta_file,
  usa_gdp_dta_file,
  brazil_gdp_dta_file,
  commodity_dta_file
)

if (any(!file.exists(processed_files))) {
  stop(
    "Faltan bases procesadas para construir el panel final: ",
    paste(processed_files[!file.exists(processed_files)], collapse = "; ")
  )
}

# Calendario maestro trimestral del TP.
panel_calendar <- tibble(
  quarter_date = seq.Date(
    from = as.Date("2004-01-01"),
    to = as.Date("2025-10-01"),
    by = "quarter"
  )
) %>%
  mutate(
    year = year(quarter_date),
    q = quarter(quarter_date),
    quarter = paste0(year, "Q", q),
    stata_qdate = (year - 1960) * 4 + q - 1
  ) %>%
  select(quarter, year, q, quarter_date, stata_qdate)

# Leer bases procesadas desde .dta.
arg_trade_panel <- read_dta(arg_trade_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    gdp_real,
    imports_real,
    exports_real,
    ln_gdp_real,
    ln_imports_real,
    ln_exports_real
  )

itcrm_panel <- read_dta(itcrm_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    itcrm,
    ln_itcrm,
    itcrm_daily_obs
  )

usa_gdp_panel <- read_dta(usa_gdp_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    usa_gdp_real,
    ln_usa_gdp_real
  )

brazil_gdp_panel <- read_dta(brazil_gdp_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    brazil_gdp_real,
    ln_brazil_gdp_real
  )

commodity_panel <- read_dta(commodity_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    commodity_price_index,
    ln_commodity_price_index,
    commodity_monthly_obs
  )

# Unificar bases por fecha trimestral.
trade_elasticities_panel <- panel_calendar %>%
  left_join(arg_trade_panel, by = "quarter_date") %>%
  left_join(itcrm_panel, by = "quarter_date") %>%
  left_join(usa_gdp_panel, by = "quarter_date") %>%
  left_join(brazil_gdp_panel, by = "quarter_date") %>%
  left_join(commodity_panel, by = "quarter_date") %>%
  arrange(year, q)

# Reporte de valores faltantes.
panel_missing <- trade_elasticities_panel %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  )

print(panel_missing)

# Trimestres con algun faltante.
panel_missing_by_quarter <- trade_elasticities_panel %>%
  mutate(missing_values = rowSums(is.na(across(everything())))) %>%
  filter(missing_values > 0) %>%
  select(quarter, quarter_date, missing_values)

print(panel_missing_by_quarter)

# Base con casos completos para la etapa econometrica.
trade_elasticities_panel_complete <- trade_elasticities_panel %>%
  filter(if_all(everything(), ~ !is.na(.x)))

# Exportar panel maestro.
write_dta(
  trade_elasticities_panel,
  file.path(processed_data_dir, "trade_elasticities_panel_2004_2025.dta")
)

write_csv(
  trade_elasticities_panel,
  file.path(processed_data_dir, "trade_elasticities_panel_2004_2025.csv")
)

# Exportar panel de casos completos.
write_dta(
  trade_elasticities_panel_complete,
  file.path(processed_data_dir, "trade_elasticities_panel_complete_cases.dta")
)

write_csv(
  trade_elasticities_panel_complete,
  file.path(processed_data_dir, "trade_elasticities_panel_complete_cases.csv")
)

# Reabrir .dta exportado.
trade_elasticities_panel_dta <- read_dta(
  file.path(processed_data_dir, "trade_elasticities_panel_2004_2025.dta")
)

# Abrir en visor de RStudio.
View(trade_elasticities_panel_dta)


# =========================================================
# 7. TRANSFORMACIONES DE VARIABLES
# =========================================================

# Estado actual:
# - Los logaritmos naturales ya fueron creados en las secciones 5.1 a 5.5.
# - El panel final ya contiene las series en niveles y en logs.
# - La base argentina llega hasta 2022Q4; el resto de las series llega hasta 2025Q4.
#
# Objetivos de esta seccion:
# - Ordenar el panel por stata_qdate.
# - Construir primeras diferencias de los logs para elasticidades de corto plazo.
# - Construir rezagos de logs y de diferencias para modelos dinamicos.
# - Preparar una base de trabajo para las pruebas econometricas.
#
# Variables principales a transformar:
# - ln_imports_real, ln_exports_real, ln_gdp_real, ln_itcrm.
# - ln_usa_gdp_real, ln_brazil_gdp_real.
# - ln_commodity_price_index.
#
# Implementacion en esta seccion:
# - Crear d_ln_* = ln_* - lag(ln_*).
# - Crear l1_ln_* y l1_d_ln_*.
# - Definir y construir PIBSOCIOS para el modelo de exportaciones.
# - Exportar una base transformada para trazabilidad.

# Ponderadores BCRA para construir una proxy de PIBSOCIOS con Brasil y EEUU.
# Como por ahora contamos con PIB real de Brasil y Estados Unidos, se normalizan
# sus ponderadores para que ambos sumen 1 dentro de esta proxy.
pib_socios_weights <- read_excel(
  itcrm_file,
  sheet = "Ponderadores",
  skip = 1
) %>%
  clean_names() %>%
  transmute(
    date = as.Date(periodo),
    weight_brazil = as.numeric(brasil),
    weight_usa = as.numeric(estados_unidos)
  ) %>%
  filter(
    date >= as.Date("2004-01-01"),
    date <= as.Date("2025-12-01"),
    !is.na(weight_brazil),
    !is.na(weight_usa)
  ) %>%
  summarise(
    weight_brazil = mean(weight_brazil, na.rm = TRUE),
    weight_usa = mean(weight_usa, na.rm = TRUE)
  ) %>%
  mutate(
    weight_sum = weight_brazil + weight_usa,
    weight_brazil_norm = weight_brazil / weight_sum,
    weight_usa_norm = weight_usa / weight_sum
  )

weight_brazil_norm <- pib_socios_weights$weight_brazil_norm
weight_usa_norm <- pib_socios_weights$weight_usa_norm

# Base transformada para analisis econometrico.
trade_elasticities_panel_transformed <- trade_elasticities_panel %>%
  arrange(stata_qdate) %>%
  mutate(
    # Indices base 2004Q1 = 100 para combinar PIBs con unidades distintas.
    usa_gdp_real_index = usa_gdp_real / first(usa_gdp_real) * 100,
    brazil_gdp_real_index = brazil_gdp_real / first(brazil_gdp_real) * 100,
    # PIBSOCIOS: promedio geometrico ponderado de los indices de Brasil y EEUU.
    ln_pib_socios = (
      weight_brazil_norm * log(brazil_gdp_real_index) +
        weight_usa_norm * log(usa_gdp_real_index)
    ),
    pib_socios_index = exp(ln_pib_socios)
  ) %>%
  mutate(
    # Primeras diferencias de logs: elasticidades/tasas de corto plazo.
    d_ln_imports_real = ln_imports_real - lag(ln_imports_real),
    d_ln_exports_real = ln_exports_real - lag(ln_exports_real),
    d_ln_gdp_real = ln_gdp_real - lag(ln_gdp_real),
    d_ln_itcrm = ln_itcrm - lag(ln_itcrm),
    d_ln_usa_gdp_real = ln_usa_gdp_real - lag(ln_usa_gdp_real),
    d_ln_brazil_gdp_real = ln_brazil_gdp_real - lag(ln_brazil_gdp_real),
    d_ln_commodity_price_index = (
      ln_commodity_price_index - lag(ln_commodity_price_index)
    ),
    d_ln_pib_socios = ln_pib_socios - lag(ln_pib_socios),
    # Tasas trimestrales aproximadas en porcentaje.
    g_imports_real = 100 * d_ln_imports_real,
    g_exports_real = 100 * d_ln_exports_real,
    g_gdp_real = 100 * d_ln_gdp_real,
    g_itcrm = 100 * d_ln_itcrm,
    g_usa_gdp_real = 100 * d_ln_usa_gdp_real,
    g_brazil_gdp_real = 100 * d_ln_brazil_gdp_real,
    g_commodity_price_index = 100 * d_ln_commodity_price_index,
    g_pib_socios = 100 * d_ln_pib_socios
  ) %>%
  mutate(
    # Rezagos de niveles en logs.
    l1_ln_imports_real = lag(ln_imports_real),
    l1_ln_exports_real = lag(ln_exports_real),
    l1_ln_gdp_real = lag(ln_gdp_real),
    l1_ln_itcrm = lag(ln_itcrm),
    l1_ln_usa_gdp_real = lag(ln_usa_gdp_real),
    l1_ln_brazil_gdp_real = lag(ln_brazil_gdp_real),
    l1_ln_commodity_price_index = lag(ln_commodity_price_index),
    l1_ln_pib_socios = lag(ln_pib_socios),
    # Rezagos de primeras diferencias.
    l1_d_ln_imports_real = lag(d_ln_imports_real),
    l1_d_ln_exports_real = lag(d_ln_exports_real),
    l1_d_ln_gdp_real = lag(d_ln_gdp_real),
    l1_d_ln_itcrm = lag(d_ln_itcrm),
    l1_d_ln_usa_gdp_real = lag(d_ln_usa_gdp_real),
    l1_d_ln_brazil_gdp_real = lag(d_ln_brazil_gdp_real),
    l1_d_ln_commodity_price_index = lag(d_ln_commodity_price_index),
    l1_d_ln_pib_socios = lag(d_ln_pib_socios)
  )

# Reporte de faltantes luego de las transformaciones.
transformed_missing <- trade_elasticities_panel_transformed %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  )

print(transformed_missing)

# Base transformada con casos completos.
trade_elasticities_panel_transformed_complete <- trade_elasticities_panel_transformed %>%
  filter(if_all(everything(), ~ !is.na(.x)))

# Exportar base transformada.
write_dta(
  trade_elasticities_panel_transformed,
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_2004_2025.dta")
)

write_csv(
  trade_elasticities_panel_transformed,
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_2004_2025.csv")
)

# Exportar base transformada con casos completos.
write_dta(
  trade_elasticities_panel_transformed_complete,
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_complete_cases.dta")
)

write_csv(
  trade_elasticities_panel_transformed_complete,
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_complete_cases.csv")
)

# Reabrir .dta exportado.
trade_elasticities_panel_transformed_dta <- read_dta(
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_2004_2025.dta")
)

# Abrir en visor de RStudio.
View(trade_elasticities_panel_transformed_dta)


# =========================================================
# 8. VISUALIZACIÓN EXPLORATORIA
# =========================================================

# Objetivos de esta seccion:
# - Graficar las series en niveles y en logaritmos.
# - Revisar tendencias, estacionalidad visual y cambios de regimen.
# - Comparar importaciones, exportaciones, PIB argentino, ITCRM y PIBSOCIOS.
# - Generar figuras limpias para el informe final.
#
# Graficos sugeridos:
# - Logs de importaciones, PIB argentino e ITCRM.
# - Logs de exportaciones, PIBSOCIOS e ITCRM.
# - Diferencias de logs para observar crecimiento trimestral.
# - ITCRM y commodity price index como variables externas.
#
# Pendiente:
# - Guardar las figuras en figures_dir.
# - Usar nombres de archivo consistentes y citables en el informe.

# Crear carpetas de salida si no existen.
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Tema comun para figuras exploratorias.
exploratory_theme <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Funcion auxiliar para guardar graficos.
save_exploratory_plot <- function(plot, filename, width = 9, height = 6) {
  ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# Logs para la ecuacion de importaciones.
plot_imports_logs <- trade_elasticities_panel_transformed %>%
  select(
    quarter_date,
    ln_imports_real,
    ln_gdp_real,
    ln_itcrm
  ) %>%
  pivot_longer(
    cols = -quarter_date,
    names_to = "variable",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    variable = recode(
      variable,
      ln_imports_real = "Importaciones reales",
      ln_gdp_real = "PIB Argentina",
      ln_itcrm = "ITCRM"
    )
  ) %>%
  ggplot(aes(x = quarter_date, y = value)) +
  geom_line(color = "#2f5597", linewidth = 0.7) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Ecuacion de importaciones: series en logaritmos",
    x = NULL,
    y = "Log natural"
  ) +
  exploratory_theme

save_exploratory_plot(
  plot_imports_logs,
  "section_08_imports_logs.png"
)

# Logs para la ecuacion de exportaciones.
plot_exports_logs <- trade_elasticities_panel_transformed %>%
  select(
    quarter_date,
    ln_exports_real,
    ln_pib_socios,
    ln_itcrm,
    ln_commodity_price_index
  ) %>%
  pivot_longer(
    cols = -quarter_date,
    names_to = "variable",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    variable = recode(
      variable,
      ln_exports_real = "Exportaciones reales",
      ln_pib_socios = "PIB socios",
      ln_itcrm = "ITCRM",
      ln_commodity_price_index = "Commodity price index"
    )
  ) %>%
  ggplot(aes(x = quarter_date, y = value)) +
  geom_line(color = "#548235", linewidth = 0.7) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Ecuacion de exportaciones: series en logaritmos",
    x = NULL,
    y = "Log natural"
  ) +
  exploratory_theme

save_exploratory_plot(
  plot_exports_logs,
  "section_08_exports_logs.png"
)

# Crecimiento trimestral de comercio y actividad local.
plot_trade_growth <- trade_elasticities_panel_transformed %>%
  select(
    quarter_date,
    g_imports_real,
    g_exports_real,
    g_gdp_real
  ) %>%
  pivot_longer(
    cols = -quarter_date,
    names_to = "variable",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    variable = recode(
      variable,
      g_imports_real = "Importaciones reales",
      g_exports_real = "Exportaciones reales",
      g_gdp_real = "PIB Argentina"
    )
  ) %>%
  ggplot(aes(x = quarter_date, y = value, color = variable)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Crecimiento trimestral aproximado",
    x = NULL,
    y = "Variacion trimestral aproximada (%)",
    color = NULL
  ) +
  exploratory_theme

save_exploratory_plot(
  plot_trade_growth,
  "section_08_trade_growth.png"
)

# Crecimiento trimestral de variables externas.
plot_external_growth <- trade_elasticities_panel_transformed %>%
  select(
    quarter_date,
    g_itcrm,
    g_pib_socios,
    g_commodity_price_index
  ) %>%
  pivot_longer(
    cols = -quarter_date,
    names_to = "variable",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    variable = recode(
      variable,
      g_itcrm = "ITCRM",
      g_pib_socios = "PIB socios",
      g_commodity_price_index = "Commodity price index"
    )
  ) %>%
  ggplot(aes(x = quarter_date, y = value, color = variable)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Crecimiento trimestral aproximado de variables externas",
    x = NULL,
    y = "Variacion trimestral aproximada (%)",
    color = NULL
  ) +
  exploratory_theme

save_exploratory_plot(
  plot_external_growth,
  "section_08_external_growth.png"
)

# Indice de disponibilidad temporal por serie principal.
plot_data_coverage <- trade_elasticities_panel_transformed %>%
  transmute(
    quarter_date,
    `Argentina: PIB/comercio` = !is.na(gdp_real),
    ITCRM = !is.na(itcrm),
    `PIB EEUU` = !is.na(usa_gdp_real),
    `PIB Brasil` = !is.na(brazil_gdp_real),
    Commodities = !is.na(commodity_price_index)
  ) %>%
  pivot_longer(
    cols = -quarter_date,
    names_to = "variable",
    values_to = "available"
  ) %>%
  ggplot(aes(x = quarter_date, y = variable, fill = available)) +
  geom_tile(height = 0.75) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_fill_manual(
    values = c(`TRUE` = "#548235", `FALSE` = "#c00000"),
    labels = c(`TRUE` = "Disponible", `FALSE` = "Faltante")
  ) +
  labs(
    title = "Cobertura temporal de las series principales",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  exploratory_theme

save_exploratory_plot(
  plot_data_coverage,
  "section_08_data_coverage.png",
  width = 9,
  height = 4.8
)

# Indice de figuras generadas.
exploratory_figures_index <- tibble(
  figure = c(
    "section_08_imports_logs.png",
    "section_08_exports_logs.png",
    "section_08_trade_growth.png",
    "section_08_external_growth.png",
    "section_08_data_coverage.png"
  ),
  description = c(
    "Logs de importaciones, PIB argentino e ITCRM",
    "Logs de exportaciones, PIB socios, ITCRM y commodities",
    "Crecimiento trimestral de comercio y PIB argentino",
    "Crecimiento trimestral de ITCRM, PIB socios y commodities",
    "Cobertura temporal de las series principales"
  ),
  path = file.path(figures_dir, figure)
)

write_csv(
  exploratory_figures_index,
  file.path(output_dir, "section_08_exploratory_figures_index.csv")
)

print(exploratory_figures_index)


# =========================================================
# 9. ANÁLISIS DE ESTACIONARIEDAD
# =========================================================

# Objetivos de esta seccion:
# - Determinar si las variables son I(1), como anticipa la consigna.
# - Aplicar pruebas sobre niveles en logaritmos y primeras diferencias.
# - Documentar si cada serie es estacionaria o requiere diferenciacion.
#
# Pruebas sugeridas:
# - ADF para raiz unitaria.
# - Phillips-Perron como contraste adicional.
# - KPSS como prueba complementaria de estacionariedad.
#
# Series a evaluar:
# - ln_imports_real, ln_exports_real, ln_gdp_real, ln_itcrm.
# - ln_pib_socios, cuando este construido.
# - ln_commodity_price_index si se mantiene como variable adicional.
#
# Implementacion en esta seccion:
# - Crear tablas resumen con estadistico, p-value/conclusion y decision I(0)/I(1).
# - Repetir pruebas en primeras diferencias.
# - Exportar resultados en Excel para el informe/anexo.

# Para las pruebas se usa la muestra comun de modelacion, limitada por las
# series argentinas de PIB, importaciones y exportaciones.
stationarity_sample_end <- trade_elasticities_panel_transformed %>%
  filter(
    !is.na(ln_gdp_real),
    !is.na(ln_imports_real),
    !is.na(ln_exports_real)
  ) %>%
  summarise(last_quarter_date = max(quarter_date)) %>%
  pull(last_quarter_date)

stationarity_data <- trade_elasticities_panel_transformed %>%
  filter(quarter_date <= stationarity_sample_end) %>%
  arrange(stata_qdate)

# Series centrales para la hoja de ruta econometrica del TP.
stationarity_specs <- tribble(
  ~series_key, ~series_label, ~level_var, ~diff_var,
  "imports", "Importaciones reales", "ln_imports_real", "d_ln_imports_real",
  "exports", "Exportaciones reales", "ln_exports_real", "d_ln_exports_real",
  "gdp_arg", "PIB real Argentina", "ln_gdp_real", "d_ln_gdp_real",
  "itcrm", "Tipo de cambio real multilateral", "ln_itcrm", "d_ln_itcrm",
  "pib_socios", "PIB socios comerciales", "ln_pib_socios", "d_ln_pib_socios",
  "commodities", "Commodity price index", "ln_commodity_price_index",
  "d_ln_commodity_price_index"
)

# Tabla de trazabilidad de muestra por serie y transformacion.
stationarity_sample_summary <- stationarity_specs %>%
  pivot_longer(
    cols = c(level_var, diff_var),
    names_to = "transformation",
    values_to = "variable"
  ) %>%
  mutate(
    transformation = recode(
      transformation,
      level_var = "log_level",
      diff_var = "log_difference"
    )
  ) %>%
  rowwise() %>%
  mutate(
    n_obs = sum(!is.na(stationarity_data[[variable]])),
    first_quarter = stationarity_data$quarter[
      which(!is.na(stationarity_data[[variable]]))[1]
    ],
    last_quarter = stationarity_data$quarter[
      tail(which(!is.na(stationarity_data[[variable]])), 1)
    ]
  ) %>%
  ungroup()

print(stationarity_sample_summary)

# Funcion auxiliar para capturar resultados de tests sin detener el script.
safe_stationarity_test <- function(test_call) {
  tryCatch(
    suppressWarnings(test_call),
    error = function(e) e
  )
}

# Funcion auxiliar para estandarizar ADF, Phillips-Perron y KPSS.
run_stationarity_tests <- function(data, variable, transformation, series_key,
                                   series_label) {
  x <- data[[variable]]
  x <- as.numeric(x[!is.na(x)])
  n_obs <- length(x)

  if (n_obs < 12) {
    return(tibble(
      series_key = series_key,
      series_label = series_label,
      variable = variable,
      transformation = transformation,
      test = c("ADF", "Phillips-Perron", "KPSS"),
      statistic = NA_real_,
      p_value = NA_real_,
      lag_order = NA_integer_,
      null_hypothesis = c(
        "raiz unitaria",
        "raiz unitaria",
        "estacionariedad"
      ),
      conclusion_5pct = "muestra insuficiente"
    ))
  }

  adf_lag <- trunc((n_obs - 1)^(1 / 3))
  kpss_null <- if_else(transformation == "log_level", "Trend", "Level")

  adf_result <- safe_stationarity_test(
    adf.test(x, alternative = "stationary", k = adf_lag)
  )
  pp_result <- safe_stationarity_test(
    pp.test(x, alternative = "stationary")
  )
  kpss_result <- safe_stationarity_test(
    kpss.test(x, null = kpss_null)
  )

  tibble(
    series_key = series_key,
    series_label = series_label,
    variable = variable,
    transformation = transformation,
    test = c("ADF", "Phillips-Perron", "KPSS"),
    statistic = c(
      if (inherits(adf_result, "error")) NA_real_ else unname(adf_result$statistic),
      if (inherits(pp_result, "error")) NA_real_ else unname(pp_result$statistic),
      if (inherits(kpss_result, "error")) NA_real_ else unname(kpss_result$statistic)
    ),
    p_value = c(
      if (inherits(adf_result, "error")) NA_real_ else adf_result$p.value,
      if (inherits(pp_result, "error")) NA_real_ else pp_result$p.value,
      if (inherits(kpss_result, "error")) NA_real_ else kpss_result$p.value
    ),
    lag_order = c(adf_lag, NA_integer_, NA_integer_),
    null_hypothesis = c(
      "raiz unitaria",
      "raiz unitaria",
      paste0("estacionariedad ", tolower(kpss_null))
    )
  ) %>%
    mutate(
      conclusion_5pct = case_when(
        is.na(p_value) ~ "test no disponible",
        test %in% c("ADF", "Phillips-Perron") & p_value < 0.05 ~
          "rechaza raiz unitaria",
        test %in% c("ADF", "Phillips-Perron") & p_value >= 0.05 ~
          "no rechaza raiz unitaria",
        test == "KPSS" & p_value < 0.05 ~ "rechaza estacionariedad",
        test == "KPSS" & p_value >= 0.05 ~ "no rechaza estacionariedad",
        TRUE ~ "sin decision"
      )
    )
}

# Ejecutar pruebas en niveles logaritmicos y primeras diferencias.
stationarity_tests <- stationarity_specs %>%
  pivot_longer(
    cols = c(level_var, diff_var),
    names_to = "transformation",
    values_to = "variable"
  ) %>%
  mutate(
    transformation = recode(
      transformation,
      level_var = "log_level",
      diff_var = "log_difference"
    )
  ) %>%
  pmap_dfr(
    function(series_key, series_label, transformation, variable) {
      run_stationarity_tests(
        data = stationarity_data,
        variable = variable,
        transformation = transformation,
        series_key = series_key,
        series_label = series_label
      )
    }
  )

print(stationarity_tests)

# Decision mecanica preliminar por transformacion.
stationarity_decisions_by_transform <- stationarity_tests %>%
  group_by(series_key, series_label, variable, transformation) %>%
  summarise(
    adf_rejects_unit_root = any(
      test == "ADF" & !is.na(p_value) & p_value < 0.05
    ),
    pp_rejects_unit_root = any(
      test == "Phillips-Perron" & !is.na(p_value) & p_value < 0.05
    ),
    kpss_rejects_stationarity = any(
      test == "KPSS" & !is.na(p_value) & p_value < 0.05
    ),
    evidence_stationary = (
      (adf_rejects_unit_root | pp_rejects_unit_root) &
        !kpss_rejects_stationarity
    ),
    .groups = "drop"
  ) %>%
  mutate(
    stationarity_decision = if_else(
      evidence_stationary,
      "evidencia compatible con I(0)",
      "sin evidencia suficiente de I(0)"
    )
  )

print(stationarity_decisions_by_transform)

# Decision preliminar de orden de integracion.
stationarity_order_summary <- stationarity_decisions_by_transform %>%
  select(series_key, series_label, transformation, evidence_stationary) %>%
  pivot_wider(
    names_from = transformation,
    values_from = evidence_stationary,
    names_prefix = "stationary_"
  ) %>%
  mutate(
    preliminary_order = case_when(
      stationary_log_level ~ "I(0) en niveles",
      !stationary_log_level & stationary_log_difference ~ "I(1)",
      !stationary_log_level & !stationary_log_difference ~
        "no concluyente; revisar especificacion",
      TRUE ~ "no concluyente"
    ),
    implication = case_when(
      preliminary_order == "I(1)" ~
        "candidata para pruebas de cointegracion en niveles",
      preliminary_order == "I(0) en niveles" ~
        "no requiere diferenciacion para estacionariedad",
      TRUE ~
        "revisar rezagos, quiebres o transformaciones antes de modelar"
    )
  )

print(stationarity_order_summary)

# Exportar resultados en Excel para lectura rapida en informe/anexo.
write_xlsx(
  list(
    sample = stationarity_sample_summary,
    tests = stationarity_tests,
    decisions = stationarity_decisions_by_transform,
    order_summary = stationarity_order_summary
  ),
  file.path(output_dir, "section_09_stationarity_results.xlsx")
)


# =========================================================
# 10. ANÁLISIS DE COINTEGRACIÓN
# =========================================================

# Objetivos de esta seccion:
# - Evaluar si existen relaciones de largo plazo entre las variables en logs.
# - Seguir la consigna: Engle-Granger y Gregory-Hansen con quiebre estructural.
# - Separar el analisis de importaciones y exportaciones.
#
# Ecuaciones base:
# - Importaciones: ln_imports_real ~ ln_gdp_real + ln_itcrm.
# - Exportaciones: ln_exports_real ~ ln_pib_socios + ln_itcrm.
#
# Extension posible:
# - Agregar ln_commodity_price_index si aporta interpretacion economica.
#
# Implementacion en esta seccion:
# - Estimar ecuaciones de cointegracion en niveles.
# - Guardar residuos de cada ecuacion.
# - Testear estacionariedad de residuos para Engle-Granger.
# - Aplicar Gregory-Hansen para permitir quiebre estructural.
# - Decidir si corresponde ECM o modelo solo en diferencias.

# Muestra comun para cointegracion: niveles en logs completos.
cointegration_data <- trade_elasticities_panel_transformed %>%
  filter(
    !is.na(ln_imports_real),
    !is.na(ln_exports_real),
    !is.na(ln_gdp_real),
    !is.na(ln_itcrm),
    !is.na(ln_pib_socios)
  ) %>%
  arrange(stata_qdate)

# Especificaciones base segun la consigna.
cointegration_specs <- tibble(
  model_key = c("imports", "exports"),
  model_label = c(
    "Importaciones: comercio, PIB argentino e ITCRM",
    "Exportaciones: comercio, PIB socios e ITCRM"
  ),
  dependent_var = c("ln_imports_real", "ln_exports_real"),
  regressors = list(
    c("ln_gdp_real", "ln_itcrm"),
    c("ln_pib_socios", "ln_itcrm")
  )
)

# Funcion auxiliar: tabla ordenada de coeficientes de lm().
tidy_lm_coefficients <- function(fit, model_key, model_label) {
  coef_table <- as.data.frame(summary(fit)$coefficients)
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL

  coef_table %>%
    as_tibble() %>%
    transmute(
      model_key = model_key,
      model_label = model_label,
      term = term,
      estimate = Estimate,
      std_error = `Std. Error`,
      t_statistic = `t value`,
      p_value = `Pr(>|t|)`
    )
}

# Funcion auxiliar: estadistico ADF de residuos sin constante.
residual_adf_t_stat <- function(residuals, lag_order) {
  residuals <- as.numeric(residuals)
  dy <- diff(residuals)
  y_lag <- residuals[-length(residuals)]

  adf_data <- tibble(
    dy = dy,
    y_lag = y_lag
  )

  if (lag_order > 0) {
    for (lag_i in seq_len(lag_order)) {
      adf_data[[paste0("dy_lag", lag_i)]] <- lag(dy, lag_i)
    }
  }

  adf_data <- adf_data %>%
    filter(if_all(everything(), ~ !is.na(.x)))

  if (lag_order > 0) {
    rhs <- paste(c("y_lag", paste0("dy_lag", seq_len(lag_order))), collapse = " + ")
  } else {
    rhs <- "y_lag"
  }

  adf_fit <- lm(as.formula(paste0("dy ~ ", rhs, " - 1")), data = adf_data)
  unname(summary(adf_fit)$coefficients["y_lag", "t value"])
}

# Funcion auxiliar: Engle-Granger en dos etapas.
estimate_engle_granger <- function(data, model_key, model_label, dependent_var,
                                   regressors) {
  model_vars <- c(dependent_var, regressors)
  model_data <- data %>%
    select(quarter, quarter_date, stata_qdate, all_of(model_vars)) %>%
    filter(if_all(all_of(model_vars), ~ !is.na(.x)))

  fit <- lm(reformulate(regressors, response = dependent_var), data = model_data)
  residuals <- resid(fit)
  n_obs <- length(residuals)
  adf_lag <- trunc((n_obs - 1)^(1 / 3))

  adf_result <- safe_stationarity_test(
    adf.test(residuals, alternative = "stationary", k = adf_lag)
  )

  list(
    fit = fit,
    equation_summary = tibble(
      model_key = model_key,
      model_label = model_label,
      method = "Engle-Granger first-stage OLS",
      dependent_var = dependent_var,
      regressors = paste(regressors, collapse = " + "),
      n_obs = n_obs,
      first_quarter = first(model_data$quarter),
      last_quarter = last(model_data$quarter),
      r_squared = summary(fit)$r.squared,
      adj_r_squared = summary(fit)$adj.r.squared
    ),
    coefficients = tidy_lm_coefficients(fit, model_key, model_label),
    residuals = model_data %>%
      transmute(
        quarter,
        quarter_date,
        stata_qdate,
        model_key = model_key,
        residual = residuals
      ),
    engle_granger_test = tibble(
      model_key = model_key,
      model_label = model_label,
      method = "Engle-Granger residual ADF",
      dependent_var = dependent_var,
      regressors = paste(regressors, collapse = " + "),
      n_obs = n_obs,
      adf_lag = adf_lag,
      statistic = if (inherits(adf_result, "error")) {
        NA_real_
      } else {
        unname(adf_result$statistic)
      },
      p_value = if (inherits(adf_result, "error")) {
        NA_real_
      } else {
        adf_result$p.value
      },
      conclusion_5pct = case_when(
        inherits(adf_result, "error") ~ "test no disponible",
        adf_result$p.value < 0.05 ~ "residuos estacionarios; cointegracion",
        TRUE ~ "no rechaza raiz unitaria en residuos"
      ),
      note = paste(
        "ADF aplicado sobre residuos de la primera etapa;",
        "la decision debe interpretarse como evidencia preliminar."
      )
    )
  )
}

# Ejecutar Engle-Granger.
engle_granger_results <- pmap(
  cointegration_specs,
  function(model_key, model_label, dependent_var, regressors) {
    estimate_engle_granger(
      data = cointegration_data,
      model_key = model_key,
      model_label = model_label,
      dependent_var = dependent_var,
      regressors = regressors
    )
  }
)

cointegration_equation_summary <- map_dfr(
  engle_granger_results,
  "equation_summary"
)

cointegration_long_run_coefficients <- map_dfr(
  engle_granger_results,
  "coefficients"
)

engle_granger_tests <- map_dfr(
  engle_granger_results,
  "engle_granger_test"
)

engle_granger_residuals_long <- map_dfr(
  engle_granger_results,
  "residuals"
)

engle_granger_residuals_wide <- engle_granger_residuals_long %>%
  mutate(
    residual_name = paste0("resid_", model_key, "_eg")
  ) %>%
  select(quarter_date, residual_name, residual) %>%
  pivot_wider(
    names_from = residual_name,
    values_from = residual
  ) %>%
  arrange(quarter_date)

# Funcion auxiliar: Gregory-Hansen residual con busqueda de quiebre.
# Se reportan tres especificaciones: cambio de intercepto, cambio de intercepto
# con tendencia, y cambio de regimen (intercepto + pendientes).
run_gregory_hansen_search <- function(data, model_key, model_label, dependent_var,
                                      regressors, gh_model) {
  model_vars <- c(dependent_var, regressors)
  model_data <- data %>%
    select(quarter, quarter_date, stata_qdate, all_of(model_vars)) %>%
    filter(if_all(all_of(model_vars), ~ !is.na(.x))) %>%
    arrange(stata_qdate) %>%
    mutate(trend = row_number())

  n_obs <- nrow(model_data)
  adf_lag <- trunc((n_obs - 1)^(1 / 3))
  break_candidates <- seq(
    from = ceiling(0.15 * n_obs),
    to = floor(0.85 * n_obs)
  )

  gh_results <- map_dfr(
    break_candidates,
    function(break_index) {
      break_data <- model_data %>%
        mutate(
          break_dummy = as.integer(row_number() > break_index)
        )

      gh_terms <- case_when(
        gh_model == "level_shift" ~
          paste(c(regressors, "break_dummy"), collapse = " + "),
        gh_model == "level_shift_trend" ~
          paste(c(regressors, "trend", "break_dummy"), collapse = " + "),
        gh_model == "regime_shift" ~
          paste(
            c(
              regressors,
              "break_dummy",
              paste0(regressors, ":break_dummy")
            ),
            collapse = " + "
          ),
        TRUE ~ paste(c(regressors, "break_dummy"), collapse = " + ")
      )

      gh_fit <- lm(
        as.formula(paste(dependent_var, "~", gh_terms)),
        data = break_data
      )

      tibble(
        model_key = model_key,
        model_label = model_label,
        gh_model = gh_model,
        break_index = break_index,
        break_quarter = break_data$quarter[break_index],
        break_date = break_data$quarter_date[break_index],
        n_obs = n_obs,
        adf_lag = adf_lag,
        statistic = residual_adf_t_stat(resid(gh_fit), adf_lag)
      )
    }
  )

  gh_results %>%
    slice_min(statistic, n = 1, with_ties = FALSE) %>%
    mutate(
      critical_value_5pct_approx = case_when(
        gh_model == "level_shift" ~ -4.95,
        gh_model == "level_shift_trend" ~ -5.29,
        gh_model == "regime_shift" ~ -5.50,
        TRUE ~ NA_real_
      ),
      conclusion_5pct = if_else(
        statistic < critical_value_5pct_approx,
        "rechaza no cointegracion con quiebre",
        "no rechaza no cointegracion con quiebre"
      ),
      note = paste(
        "Valores criticos aproximados para Gregory-Hansen con dos regresores;",
        "confirmar contra tabla del curso si se reporta como resultado final."
      )
    )
}

# Ejecutar Gregory-Hansen para cada ecuacion y tipo de quiebre.
gregory_hansen_tests <- pmap_dfr(
  cointegration_specs,
  function(model_key, model_label, dependent_var, regressors) {
    map_dfr(
      c("level_shift", "level_shift_trend", "regime_shift"),
      function(gh_model) {
        run_gregory_hansen_search(
          data = cointegration_data,
          model_key = model_key,
          model_label = model_label,
          dependent_var = dependent_var,
          regressors = regressors,
          gh_model = gh_model
        )
      }
    )
  }
)

# Decision preliminar para orientar la siguiente seccion.
cointegration_decision_summary <- engle_granger_tests %>%
  transmute(
    model_key,
    model_label,
    engle_granger_cointegration = p_value < 0.05,
    engle_granger_conclusion = conclusion_5pct
  ) %>%
  left_join(
    gregory_hansen_tests %>%
      group_by(model_key, model_label) %>%
      summarise(
        gregory_hansen_cointegration = any(
          conclusion_5pct == "rechaza no cointegracion con quiebre"
        ),
        best_gh_model = gh_model[which.min(statistic)],
        best_gh_break_quarter = break_quarter[which.min(statistic)],
        best_gh_statistic = min(statistic),
        .groups = "drop"
      ),
    by = c("model_key", "model_label")
  ) %>%
  mutate(
    cointegration_evidence = case_when(
      engle_granger_cointegration & gregory_hansen_cointegration ~
        "evidencia por Engle-Granger y Gregory-Hansen",
      engle_granger_cointegration & !gregory_hansen_cointegration ~
        "evidencia por Engle-Granger",
      !engle_granger_cointegration & gregory_hansen_cointegration ~
        "evidencia por Gregory-Hansen",
      TRUE ~ "sin evidencia de cointegracion"
    ),
    next_step = if_else(
      engle_granger_cointegration | gregory_hansen_cointegration,
      "estimar ECM con termino de correccion del error",
      "estimar modelo en primeras diferencias sin ECM"
    )
  )

print(cointegration_equation_summary)
print(cointegration_long_run_coefficients)
print(engle_granger_tests)
print(gregory_hansen_tests)
print(cointegration_decision_summary)

# Agregar residuos de cointegracion al panel para la seccion de modelos.
trade_elasticities_panel_modeling <- trade_elasticities_panel_transformed %>%
  left_join(engle_granger_residuals_wide, by = "quarter_date") %>%
  arrange(stata_qdate) %>%
  mutate(
    l1_resid_imports_eg = lag(resid_imports_eg),
    l1_resid_exports_eg = lag(resid_exports_eg)
  )

# Exportar base de modelacion con residuos de cointegracion.
write_dta(
  trade_elasticities_panel_modeling,
  file.path(processed_data_dir, "trade_elasticities_panel_modeling_2004_2025.dta")
)

# Reabrir .dta exportado.
trade_elasticities_panel_modeling_dta <- read_dta(
  file.path(processed_data_dir, "trade_elasticities_panel_modeling_2004_2025.dta")
)

# Abrir en visor de RStudio.
View(trade_elasticities_panel_modeling_dta)

# Exportar resultados de cointegracion en tablas separadas.
write_xlsx(
  list(
    equations = cointegration_equation_summary,
    long_run_coefficients = cointegration_long_run_coefficients,
    engle_granger = engle_granger_tests,
    gregory_hansen = gregory_hansen_tests,
    decision_summary = cointegration_decision_summary,
    residuals = engle_granger_residuals_long
  ),
  file.path(output_dir, "section_10_cointegration_results.xlsx")
)


# =========================================================
# 11. MODELOS ECONOMÉTRICOS
# =========================================================

# Objetivos de esta seccion:
# - Estimar elasticidades de largo plazo si hay cointegracion.
# - Estimar elasticidades de corto plazo con variables diferenciadas.
# - Construir ECM cuando la relacion de cointegracion sea valida.
#
# Modelos esperados:
# - Largo plazo importaciones: logs en niveles.
# - Largo plazo exportaciones: logs en niveles.
# - Corto plazo importaciones: diferencias de logs y rezagos.
# - Corto plazo exportaciones: diferencias de logs y rezagos.
#
# Si hay cointegracion:
# - Incluir residuo de cointegracion rezagado como termino de correccion del error.
# - Corregir autocorrelacion agregando rezagos de diferencias.
# - Evaluar significatividad y eliminar coeficientes no significativos con criterio.
#
# Si no hay cointegracion:
# - Omitir el termino de correccion del error.
# - Estimar modelos en diferencias para elasticidades de corto plazo.
#
# Pendiente:
# - Aplicar correccion Wickens-Breusch si se implementa la estimacion conjunta.
# - Guardar tablas comparables para el informe.


# =========================================================
# 12. DIAGNÓSTICOS DEL MODELO
# =========================================================

# Objetivos de esta seccion:
# - Verificar que los modelos estimados tengan residuos razonables.
# - Detectar autocorrelacion, heterocedasticidad y problemas de estabilidad.
# - Ajustar rezagos si los diagnosticos lo justifican.
#
# Diagnosticos sugeridos:
# - Breusch-Godfrey o Ljung-Box para autocorrelacion.
# - Breusch-Pagan o errores robustos para heterocedasticidad.
# - Jarque-Bera o inspeccion grafica para normalidad.
# - Tests/plots de estabilidad y quiebres estructurales.
#
# Pendiente:
# - Guardar diagnosticos por modelo.
# - Reportar solo resultados relevantes en el cuerpo del informe.
# - Enviar salidas extensas al anexo.


# =========================================================
# 13. EXPORTACIÓN DE RESULTADOS
# =========================================================

# Objetivos de esta seccion:
# - Exportar bases finales, tablas y graficos necesarios para el informe.
# - Separar resultados principales de anexos econometricos.
# - Asegurar trazabilidad: el script debe correr desde los datos originales.
#
# Archivos esperados:
# - Panel final y panel transformado en .dta y .csv.
# - Tablas de estacionariedad.
# - Tablas de cointegracion.
# - Tablas de modelos de largo y corto plazo.
# - Figuras exploratorias.
#
# Pendiente:
# - Definir formato de tablas para Word/PDF.
# - Guardar outputs en output_dir y figures_dir con nombres consistentes.


# =========================================================
# 14. FIN DEL SCRIPT
# =========================================================

# Checklist final antes de entregar:
# - El script corre completo desde las bases crudas.
# - No hay rutas temporales ni dependencias manuales.
# - El panel final tiene una llave trimestral unica.
# - Las variables usadas en los modelos coinciden con la consigna.
# - Word, PDF y script R se pueden comprimir para entrega en Campus.
