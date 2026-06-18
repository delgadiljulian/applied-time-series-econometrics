#********************************************************
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
# - Andrea Chasi
# - Julián Delgadillo Marín
# - Christian Arias
# - Leonardo Ávila
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
#********************************************************
# 1. INICIALIZACIÓN DEL ENTORNO
#********************************************************
# reiniciar la sesion de trabajo para evitar que objetos
# viejos contaminen los resultados reproducibles del script.

# Limpiar objetos previos.
rm(list = ls())

# Cerrar graficos abiertos.
graphics.off()

# Evitar notacion cientifica.
options(scipen = 999)

#********************************************************
# 2. CARGA DE PAQUETES
#********************************************************
# cargar librerias para limpieza, series de tiempo,
# econometria, visualizacion y exportacion de resultados.

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

#********************************************************
# 3. DIRECTORIOS DE TRABAJO
#********************************************************
# definir rutas del proyecto y crear carpetas necesarias
# para que las salidas se escriban siempre en ubicaciones esperadas.

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

# Crear carpetas esperadas para que el script pueda correr desde cero.
walk(
  c(
    raw_data_dir,
    processed_data_dir,
    output_dir,
    figures_dir,
    scripts_dir,
    report_dir,
    docs_dir
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

#********************************************************
# 3.1 CONFIGURACIÓN GLOBAL DEL MODELO
#********************************************************
# concentrar los parametros econometricos principales en un solo bloque para
# que las decisiones de muestra, rezagos, inferencia y parsimonia sean visibles.
analysis_start_date <- as.Date("2004-01-01")
analysis_end_date <- as.Date("2025-10-01")

# se construye model_sample_start con las instrucciones de este minibloque.
model_sample_start <- analysis_start_date
model_sample_end_preferred <- analysis_end_date

# se construye significance_level con las instrucciones de este minibloque.
significance_level <- 0.05
information_criterion <- "BIC"
max_lags <- 4

# Se usa lag 4 en Newey-West porque la frecuencia es trimestral.
newey_west_lag <- 4

# se construye parsimonious_p_threshold con las instrucciones de este minibloque.
parsimonious_p_threshold <- 0.10
gregory_hansen_trim <- 0.15

# se arma la tabla global_model_config con informacion de este paso.
global_model_config <- tibble(
  parameter = c(
    "analysis_start_date",
    "analysis_end_date",
    "model_sample_start",
    "model_sample_end_preferred",
    "significance_level",
    "information_criterion",
    "max_lags",
    "newey_west_lag",
    "parsimonious_p_threshold",
    "gregory_hansen_trim"
  ),
  value = c(
    as.character(analysis_start_date),
    as.character(analysis_end_date),
    as.character(model_sample_start),
    as.character(model_sample_end_preferred),
    as.character(significance_level),
    information_criterion,
    as.character(max_lags),
    as.character(newey_west_lag),
    as.character(parsimonious_p_threshold),
    as.character(gregory_hansen_trim)
  ),
  rationale = c(
    "inicio del horizonte trimestral del TP",
    "ultimo trimestre con informacion efectiva usada por el script",
    "inicio de la muestra econometrica comun",
    "fin preferido de la muestra, sujeto a disponibilidad de datos",
    "umbral comun para decisiones estadisticas",
    "criterio principal de seleccion por parsimonia",
    "maximo de rezagos trimestrales evaluados",
    "rezago HAC/Newey-West para frecuencia trimestral",
    "umbral de eliminacion general-to-specific",
    "porcentaje excluido en extremos para busqueda Gregory-Hansen"
  )
)

#********************************************************
# 4. ARCHIVOS DE ENTRADA
#********************************************************
# declarar las rutas de las fuentes crudas que alimentan
# todo el flujo posterior de limpieza, panel y modelos.

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

# Convencion economica del BCRA: un aumento del ITCRM indica depreciacion real
# multilateral del peso, por lo que se espera, en principio, un efecto positivo
# sobre exportaciones y negativo sobre importaciones. La respuesta puede operar
# con rezagos y depender de restricciones de oferta/demanda.

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

#********************************************************
# 5. IMPORTACIÓN Y LIMPIEZA DE DATOS
#********************************************************
# importar cada fuente cruda, homogeneizar fechas,
# construir logs y guardar bases intermedias auditables en .dta y .csv.

# ---------------------------------------------------------
# 5.1 Comercio exterior argentino
# ---------------------------------------------------------

# limpiar la planilla de comercio/PIB de INDEC y dejar
# importaciones, exportaciones y PIB real listos en frecuencia trimestral.

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

  # se construye x_chr con las instrucciones de este minibloque.
  x_chr <- str_squish(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  # se ejecuta este minibloque del procedimiento.
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

# se informa en consola el estado de la corrida.
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

# ---------------------------------------------------------
# 5.2 Tipo de Cambio Real Multilateral (ITCRM)
# ---------------------------------------------------------

# convertir la serie diaria del BCRA a promedio
# trimestral para alinearla con las demas variables macroeconomicas.

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
    quarter_date >= analysis_start_date,
    quarter_date <= analysis_end_date
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

# se informa en consola el estado de la corrida.
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

# ---------------------------------------------------------
# 5.3 PIB real de Estados Unidos
# ---------------------------------------------------------

# importar PIB real de Estados Unidos desde FRED y
# construir su version trimestral en logs.

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

# se informa en consola el estado de la corrida.
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

# ---------------------------------------------------------
# 5.4 PIB real de Brasil
# ---------------------------------------------------------

# importar PIB real de Brasil desde FRED y construir su
# version trimestral comparable con el panel.

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

# se informa en consola el estado de la corrida.
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

# ---------------------------------------------------------
# 5.5 Commodity Price Index
# ---------------------------------------------------------

# transformar el indice mensual de commodities del Banco
# Mundial a frecuencia trimestral para usarlo como variable externa.

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

# se informa en consola el estado de la corrida.
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

#********************************************************
# 6. CONSTRUCCIÓN DEL PANEL FINAL
#********************************************************
# unir todas las bases trimestrales por fecha para
# obtener el panel maestro que usaran transformaciones, graficos y modelos.

# Objetivos:
# - Unificar todas las bases
# - Realizar merges por fecha
# - Verificar consistencia temporal
# - Revisar NA's
# - Construir base maestra

# registrar rutas de las bases limpias generadas en la
# seccion 5 para leerlas de forma homogenea.
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

# se evalua esta condicion antes de continuar con el flujo.
if (any(!file.exists(processed_files))) {
  stop(
    "Faltan bases procesadas para construir el panel final: ",
    paste(processed_files[!file.exists(processed_files)], collapse = "; ")
  )
}

# construir una grilla trimestral completa para que los
# faltantes queden visibles luego de unir las fuentes.
# Calendario maestro trimestral del TP.
panel_calendar <- tibble(
  quarter_date = seq.Date(
    from = analysis_start_date,
    to = analysis_end_date,
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

# se importa la base que alimenta el objeto itcrm_panel.
itcrm_panel <- read_dta(itcrm_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    itcrm,
    ln_itcrm,
    itcrm_daily_obs
  )

# se importa la base que alimenta el objeto usa_gdp_panel.
usa_gdp_panel <- read_dta(usa_gdp_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    usa_gdp_real,
    ln_usa_gdp_real
  )

# se importa la base que alimenta el objeto brazil_gdp_panel.
brazil_gdp_panel <- read_dta(brazil_gdp_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    brazil_gdp_real,
    ln_brazil_gdp_real
  )

# se importa la base que alimenta el objeto commodity_panel.
commodity_panel <- read_dta(commodity_dta_file) %>%
  mutate(quarter_date = as.Date(quarter_date)) %>%
  select(
    quarter_date,
    commodity_price_index,
    ln_commodity_price_index,
    commodity_monthly_obs
  )

# hacer los merges por fecha; el left_join conserva la
# grilla completa 2004Q1-2025Q4 y revela faltantes por fuente.
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

# se informa en consola el estado de la corrida.
print(panel_missing)

# Trimestres con algun faltante.
panel_missing_by_quarter <- trade_elasticities_panel %>%
  mutate(missing_values = rowSums(is.na(across(everything())))) %>%
  filter(missing_values > 0) %>%
  select(quarter, quarter_date, missing_values)

# se informa en consola el estado de la corrida.
print(panel_missing_by_quarter)

# Base con casos completos para la etapa econometrica.
trade_elasticities_panel_complete <- trade_elasticities_panel %>%
  filter(if_all(everything(), ~ !is.na(.x)))

# guardar el panel maestro y una version de casos
# completos para trazabilidad y uso eventual en software externo.
# Exportar panel maestro.
write_dta(
  trade_elasticities_panel,
  file.path(processed_data_dir, "trade_elasticities_panel_2004_2025.dta")
)

# se exporta esta salida para documentar los resultados.
write_csv(
  trade_elasticities_panel,
  file.path(processed_data_dir, "trade_elasticities_panel_2004_2025.csv")
)

# Exportar panel de casos completos.
write_dta(
  trade_elasticities_panel_complete,
  file.path(processed_data_dir, "trade_elasticities_panel_complete_cases.dta")
)

# se exporta esta salida para documentar los resultados.
write_csv(
  trade_elasticities_panel_complete,
  file.path(processed_data_dir, "trade_elasticities_panel_complete_cases.csv")
)

# Reabrir .dta exportado.
trade_elasticities_panel_dta <- read_dta(
  file.path(processed_data_dir, "trade_elasticities_panel_2004_2025.dta")
)

#********************************************************
# 7. TRANSFORMACIONES DE VARIABLES
#********************************************************
# crear variables econometricas derivadas, incluyendo
# diferencias logaritmicas, rezagos y la proxy PIBSOCIOS.

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

# extraer ponderadores BCRA y normalizarlos para construir
# una proxy de PIBSOCIOS basada solo en Brasil y Estados Unidos.
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

# se arma la tabla pib_socios_traceability con informacion de este paso.
pib_socios_traceability <- tibble(
  country = c("Brasil", "Estados Unidos"),
  source_variable = c("brasil", "estados_unidos"),
  original_weight = c(
    pib_socios_weights$weight_brazil,
    pib_socios_weights$weight_usa
  ),
  normalized_weight = c(
    pib_socios_weights$weight_brazil_norm,
    pib_socios_weights$weight_usa_norm
  ),
  source = "BCRA ITCRM - hoja Ponderadores",
  weighting_period_start = as.Date("2004-01-01"),
  weighting_period_end = as.Date("2025-12-01"),
  weighting_period_note = paste(
    "Promedio simple del ponderador disponible en la hoja Ponderadores;",
    "los ponderadores de Brasil y Estados Unidos se normalizan para sumar 1",
    "dentro de la proxy PIBSOCIOS."
  )
)

# se construye weight_brazil_norm con las instrucciones de este minibloque.
weight_brazil_norm <- pib_socios_weights$weight_brazil_norm
weight_usa_norm <- pib_socios_weights$weight_usa_norm

# crear variables usadas por estacionariedad,
# cointegracion y modelos de corto plazo.
# Base transformada para analisis econometrico.
trade_elasticities_panel_transformed <- trade_elasticities_panel %>%
  arrange(stata_qdate) %>%
  mutate(
    # Indices base 2004Q1 = 100 para combinar PIBs con unidades distintas.
    usa_gdp_real_index = usa_gdp_real / first(usa_gdp_real[!is.na(usa_gdp_real)]) * 100,
    brazil_gdp_real_index = (
      brazil_gdp_real / first(brazil_gdp_real[!is.na(brazil_gdp_real)]) * 100
    ),
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

# se informa en consola el estado de la corrida.
print(transformed_missing)

# Base transformada con casos completos.
trade_elasticities_panel_transformed_complete <- trade_elasticities_panel_transformed %>%
  filter(if_all(everything(), ~ !is.na(.x)))

# guardar la base con transformaciones para que el panel
# econometrico sea inspeccionable fuera del script.
# Exportar base transformada.
write_dta(
  trade_elasticities_panel_transformed,
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_2004_2025.dta")
)

# se exporta esta salida para documentar los resultados.
write_csv(
  trade_elasticities_panel_transformed,
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_2004_2025.csv")
)

# Exportar base transformada con casos completos.
write_dta(
  trade_elasticities_panel_transformed_complete,
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_complete_cases.dta")
)

# se exporta esta salida para documentar los resultados.
write_csv(
  trade_elasticities_panel_transformed_complete,
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_complete_cases.csv")
)

# Reabrir .dta exportado.
trade_elasticities_panel_transformed_dta <- read_dta(
  file.path(processed_data_dir, "trade_elasticities_panel_transformed_2004_2025.dta")
)

#********************************************************
# 8. VISUALIZACIÓN EXPLORATORIA
#********************************************************
# generar figuras descriptivas para entender tendencias,
# cobertura de datos y dinamica previa a las pruebas econometricas.

# Objetivos de esta seccion:
# - Graficar las series en niveles y en logaritmos.
# - Revisar tendencias, estacionalidad visual y cambios de regimen.
# - Comparar importaciones, exportaciones, PIB argentino, ITCRM y PIBSOCIOS.
# - Generar figuras limpias para el informe final.
#
# Graficos sugeridos:
# - Logs de importaciones, PIB argentino e ITCRM.
# - Logs de exportaciones, PIBSOCIOS y commodities.
# - Diferencias de logs para observar crecimiento trimestral.
# - ITCRM y commodity price index como variables externas.
#
# Pendiente:
# - Guardar las figuras en figures_dir.
# - Usar nombres de archivo consistentes y citables en el informe.

# Crear carpetas de salida si no existen.
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# fijar una estetica comun para que todas las figuras del
# informe tengan formato consistente.
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

# comparar importaciones con actividad interna e ITCRM en
# logs antes de estimar la relacion de largo plazo.
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
    x = NULL,
    y = "Log natural"
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_exploratory_plot(
  plot_imports_logs,
  "section_08_imports_logs.png"
)

# comparar exportaciones con PIB socios y commodities en logs antes
# de definir la ecuacion de largo plazo; el ITCRM ya se muestra en
# la figura de importaciones para evitar repetir la misma serie.
# Logs para la ecuacion de exportaciones.
plot_exports_logs <- trade_elasticities_panel_transformed %>%
  select(
    quarter_date,
    ln_exports_real,
    ln_pib_socios,
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
      ln_commodity_price_index = "Commodity price index"
    )
  ) %>%
  ggplot(aes(x = quarter_date, y = value)) +
  geom_line(color = "#548235", linewidth = 0.7) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x = NULL,
    y = "Log natural"
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_exploratory_plot(
  plot_exports_logs,
  "section_08_exports_logs.png"
)

# observar la dinamica de corto plazo que luego aparece
# en los modelos en primeras diferencias y ECM.
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
    x = NULL,
    y = "Variacion trimestral aproximada (%)",
    color = NULL
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_exploratory_plot(
  plot_trade_growth,
  "section_08_trade_growth.png"
)

# identificar shocks en ITCRM, PIB socios y commodities
# que pueden afectar las elasticidades de corto plazo.
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
    x = NULL,
    y = "Variacion trimestral aproximada (%)",
    color = NULL
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_exploratory_plot(
  plot_external_growth,
  "section_08_external_growth.png"
)

# documentar cobertura y faltantes para justificar la
# muestra econometrica efectiva usada mas adelante.
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
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_exploratory_plot(
  plot_data_coverage,
  "section_08_data_coverage.png",
  width = 9,
  height = 4.8
)


#********************************************************
# 9. ANÁLISIS DE ESTACIONARIEDAD
#********************************************************
# evaluar si las series en logs son I(1) y si sus
# primeras diferencias son estacionarias antes de pasar a cointegracion.

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

# definir explicitamente el tramo usado en pruebas
# econometricas para evitar que cada modelo cambie de muestra sin documentarlo.
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

# se calcula model_sample_end para usarlo en el paso siguiente.
model_sample_end <- min(stationarity_sample_end, model_sample_end_preferred)

# se arma la tabla model_sample_definition con informacion de este paso.
model_sample_definition <- tibble(
  model_sample_start = model_sample_start,
  model_sample_end = model_sample_end,
  model_sample_note = paste(
    "Muestra econometrica comun para estacionariedad, cointegracion y modelos;",
    "las visualizaciones conservan la cobertura completa para documentar",
    "disponibilidad temporal."
  )
)

# se construye stationarity_data con las instrucciones de este minibloque.
stationarity_data <- trade_elasticities_panel_transformed %>%
  filter(
    quarter_date >= model_sample_start,
    quarter_date <= model_sample_end
  ) %>%
  arrange(stata_qdate)

# evaluar estacionalidad simple en primeras diferencias logaritmicas. El test de
# Kruskal-Wallis compara la distribucion por trimestre sin imponer normalidad.
safe_kruskal_p_value <- function(data, variable) {
  test_data <- data %>%
    transmute(q = q, value = .data[[variable]]) %>%
    filter(!is.na(q), !is.na(value))

  # se evalua esta condicion antes de continuar con el flujo.
  if (nrow(test_data) < 12 || n_distinct(test_data$q) < 4) {
    return(NA_real_)
  }

  # se ejecuta el calculo capturando posibles errores.
  tryCatch(
    kruskal.test(value ~ factor(q), data = test_data)$p.value,
    error = function(e) NA_real_
  )
}

# se construye seasonality_diagnostics mediante un bloque de calculo extendido.
seasonality_diagnostics <- tribble(
  ~series_key, ~series_label, ~diff_var,
  "imports", "Importaciones reales", "d_ln_imports_real",
  "exports", "Exportaciones reales", "d_ln_exports_real",
  "gdp_arg", "PIB real Argentina", "d_ln_gdp_real",
  "itcrm", "Tipo de cambio real multilateral", "d_ln_itcrm",
  "pib_socios", "PIB socios comerciales", "d_ln_pib_socios",
  "commodities", "Commodity price index", "d_ln_commodity_price_index"
) %>%
  mutate(
    kruskal_p_value = map_dbl(
      diff_var,
      ~ safe_kruskal_p_value(stationarity_data, .x)
    ),
    seasonality_flag = case_when(
      is.na(kruskal_p_value) ~ "no evaluable",
      kruskal_p_value < significance_level ~
        "posible patron estacional por trimestre",
      TRUE ~ "sin evidencia estadistica al 5%"
    ),
    diagnostic_note = paste(
      "Prueba sobre primeras diferencias logaritmicas;",
      "complementa la inspeccion grafica y no sustituye un ajuste estacional formal."
    )
  )

# listar las series que se evaluan en niveles y primeras
# diferencias, junto con la especificacion KPSS usada en cada caso.
# Series centrales para la hoja de ruta econometrica del TP. KPSS usa tendencia
# en niveles cuando puede haber tendencia deterministica y nivel en diferencias.
stationarity_specs <- tribble(
  ~series_key, ~series_label, ~level_var, ~diff_var, ~kpss_level_null, ~kpss_diff_null,
  "imports", "Importaciones reales", "ln_imports_real", "d_ln_imports_real", "Trend", "Level",
  "exports", "Exportaciones reales", "ln_exports_real", "d_ln_exports_real", "Trend", "Level",
  "gdp_arg", "PIB real Argentina", "ln_gdp_real", "d_ln_gdp_real", "Trend", "Level",
  "itcrm", "Tipo de cambio real multilateral", "ln_itcrm", "d_ln_itcrm", "Trend", "Level",
  "pib_socios", "PIB socios comerciales", "ln_pib_socios", "d_ln_pib_socios", "Trend", "Level",
  "commodities", "Commodity price index", "ln_commodity_price_index",
  "d_ln_commodity_price_index", "Trend", "Level"
)

# verificar cuantas observaciones efectivas tiene cada
# serie antes de interpretar tests de raiz unitaria.
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

# se informa en consola el estado de la corrida.
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
                                   series_label, kpss_level_null,
                                   kpss_diff_null) {
  x <- data[[variable]]
  x <- as.numeric(x[!is.na(x)])
  n_obs <- length(x)

  # se evalua esta condicion antes de continuar con el flujo.
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

  # se construye adf_lag con las instrucciones de este minibloque.
  adf_lag <- trunc((n_obs - 1)^(1 / 3))
  kpss_null <- if_else(
    transformation == "log_level",
    kpss_level_null,
    kpss_diff_null
  )

  # se construye adf_result mediante un bloque de calculo extendido.
  adf_result <- safe_stationarity_test(
    adf.test(x, alternative = "stationary", k = adf_lag)
  )
  pp_result <- safe_stationarity_test(
    pp.test(x, alternative = "stationary")
  )
  kpss_result <- safe_stationarity_test(
    kpss.test(x, null = kpss_null)
  )

  # se ejecuta este bloque amplio del procedimiento.
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
      ),
      test_interpretation = case_when(
        test %in% c("ADF", "Phillips-Perron") & p_value < 0.05 ~
          "evidencia a favor de estacionariedad",
        test %in% c("ADF", "Phillips-Perron") & p_value >= 0.05 ~
          "evidencia compatible con raiz unitaria",
        test == "KPSS" & p_value < 0.05 ~
          "evidencia contra estacionariedad",
        test == "KPSS" & p_value >= 0.05 ~
          "evidencia compatible con estacionariedad",
        TRUE ~ "sin interpretacion disponible"
      )
    )
}

# correr ADF, Phillips-Perron y KPSS para cada serie y
# transformar las salidas en una tabla larga comparable.
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
    function(series_key, series_label, kpss_level_null, kpss_diff_null,
             transformation, variable) {
      run_stationarity_tests(
        data = stationarity_data,
        variable = variable,
        transformation = transformation,
        series_key = series_key,
        series_label = series_label,
        kpss_level_null = kpss_level_null,
        kpss_diff_null = kpss_diff_null
      )
    }
  )

# se informa en consola el estado de la corrida.
print(stationarity_tests)
print(seasonality_diagnostics)

# sintetizar ADF/PP/KPSS y marcar evidencia estacionaria,
# no estacionaria o mixta para cada transformacion.
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
    evidence_nonstationary = (
      !adf_rejects_unit_root &
        !pp_rejects_unit_root &
        kpss_rejects_stationarity
    ),
    contradictory_evidence = (
      (adf_rejects_unit_root | pp_rejects_unit_root) &
        kpss_rejects_stationarity
    ) | (
      !adf_rejects_unit_root &
        !pp_rejects_unit_root &
        !kpss_rejects_stationarity
    ),
    .groups = "drop"
  ) %>%
  mutate(
    stationarity_decision = case_when(
      evidence_stationary ~ "evidencia compatible con I(0)",
      evidence_nonstationary ~ "evidencia compatible con raiz unitaria",
      contradictory_evidence ~ "evidencia mixta",
      TRUE ~ "sin evidencia suficiente de I(0)"
    ),
    final_decision_comment = case_when(
      evidence_stationary ~
        "ADF/PP rechazan raiz unitaria y KPSS no rechaza estacionariedad.",
      evidence_nonstationary ~
        "ADF/PP no rechazan raiz unitaria y KPSS rechaza estacionariedad.",
      contradictory_evidence &
        (adf_rejects_unit_root | pp_rejects_unit_root) &
        kpss_rejects_stationarity ~
        "ADF/PP sugieren estacionariedad, pero KPSS rechaza estacionariedad; revisar tendencia, quiebres o rezagos.",
      contradictory_evidence ~
        "ADF/PP no rechazan raiz unitaria, pero KPSS tampoco rechaza estacionariedad; resultado no concluyente.",
      TRUE ~ "Decision no concluyente."
    )
  )

# se informa en consola el estado de la corrida.
print(stationarity_decisions_by_transform)

# resumir si cada serie parece I(0), I(1) o no
# concluyente antes de pasar a cointegracion.
# Decision preliminar de orden de integracion.
stationarity_order_summary <- stationarity_decisions_by_transform %>%
  select(
    series_key,
    series_label,
    transformation,
    evidence_stationary,
    contradictory_evidence
  ) %>%
  pivot_wider(
    names_from = transformation,
    values_from = c(evidence_stationary, contradictory_evidence)
  ) %>%
  mutate(
    preliminary_order = case_when(
      evidence_stationary_log_level ~ "I(0) en niveles",
      !evidence_stationary_log_level & evidence_stationary_log_difference ~ "I(1)",
      !evidence_stationary_log_level & !evidence_stationary_log_difference ~
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
    ),
    final_decision_comment = case_when(
      coalesce(contradictory_evidence_log_level, FALSE) |
        coalesce(contradictory_evidence_log_difference, FALSE) ~
        "Hay evidencia mixta en al menos una transformacion; interpretar junto con graficos, quiebres y especificacion deterministica.",
      preliminary_order == "I(1)" ~
        "La serie no parece estacionaria en niveles y mejora en primeras diferencias.",
      preliminary_order == "I(0) en niveles" ~
        "La serie presenta evidencia compatible con estacionariedad en niveles.",
      TRUE ~ "Resultado no concluyente; revisar especificacion."
    )
  )

# se informa en consola el estado de la corrida.
print(stationarity_order_summary)

# Exportar resultados en Excel para lectura rapida en informe/anexo.
write_xlsx(
  list(
    model_sample = model_sample_definition,
    sample = stationarity_sample_summary,
    seasonality = seasonality_diagnostics,
    tests = stationarity_tests,
    decisions = stationarity_decisions_by_transform,
    order_summary = stationarity_order_summary
  ),
  file.path(output_dir, "section_09_stationarity_results.xlsx")
)


#********************************************************
# 10. ANÁLISIS DE COINTEGRACIÓN
#********************************************************
# estimar relaciones de largo plazo, guardar residuos y
# decidir si corresponde modelar con ECM o solo con primeras diferencias.

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
# - Comparar el ADF residual con valores criticos Engle-Granger del curso.
# - Aplicar Gregory-Hansen para permitir quiebre estructural.
# - Decidir si corresponde ECM o modelo solo en diferencias.

# preparar la base en niveles logaritmicos sin faltantes
# para que Engle-Granger y Gregory-Hansen usen exactamente la misma muestra.
# Muestra comun para cointegracion: niveles en logs completos dentro de la
# muestra econometrica global.
cointegration_data <- trade_elasticities_panel_transformed %>%
  filter(
    quarter_date >= model_sample_start,
    quarter_date <= model_sample_end,
    !is.na(ln_imports_real),
    !is.na(ln_exports_real),
    !is.na(ln_gdp_real),
    !is.na(ln_itcrm),
    !is.na(ln_pib_socios)
  ) %>%
  arrange(stata_qdate)

# declarar las ecuaciones de largo plazo para
# importaciones y exportaciones antes de estimarlas en forma automatizada.
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

  # se ejecuta este bloque amplio del procedimiento.
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

  # se arma la tabla adf_data con informacion de este paso.
  adf_data <- tibble(
    dy = dy,
    y_lag = y_lag
  )

  # se evalua esta condicion antes de continuar con el flujo.
  if (lag_order > 0) {
    for (lag_i in seq_len(lag_order)) {
      adf_data[[paste0("dy_lag", lag_i)]] <- lag(dy, lag_i)
    }
  }

  # se construye adf_data con las instrucciones de este minibloque.
  adf_data <- adf_data %>%
    filter(if_all(everything(), ~ !is.na(.x)))

  # se evalua esta condicion antes de continuar con el flujo.
  if (lag_order > 0) {
    rhs <- paste(c("y_lag", paste0("dy_lag", seq_len(lag_order))), collapse = " + ")
  } else {
    rhs <- "y_lag"
  }

  # se construye adf_fit con las instrucciones de este minibloque.
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

  # se construye fit con las instrucciones de este minibloque.
  fit <- lm(reformulate(regressors, response = dependent_var), data = model_data)
  residuals <- resid(fit)
  n_obs <- length(residuals)
  adf_lag <- trunc((n_obs - 1)^(1 / 3))

  # se construye adf_result con las instrucciones de este minibloque.
  adf_result <- safe_stationarity_test(
    adf.test(residuals, alternative = "stationary", k = adf_lag)
  )

  # se ejecuta este bloque amplio del procedimiento.
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
        "el p-value del ADF estandar se reporta como aproximacion;",
        "la inferencia formal de Engle-Granger requiere valores criticos",
        "especificos para residuos de cointegracion."
      )
    )
  )
}

# estimar las ecuaciones de largo plazo, extraer
# coeficientes y testear estacionariedad de sus residuos.
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

# se construye cointegration_equation_summary con las instrucciones de este minibloque.
cointegration_equation_summary <- map_dfr(
  engle_granger_results,
  "equation_summary"
)

# se construye cointegration_long_run_coefficients con las instrucciones de este minibloque.
cointegration_long_run_coefficients <- map_dfr(
  engle_granger_results,
  "coefficients"
)

# se construye engle_granger_tests con las instrucciones de este minibloque.
engle_granger_tests <- map_dfr(
  engle_granger_results,
  "engle_granger_test"
)

# incorporar la tabla critica de Engle-Granger usada en clase para que la
# decision no dependa solamente del p-value aproximado del ADF estandar.
engle_granger_critical_values <- tribble(
  ~q_series, ~reference_n_obs, ~critical_value_1pct, ~critical_value_5pct, ~critical_value_10pct,
  2L, 50L, -4.32, -3.67, -3.28,
  2L, 100L, -4.07, -3.37, -3.03,
  2L, 200L, -4.00, -3.37, -3.02,
  3L, 50L, -4.84, -4.11, -3.73,
  3L, 100L, -4.45, -3.93, -3.59,
  3L, 200L, -4.35, -3.78, -3.47
) %>%
  mutate(
    critical_value_source = "Tabla_EG.R usada en clases",
    critical_value_note = paste(
      "q_series cuenta la variable dependiente y los regresores de la",
      "relacion de cointegracion; se selecciona la fila con N mas cercano."
    )
  )

# se construye engle_granger_critical_comparison mediante un bloque de calculo extendido.
engle_granger_critical_comparison <- engle_granger_tests %>%
  left_join(
    cointegration_specs %>%
      transmute(
        model_key,
        q_series = map_int(regressors, length) + 1L
      ),
    by = "model_key"
  ) %>%
  rowwise() %>%
  mutate(
    reference_n_obs = engle_granger_critical_values$reference_n_obs[
      which.min(abs(engle_granger_critical_values$reference_n_obs - n_obs))
    ]
  ) %>%
  ungroup() %>%
  left_join(
    engle_granger_critical_values,
    by = c("q_series", "reference_n_obs")
  ) %>%
  mutate(
    rejects_unit_root_5pct_eg = statistic < critical_value_5pct,
    engle_granger_table_conclusion_5pct = case_when(
      is.na(statistic) | is.na(critical_value_5pct) ~ "no evaluable",
      rejects_unit_root_5pct_eg ~
        "rechaza raiz unitaria en residuos; cointegracion por tabla EG",
      TRUE ~ "no rechaza raiz unitaria en residuos por tabla EG"
    ),
    comparison_note = paste(
      "Comparacion formal contra valores criticos Engle-Granger del curso;",
      "el p-value ADF estandar se mantiene solo como referencia aproximada."
    )
  )

# se construye engle_granger_residuals_long con las instrucciones de este minibloque.
engle_granger_residuals_long <- map_dfr(
  engle_granger_results,
  "residuals"
)

# se construye engle_granger_residuals_wide mediante un bloque de calculo extendido.
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

# definir la busqueda manual de quiebres para evaluar
# cointegracion permitiendo cambios estructurales. Gregory-Hansen se usa como
# evidencia complementaria: la inferencia formal depende de valores criticos
# especificos, trimming, especificacion y tamano muestral.
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

  # se construye n_obs con las instrucciones de este minibloque.
  n_obs <- nrow(model_data)
  adf_lag <- trunc((n_obs - 1)^(1 / 3))
  break_candidates <- seq(
    from = ceiling(gregory_hansen_trim * n_obs),
    to = floor((1 - gregory_hansen_trim) * n_obs)
  )

  # se construye gh_results con las instrucciones de este minibloque.
  gh_results <- map_dfr(
    break_candidates,
    function(break_index) {
      break_data <- model_data %>%
        mutate(
          break_dummy = as.integer(row_number() > break_index)
        )

      # se construye gh_terms mediante un bloque de calculo extendido.
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

      # se construye gh_fit con las instrucciones de este minibloque.
      gh_fit <- lm(
        as.formula(paste(dependent_var, "~", gh_terms)),
        data = break_data
      )

      # se ejecuta este bloque amplio del procedimiento.
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

  # se ejecuta este bloque amplio del procedimiento.
  gh_results %>%
    slice_min(statistic, n = 1, with_ties = FALSE) %>%
    mutate(
      critical_value_5pct_approx = case_when(
        gh_model == "level_shift" ~ -4.95,
        gh_model == "level_shift_trend" ~ -5.29,
        gh_model == "regime_shift" ~ -5.50,
        TRUE ~ NA_real_
      ),
      critical_value_source = paste(
        "aproximado; verificar contra tabla del curso o bibliografia",
        "antes de reportar como resultado definitivo"
      ),
      trimming = gregory_hansen_trim,
      conclusion_5pct = if_else(
        statistic < critical_value_5pct_approx,
        "rechaza no cointegracion con quiebre",
        "no rechaza no cointegracion con quiebre"
      ),
      note = paste(
        "Valores criticos aproximados para Gregory-Hansen con dos regresores;",
        "no presentar como resultado definitivo sin validar los valores",
        "criticos contra el material del curso o la bibliografia."
      )
    )
}

# buscar el quiebre que minimiza el estadistico residual
# para cada ecuacion y tipo de modelo Gregory-Hansen.
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

# combinar evidencia de Engle-Granger y Gregory-Hansen
# para decidir si en la seccion 11 se estima ECM.
# Decision preliminar para orientar la siguiente seccion.
cointegration_decision_summary <- engle_granger_tests %>%
  transmute(
    model_key,
    model_label,
    engle_granger_p_value_approx = p_value,
    engle_granger_conclusion = conclusion_5pct
  ) %>%
  left_join(
    engle_granger_critical_comparison %>%
      select(
        model_key,
        q_series,
        reference_n_obs,
        engle_granger_statistic = statistic,
        engle_granger_critical_value_5pct = critical_value_5pct,
        engle_granger_cointegration = rejects_unit_root_5pct_eg,
        engle_granger_table_conclusion_5pct
      ),
    by = "model_key"
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
      coalesce(engle_granger_cointegration, FALSE) |
        coalesce(gregory_hansen_cointegration, FALSE),
      "estimar ECM con termino de correccion del error",
      "estimar modelo en primeras diferencias sin ECM"
    )
  )

# se informa en consola el estado de la corrida.
print(cointegration_equation_summary)
print(cointegration_long_run_coefficients)
print(engle_granger_tests)
print(engle_granger_critical_comparison)
print(gregory_hansen_tests)
print(cointegration_decision_summary)

# unir los residuos de cointegracion al panel y crear sus
# rezagos, que funcionan como termino de correccion del error.
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


# guardar las salidas de largo plazo, tests y residuos en
# un Excel de lectura directa para el informe/anexo.
# Exportar resultados de cointegracion en tablas separadas.
write_xlsx(
  list(
    model_sample = model_sample_definition,
    equations = cointegration_equation_summary,
    long_run_coefficients = cointegration_long_run_coefficients,
    engle_granger = engle_granger_tests,
    eg_critical_values = engle_granger_critical_values,
    eg_critical_comparison = engle_granger_critical_comparison,
    gregory_hansen = gregory_hansen_tests,
    decision_summary = cointegration_decision_summary,
    residuals = engle_granger_residuals_long
  ),
  file.path(output_dir, "section_10_cointegration_results.xlsx")
)

# Vista reducida de chequeo para evitar buscarlas al final del visor.
trade_elasticities_residuals_view <- trade_elasticities_panel_modeling_dta %>%
  select(
    quarter,
    quarter_date,
    resid_imports_eg,
    resid_exports_eg,
    l1_resid_imports_eg,
    l1_resid_exports_eg
  )

# se informa en consola el estado de la corrida.
print(head(trade_elasticities_residuals_view, 10))
tail(names(trade_elasticities_panel_modeling_dta), 10)

#********************************************************
# 11. MODELOS ECONOMÉTRICOS
#********************************************************
# estimar modelos de corto plazo y ECM con rezagos,
# HAC/Newey-West, diagnosticos, parsimonia y tablas listas para el informe.

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

# usar los parametros definidos en la configuracion global: BIC como criterio
# principal, hasta cuatro rezagos trimestrales y errores Newey-West con lag 4.

# preparar el panel de modelacion, agregar dummies de
# shocks macroeconomicos y restringir a la muestra econometrica global.
# Base comun para modelos: se usa el panel con residuos de Engle-Granger dentro
# de la muestra econometrica global.
modeling_data <- trade_elasticities_panel_modeling_dta %>%
  filter(
    quarter_date >= model_sample_start,
    quarter_date <= model_sample_end
  ) %>%
  arrange(stata_qdate) %>%
  mutate(
    global_crisis_dummy = if_else(
      quarter_date >= as.Date("2008-07-01") &
        quarter_date <= as.Date("2009-12-01"),
      1, 0
    ),
    argentina_2018_dummy = if_else(
      quarter_date >= as.Date("2018-04-01") &
        quarter_date <= as.Date("2019-12-01"),
      1, 0
    ),
    covid_dummy = if_else(
      quarter_date >= as.Date("2020-04-01") &
        quarter_date <= as.Date("2020-10-01"),
      1, 0
    )
  )

# declarar variables que recibiran rezagos de 1 a
# max_lags para evaluar dinamica flexible.
lag_source_vars <- c(
  "d_ln_imports_real",
  "d_ln_exports_real",
  "d_ln_gdp_real",
  "d_ln_itcrm",
  "d_ln_pib_socios",
  "d_ln_commodity_price_index"
)

# se recorre cada elemento necesario para completar este paso.
for (lag_i in seq_len(max_lags)) {
  for (var_name in lag_source_vars) {
    modeling_data[[paste0("l", lag_i, "_", var_name)]] <-
      lag(modeling_data[[var_name]], lag_i)
  }
}

# listar dummies estructurales incluidas inicialmente en
# los modelos y documentar su justificacion.
structural_dummies <- c(
  "global_crisis_dummy",
  "argentina_2018_dummy",
  "covid_dummy"
)

# se arma la tabla structural_dummies_summary con informacion de este paso.
structural_dummies_summary <- tibble(
  dummy_name = structural_dummies,
  start_date = as.Date(c("2008-07-01", "2018-04-01", "2020-04-01")),
  end_date = as.Date(c("2009-12-01", "2019-12-01", "2020-10-01")),
  economic_justification = c(
    "Crisis financiera global y contraccion del comercio internacional.",
    "Crisis cambiaria y macroeconomica argentina iniciada en 2018.",
    "Shock COVID-19 sobre actividad, comercio y precios relativos."
  )
) %>%
  mutate(
    active_observations = map2_int(
      start_date,
      end_date,
      ~ sum(modeling_data$quarter_date >= .x & modeling_data$quarter_date <= .y)
    ),
    included_in_models = TRUE
  )

# traducir la decision de cointegracion en una regla
# operativa para estimar o no modelos ECM.
# Decision operativa: incluir ECM solo cuando la seccion 10 encontro evidencia
# de cointegracion por Engle-Granger o Gregory-Hansen.
modeling_cointegration_decision <- cointegration_decision_summary %>%
  transmute(
    model_key,
    use_ecm = engle_granger_cointegration | gregory_hansen_cointegration,
    cointegration_evidence,
    next_step
  )

# definir, por flujo comercial, variable dependiente,
# regresores contemporaneos, regresores rezagables y termino ECM.
trade_flow_model_info <- tibble(
  trade_flow = c("imports", "exports"),
  dependent_var = c("d_ln_imports_real", "d_ln_exports_real"),
  contemporaneous_vars = list(
    c("d_ln_gdp_real", "d_ln_itcrm"),
    c("d_ln_pib_socios", "d_ln_itcrm", "d_ln_commodity_price_index")
  ),
  lagged_vars = list(
    c("d_ln_imports_real", "d_ln_gdp_real", "d_ln_itcrm"),
    c(
      "d_ln_exports_real",
      "d_ln_pib_socios",
      "d_ln_itcrm",
      "d_ln_commodity_price_index"
    )
  ),
  ecm_term = c("l1_resid_imports_eg", "l1_resid_exports_eg")
)

# se define la funcion auxiliar make_lag_terms.
make_lag_terms <- function(vars, lag_order) {
  as.vector(outer(paste0("l", seq_len(lag_order), "_"), vars, paste0))
}

# se define la funcion auxiliar make_model_terms.
make_model_terms <- function(contemporaneous_vars, lagged_vars, lag_order,
                             ecm_term = NULL, include_ecm = FALSE) {
  c(
    contemporaneous_vars,
    make_lag_terms(lagged_vars, lag_order),
    if (include_ecm) ecm_term else character(0),
    structural_dummies
  )
}

# se define la funcion auxiliar hq_criterion.
hq_criterion <- function(fit) {
  -2 * as.numeric(logLik(fit)) + 2 * log(log(nobs(fit))) * length(coef(fit))
}

# se define la funcion auxiliar safe_test_value.
safe_test_value <- function(expr, field) {
  result <- tryCatch(expr, error = function(e) e)
  if (inherits(result, "error")) {
    return(NA_real_)
  }
  unname(result[[field]])
}

# se define la funcion auxiliar safe_efp_p_value.
safe_efp_p_value <- function(fit) {
  # CUSUM es opcional: si strucchange no esta instalado, el script sigue
  # corriendo y reporta NA en el diagnostico de estabilidad.
  if (!requireNamespace("strucchange", quietly = TRUE)) {
    return(NA_real_)
  }

  # se construye result con las instrucciones de este minibloque.
  result <- tryCatch(
    {
      efp_fit <- strucchange::efp(formula(fit), data = model.frame(fit), type = "Rec-CUSUM")
      strucchange::sctest(efp_fit)$p.value
    },
    error = function(e) NA_real_
  )

  # se ejecuta este minibloque del procedimiento.
  unname(result)
}

# se define la funcion auxiliar estimate_lm_on_common_sample.
estimate_lm_on_common_sample <- function(data, dependent_var, regressors) {
  model_vars <- c(dependent_var, regressors)
  model_data <- data %>%
    select(quarter, quarter_date, stata_qdate, all_of(model_vars)) %>%
    filter(if_all(all_of(model_vars), ~ !is.na(.x))) %>%
    arrange(stata_qdate)

  # se ejecuta este minibloque del procedimiento.
  lm(reformulate(regressors, response = dependent_var), data = model_data)
}

# se define la funcion auxiliar get_common_modeling_sample.
get_common_modeling_sample <- function(data, dependent_var, contemporaneous_vars,
                                       lagged_vars, ecm_term) {
  common_vars <- c(
    dependent_var,
    contemporaneous_vars,
    make_lag_terms(lagged_vars, max_lags),
    ecm_term,
    structural_dummies
  )

  # se ejecuta este minibloque del procedimiento.
  data %>%
    select(quarter, quarter_date, stata_qdate, all_of(common_vars)) %>%
    filter(if_all(all_of(common_vars), ~ !is.na(.x))) %>%
    arrange(stata_qdate)
}

# comparar especificaciones con 1 a 4 rezagos usando
# AIC, BIC y HQ; luego se elegira BIC como criterio principal.
lag_selection_results <- pmap_dfr(
  trade_flow_model_info,
  function(trade_flow, dependent_var, contemporaneous_vars, lagged_vars,
           ecm_term) {
    use_ecm <- modeling_cointegration_decision %>%
      filter(model_key == trade_flow) %>%
      pull(use_ecm)

    # se construye common_sample con las instrucciones de este minibloque.
    common_sample <- get_common_modeling_sample(
      modeling_data,
      dependent_var,
      contemporaneous_vars,
      lagged_vars,
      ecm_term
    )

    # se ejecuta este minibloque del procedimiento.
    map_dfr(
      seq_len(max_lags),
      function(lag_order) {
        map_dfr(
          c(FALSE, TRUE),
          function(include_ecm) {
            model_type <- if_else(include_ecm, "ecm", "diff")

            # se evalua esta condicion antes de continuar con el flujo.
            if (include_ecm && !isTRUE(use_ecm)) {
              return(tibble(
                trade_flow = trade_flow,
                model_type = model_type,
                lag_order = lag_order,
                n_obs = NA_integer_,
                aic = NA_real_,
                bic = NA_real_,
                hq = NA_real_,
                estimated = FALSE
              ))
            }

            # se construye regressors con las instrucciones de este minibloque.
            regressors <- make_model_terms(
              contemporaneous_vars,
              lagged_vars,
              lag_order,
              ecm_term,
              include_ecm
            )

            # se construye fit con las instrucciones de este minibloque.
            fit <- estimate_lm_on_common_sample(
              common_sample,
              dependent_var,
              regressors
            )

            # se ejecuta este bloque amplio del procedimiento.
            tibble(
              trade_flow = trade_flow,
              model_type = model_type,
              lag_order = lag_order,
              n_obs = nobs(fit),
              aic = AIC(fit),
              bic = BIC(fit),
              hq = hq_criterion(fit),
              estimated = TRUE
            )
          }
        )
      }
    )
  }
)

# seleccionar el rezago optimo por BIC para cada flujo y
# tipo de modelo.
selected_lags <- lag_selection_results %>%
  filter(estimated) %>%
  group_by(trade_flow, model_type) %>%
  slice_min(bic, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    trade_flow,
    model_type,
    selected_lag_bic = lag_order,
    selected_by = "BIC"
  )

# construir la tabla maestra de especificaciones que
# alimenta la estimacion automatica de modelos.
econometric_model_specs <- crossing(
  trade_flow = c("imports", "exports"),
  model_type = c("diff", "ecm")
) %>%
  left_join(trade_flow_model_info, by = "trade_flow") %>%
  left_join(modeling_cointegration_decision, by = c("trade_flow" = "model_key")) %>%
  left_join(selected_lags, by = c("trade_flow", "model_type")) %>%
  mutate(
    selected_lag_bic = replace_na(selected_lag_bic, 1L),
    selected_by = replace_na(selected_by, "BIC"),
    include_ecm = model_type == "ecm",
    estimate_model = model_type == "diff" | (model_type == "ecm" & use_ecm),
    model_key = paste(trade_flow, model_type, sep = "_"),
    model_label = case_when(
      trade_flow == "imports" & model_type == "diff" ~
        "Importaciones - primeras diferencias",
      trade_flow == "imports" & model_type == "ecm" ~
        "Importaciones - ECM",
      trade_flow == "exports" & model_type == "diff" ~
        "Exportaciones - primeras diferencias",
      TRUE ~ "Exportaciones - ECM"
    ),
    regressors = pmap(
      list(contemporaneous_vars, lagged_vars, selected_lag_bic, ecm_term, include_ecm),
      make_model_terms
    ),
    role = case_when(
      model_type == "diff" ~
        "modelo de corto plazo en primeras diferencias",
      estimate_model ~
        "modelo ECM con termino de correccion del error rezagado",
      TRUE ~
        "ECM no estimado porque la seccion 10 no encontro cointegracion"
    )
  )

# se define la funcion auxiliar simplify_model_terms.
simplify_model_terms <- function(data, dependent_var, regressors,
                                 protected_terms,
                                 p_threshold = parsimonious_p_threshold) {
  # Estrategia general-to-specific: se remueven terminos no protegidos con
  # baja significatividad individual y se acepta la reduccion solo si mejora BIC.
  current_terms <- regressors
  current_fit <- estimate_lm_on_common_sample(data, dependent_var, current_terms)
  current_bic <- BIC(current_fit)
  simplification_steps <- tibble()
  step_id <- 0

  # se ejecuta este minibloque del procedimiento.
  repeat {
    removable_terms <- setdiff(current_terms, protected_terms)
    if (length(removable_terms) == 0) {
      break
    }

    # se construye coef_table con las instrucciones de este minibloque.
    coef_table <- summary(current_fit)$coefficients
    candidate_p_values <- coef_table[rownames(coef_table) %in% removable_terms, "Pr(>|t|)"]
    if (length(candidate_p_values) == 0 || max(candidate_p_values, na.rm = TRUE) <= p_threshold) {
      break
    }

    # se construye remove_term con las instrucciones de este minibloque.
    remove_term <- names(which.max(candidate_p_values))
    candidate_terms <- setdiff(current_terms, remove_term)
    candidate_fit <- estimate_lm_on_common_sample(data, dependent_var, candidate_terms)
    candidate_bic <- BIC(candidate_fit)

    # se construye step_id mediante un bloque de calculo extendido.
    step_id <- step_id + 1
    simplification_steps <- bind_rows(
      simplification_steps,
      tibble(
        step = step_id,
        removed_term = remove_term,
        removed_term_p_value = unname(candidate_p_values[remove_term]),
        bic_before = current_bic,
        bic_after = candidate_bic,
        accepted = candidate_bic <= current_bic
      )
    )

    # se evalua esta condicion antes de continuar con el flujo.
    if (candidate_bic <= current_bic) {
      current_terms <- candidate_terms
      current_fit <- candidate_fit
      current_bic <- candidate_bic
    } else {
      break
    }
  }

  # se ejecuta este minibloque del procedimiento.
  list(
    regressors = current_terms,
    fit = current_fit,
    simplification_steps = simplification_steps
  )
}

# se define la funcion auxiliar estimate_trade_model.
estimate_trade_model <- function(data, model_key, trade_flow, model_type,
                                 model_label, dependent_var, regressors,
                                 contemporaneous_vars, lagged_vars, ecm_term,
                                 selected_lag_bic,
                                 include_ecm, estimate_model,
                                 cointegration_evidence, role) {
  if (!estimate_model) {
    return(list(
      fit = NULL,
      final_regressors = character(0),
      simplification_steps = tibble(),
      coefficients = tibble(
        model_key = model_key,
        trade_flow = trade_flow,
        model_label = model_label,
        term = NA_character_,
        estimate = NA_real_,
        nw_std_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        note = role
      ),
      summary = tibble(
        model_key = model_key,
        trade_flow = trade_flow,
        model_label = model_label,
        dependent_var = dependent_var,
        selected_lag_bic = selected_lag_bic,
        regressors = NA_character_,
        n_obs = NA_integer_,
        r_squared = NA_real_,
        adj_r_squared = NA_real_,
        sigma = NA_real_,
        aic = NA_real_,
        bic = NA_real_,
        hq = NA_real_,
        cointegration_evidence = cointegration_evidence,
        role = role
      ),
      diagnostics = tibble()
    ))
  }

  # se define el vector protected_terms usado en este bloque.
  protected_terms <- c(
    contemporaneous_vars,
    if (include_ecm) ecm_term else character(0)
  )
  # Las dummies estructurales no se protegen de forma obligatoria: pueden
  # eliminarse si no aportan parsimonia segun la regla significancia + BIC.

  common_sample <- get_common_modeling_sample(
    data,
    dependent_var,
    contemporaneous_vars,
    lagged_vars,
    ecm_term
  )

  # se construye simplified con las instrucciones de este minibloque.
  simplified <- simplify_model_terms(
    common_sample,
    dependent_var,
    regressors,
    protected_terms
  )

  # se construye fit con las instrucciones de este minibloque.
  fit <- simplified$fit
  model_vars <- c(dependent_var, simplified$regressors)
  model_sample_data <- common_sample %>%
    select(quarter, quarter_date, stata_qdate, all_of(model_vars)) %>%
    filter(if_all(all_of(model_vars), ~ !is.na(.x))) %>%
    arrange(stata_qdate)

  # se construye nw_test con las instrucciones de este minibloque.
  nw_test <- coeftest(
    fit,
    vcov. = NeweyWest(fit, lag = newey_west_lag, prewhite = FALSE)
  )

  # se arma la tabla nw_table con informacion de este paso.
  nw_table <- tibble(
    term = rownames(nw_test),
    estimate = nw_test[, 1],
    nw_std_error = nw_test[, 2],
    t_statistic = nw_test[, 3],
    p_value = nw_test[, 4]
  )

  # se construye fit_summary con las instrucciones de este minibloque.
  fit_summary <- summary(fit)
  residual_values <- residuals(fit)
  ljung_lag <- min(4, max(1, length(residual_values) - 1))

  # se arma la tabla diagnostics con informacion de este paso.
  diagnostics <- tibble(
    model_key = model_key,
    trade_flow = trade_flow,
    model_label = model_label,
    bg_p_value = safe_test_value(bgtest(fit, order = 4), "p.value"),
    bp_p_value = safe_test_value(bptest(fit), "p.value"),
    jarque_bera_p_value = safe_test_value(
      jarque.bera.test(residual_values),
      "p.value"
    ),
    ljung_box_p_value = safe_test_value(
      Box.test(residual_values, lag = ljung_lag, type = "Ljung-Box"),
      "p.value"
    ),
    cusum_p_value = safe_efp_p_value(fit),
    autocorrelation_flag = case_when(
      is.na(bg_p_value) & is.na(ljung_box_p_value) ~ NA_character_,
      coalesce(bg_p_value < 0.05, FALSE) |
        coalesce(ljung_box_p_value < 0.05, FALSE) ~
        "posible autocorrelacion",
      TRUE ~ "sin evidencia al 5%"
    ),
    heteroskedasticity_flag = case_when(
      is.na(bp_p_value) ~ NA_character_,
      bp_p_value < 0.05 ~ "posible heterocedasticidad",
      TRUE ~ "sin evidencia al 5%"
    ),
    non_normality_flag = case_when(
      is.na(jarque_bera_p_value) ~ NA_character_,
      jarque_bera_p_value < 0.05 ~ "posible no normalidad",
      TRUE ~ "sin evidencia al 5%"
    ),
    instability_flag = case_when(
      is.na(cusum_p_value) ~ NA_character_,
      cusum_p_value < 0.05 ~ "posible inestabilidad",
      TRUE ~ "sin evidencia al 5%"
    ),
    diagnostics_note = if_else(
      is.na(cusum_p_value),
      "CUSUM no disponible si el paquete strucchange no esta instalado",
      "p-valores altos no rechazan el supuesto diagnostico correspondiente"
    )
  )

  # se ejecuta este bloque amplio del procedimiento.
  list(
    fit = fit,
    final_regressors = simplified$regressors,
    simplification_steps = simplified$simplification_steps %>%
      mutate(model_key = model_key, .before = 1),
    coefficients = nw_table %>%
      transmute(
        model_key = model_key,
        trade_flow = trade_flow,
        model_label = model_label,
        term = term,
        estimate = estimate,
        nw_std_error = nw_std_error,
        t_statistic = t_statistic,
        p_value = p_value,
        note = paste0("Newey-West HAC, lag = ", newey_west_lag)
      ),
    summary = tibble(
      model_key = model_key,
      trade_flow = trade_flow,
      model_label = model_label,
      dependent_var = dependent_var,
      selected_lag_bic = selected_lag_bic,
      regressors = paste(simplified$regressors, collapse = " + "),
      n_obs = nobs(fit),
      first_quarter = first(model_sample_data$quarter),
      last_quarter = last(model_sample_data$quarter),
      r_squared = fit_summary$r.squared,
      adj_r_squared = fit_summary$adj.r.squared,
      sigma = fit_summary$sigma,
      aic = AIC(fit),
      bic = BIC(fit),
      hq = hq_criterion(fit),
      cointegration_evidence = cointegration_evidence,
      role = role
    ),
    diagnostics = diagnostics
  )
}

# estimar cada especificacion definida, aplicando
# parsimonia, Newey-West y diagnosticos en una misma rutina.
econometric_model_results <- pmap(
  econometric_model_specs,
  function(trade_flow, model_type, dependent_var, contemporaneous_vars,
           lagged_vars, ecm_term, use_ecm, cointegration_evidence, next_step,
           selected_lag_bic, selected_by, include_ecm, estimate_model,
           model_key, model_label, regressors, role) {
    estimate_trade_model(
      data = modeling_data,
      model_key = model_key,
      trade_flow = trade_flow,
      model_type = model_type,
      model_label = model_label,
      dependent_var = dependent_var,
      regressors = regressors,
      contemporaneous_vars = contemporaneous_vars,
      lagged_vars = lagged_vars,
      ecm_term = ecm_term,
      selected_lag_bic = selected_lag_bic,
      include_ecm = include_ecm,
      estimate_model = estimate_model,
      cointegration_evidence = cointegration_evidence,
      role = role
    )
  }
)

# separar resultados en listas/tablas para facilitar
# inspeccion, exportacion y uso posterior en el informe.
econometric_model_fits <- set_names(
  map(econometric_model_results, "fit"),
  econometric_model_specs$model_key
)

# se construye econometric_model_coefficients con las instrucciones de este minibloque.
econometric_model_coefficients <- map_dfr(
  econometric_model_results,
  "coefficients"
)

# se construye econometric_model_summary con las instrucciones de este minibloque.
econometric_model_summary <- map_dfr(
  econometric_model_results,
  "summary"
)

# se construye econometric_model_diagnostics con las instrucciones de este minibloque.
econometric_model_diagnostics <- map_dfr(
  econometric_model_results,
  "diagnostics"
)

# se construye model_simplification_steps con las instrucciones de este minibloque.
model_simplification_steps <- map_dfr(
  econometric_model_results,
  "simplification_steps"
)

# se evalua esta condicion antes de continuar con el flujo.
if (ncol(model_simplification_steps) == 0) {
  model_simplification_steps <- tibble(
    model_key = character(),
    step = integer(),
    removed_term = character(),
    removed_term_p_value = numeric(),
    bic_before = numeric(),
    bic_after = numeric(),
    accepted = logical()
  )
}

# definir modelos alternativos de exportaciones con commodity contemporaneo,
# solo commodity rezagado y sin commodities para evaluar sensibilidad de la
# especificacion sin perder parsimonia.
exports_commodity_sensitivity_info <- tibble(
  commodity_variant = c(
    "with_commodities",
    "lagged_commodity_only",
    "without_commodities"
  ),
  dependent_var = "d_ln_exports_real",
  contemporaneous_vars = list(
    c("d_ln_pib_socios", "d_ln_itcrm", "d_ln_commodity_price_index"),
    c("d_ln_pib_socios", "d_ln_itcrm"),
    c("d_ln_pib_socios", "d_ln_itcrm")
  ),
  lagged_vars = list(
    c(
      "d_ln_exports_real",
      "d_ln_pib_socios",
      "d_ln_itcrm",
      "d_ln_commodity_price_index"
    ),
    c(
      "d_ln_exports_real",
      "d_ln_pib_socios",
      "d_ln_itcrm",
      "d_ln_commodity_price_index"
    ),
    c("d_ln_exports_real", "d_ln_pib_socios", "d_ln_itcrm")
  ),
  ecm_term = "l1_resid_exports_eg"
)

# se construye exports_use_ecm con las instrucciones de este minibloque.
exports_use_ecm <- modeling_cointegration_decision %>%
  filter(model_key == "exports") %>%
  pull(use_ecm)

# repetir seleccion de rezagos para la sensibilidad de
# commodities, manteniendo BIC como criterio principal.
exports_commodity_sensitivity_lag_selection <- pmap_dfr(
  exports_commodity_sensitivity_info,
  function(commodity_variant, dependent_var, contemporaneous_vars,
           lagged_vars, ecm_term) {
    common_sample <- get_common_modeling_sample(
      modeling_data,
      dependent_var,
      contemporaneous_vars,
      lagged_vars,
      ecm_term
    )

    # se ejecuta este minibloque del procedimiento.
    map_dfr(
      seq_len(max_lags),
      function(lag_order) {
        map_dfr(
          c(FALSE, TRUE),
          function(include_ecm) {
            model_type <- if_else(include_ecm, "ecm", "diff")

            # se evalua esta condicion antes de continuar con el flujo.
            if (include_ecm && !isTRUE(exports_use_ecm)) {
              return(tibble(
                commodity_variant = commodity_variant,
                model_type = model_type,
                lag_order = lag_order,
                n_obs = NA_integer_,
                aic = NA_real_,
                bic = NA_real_,
                hq = NA_real_,
                estimated = FALSE
              ))
            }

            # se construye regressors con las instrucciones de este minibloque.
            regressors <- make_model_terms(
              contemporaneous_vars,
              lagged_vars,
              lag_order,
              ecm_term,
              include_ecm
            )

            # se construye fit con las instrucciones de este minibloque.
            fit <- estimate_lm_on_common_sample(
              common_sample,
              dependent_var,
              regressors
            )

            # se ejecuta este bloque amplio del procedimiento.
            tibble(
              commodity_variant = commodity_variant,
              model_type = model_type,
              lag_order = lag_order,
              n_obs = nobs(fit),
              aic = AIC(fit),
              bic = BIC(fit),
              hq = hq_criterion(fit),
              estimated = TRUE
            )
          }
        )
      }
    )
  }
)

# se construye exports_commodity_sensitivity_selected_lags mediante un bloque de calculo extendido.
exports_commodity_sensitivity_selected_lags <-
  exports_commodity_sensitivity_lag_selection %>%
  filter(estimated) %>%
  group_by(commodity_variant, model_type) %>%
  slice_min(bic, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    commodity_variant,
    model_type,
    selected_lag_bic = lag_order,
    selected_by = "BIC"
  )

# se define la funcion auxiliar estimate_exports_commodity_sensitivity.
estimate_exports_commodity_sensitivity <- function(commodity_variant,
                                                   model_type,
                                                   dependent_var,
                                                   contemporaneous_vars,
                                                   lagged_vars,
                                                   ecm_term,
                                                   selected_lag_bic,
                                                   selected_by) {
  include_ecm <- model_type == "ecm"
  regressors <- make_model_terms(
    contemporaneous_vars,
    lagged_vars,
    selected_lag_bic,
    ecm_term,
    include_ecm
  )
  common_sample <- get_common_modeling_sample(
    modeling_data,
    dependent_var,
    contemporaneous_vars,
    lagged_vars,
    ecm_term
  )
  fit <- estimate_lm_on_common_sample(common_sample, dependent_var, regressors)
  fit_summary <- summary(fit)
  nw_test <- coeftest(
    fit,
    vcov. = NeweyWest(fit, lag = newey_west_lag, prewhite = FALSE)
  )
  has_commodity_terms <- any(str_detect(regressors, "commodity_price_index"))

  # se ejecuta este bloque amplio del procedimiento.
  tibble(
    model_key = paste("exports", model_type, commodity_variant, sep = "_"),
    commodity_variant = commodity_variant,
    model_type = model_type,
    includes_commodity_terms = has_commodity_terms,
    selected_lag_bic = selected_lag_bic,
    selected_by = selected_by,
    n_obs = nobs(fit),
    r_squared = fit_summary$r.squared,
    adj_r_squared = fit_summary$adj.r.squared,
    sigma = fit_summary$sigma,
    aic = AIC(fit),
    bic = BIC(fit),
    hq = hq_criterion(fit),
    commodity_current_estimate = if ("d_ln_commodity_price_index" %in% rownames(nw_test)) {
      nw_test["d_ln_commodity_price_index", 1]
    } else {
      NA_real_
    },
    commodity_current_p_value = if ("d_ln_commodity_price_index" %in% rownames(nw_test)) {
      nw_test["d_ln_commodity_price_index", 4]
    } else {
      NA_real_
    },
    regressors = paste(regressors, collapse = " + "),
    note = paste(
      "Sensibilidad de exportaciones con y sin commodity price index;",
      "comparar AIC/BIC/HQ y significancia del termino de commodities."
    )
  )
}

# estimar y resumir la sensibilidad con/sin commodities
# para comparar ajuste y significancia del termino commodity.
exports_commodity_sensitivity_summary <- exports_commodity_sensitivity_info %>%
  expand_grid(model_type = c("diff", "ecm")) %>%
  left_join(
    exports_commodity_sensitivity_selected_lags,
    by = c("commodity_variant", "model_type")
  ) %>%
  filter(model_type == "diff" | isTRUE(exports_use_ecm)) %>%
  pmap_dfr(
    function(commodity_variant, dependent_var, contemporaneous_vars,
             lagged_vars, ecm_term, model_type, selected_lag_bic,
             selected_by) {
      estimate_exports_commodity_sensitivity(
        commodity_variant,
        model_type,
        dependent_var,
        contemporaneous_vars,
        lagged_vars,
        ecm_term,
        selected_lag_bic,
        selected_by
      )
    }
  )

# se define la funcion auxiliar calculate_vif_from_fit.
calculate_vif_from_fit <- function(fit) {
  if (is.null(fit)) {
    return(tibble())
  }

  # se construye x_matrix con las instrucciones de este minibloque.
  x_matrix <- model.matrix(fit)
  x_matrix <- x_matrix[, colnames(x_matrix) != "(Intercept)", drop = FALSE]

  # se evalua esta condicion antes de continuar con el flujo.
  if (ncol(x_matrix) == 0) {
    return(tibble())
  }

  # se evalua esta condicion antes de continuar con el flujo.
  if (ncol(x_matrix) == 1) {
    return(
      tibble(
        term = colnames(x_matrix),
        vif = NA_real_,
        multicollinearity_flag = "no evaluable"
      )
    )
  }

  # se ejecuta este bloque amplio del procedimiento.
  map_dfr(
    colnames(x_matrix),
    function(term_name) {
      y <- x_matrix[, term_name]
      x_others <- x_matrix[, colnames(x_matrix) != term_name, drop = FALSE]
      aux_fit <- lm(y ~ x_others)
      aux_r_squared <- summary(aux_fit)$r.squared
      vif_value <- 1 / (1 - aux_r_squared)

      # se ejecuta este bloque amplio del procedimiento.
      tibble(
        term = term_name,
        vif = vif_value,
        multicollinearity_flag = case_when(
          is.na(vif_value) ~ "no evaluable",
          vif_value >= 10 ~ "alta",
          vif_value >= 5 ~ "moderada",
          TRUE ~ "baja"
        )
      )
    }
  )
}

# revisar multicolinealidad en los modelos finales sin agregar dependencia a
# paquetes externos; VIF >= 5 se lee como alerta moderada y VIF >= 10 como alta.
model_multicollinearity_vif <- imap_dfr(
  econometric_model_fits,
  function(fit, model_key_name) {
    calculate_vif_from_fit(fit) %>%
      mutate(model_key = model_key_name, .before = 1)
  }
) %>%
  left_join(
    econometric_model_specs %>%
      select(model_key, trade_flow, model_label),
    by = "model_key"
  ) %>%
  relocate(trade_flow, model_label, .after = model_key)

# se construye external_regressor_correlation con las instrucciones de este minibloque.
external_regressor_correlation <- modeling_data %>%
  select(d_ln_pib_socios, d_ln_itcrm, d_ln_commodity_price_index) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  as_tibble()

# se calcula long_run_coefficients_export para usarlo en el paso siguiente.
long_run_coefficients_export <- cointegration_long_run_coefficients

# aislar los terminos ECM y traducirlos en velocidad de
# ajuste hacia el equilibrio de largo plazo.
ecm_adjustment_summary <- econometric_model_coefficients %>%
  filter(term %in% c("l1_resid_imports_eg", "l1_resid_exports_eg")) %>%
  mutate(
    expected_sign = "negativo",
    adjustment_speed = if_else(estimate < 0, abs(estimate), NA_real_),
    adjustment_percent_per_quarter = 100 * adjustment_speed,
    dynamic_stability = case_when(
      estimate < 0 & abs(estimate) < 1 ~ "estable: convergencia gradual",
      estimate < 0 & abs(estimate) >= 1 ~
        "ajuste negativo pero potencialmente sobrerreaccionado",
      estimate >= 0 ~ "inestable: no corrige hacia el equilibrio",
      TRUE ~ "no evaluable"
    ),
    adjustment_interpretation = case_when(
      is.na(estimate) ~ "ECM no estimado",
      estimate < 0 & p_value < 0.05 ~
        paste0(
          "corrige aproximadamente ",
          round(adjustment_percent_per_quarter, 1),
          "% del desequilibrio por trimestre"
        ),
      estimate < 0 & p_value >= 0.05 ~
        "signo esperado, pero no significativo al 5%",
      estimate >= 0 ~
        "signo no consistente con correccion hacia el equilibrio",
      TRUE ~ "revisar resultado"
    )
  )

# se define la funcion auxiliar extract_main_coefficient_interpretation.
extract_main_coefficient_interpretation <- function(model_key_filter, term_filter,
                                                    coefficient_label,
                                                    expected_sign,
                                                    economic_reading) {
  econometric_model_coefficients %>%
    filter(model_key == model_key_filter, term == term_filter) %>%
    transmute(
      model_key,
      trade_flow,
      coefficient_label = coefficient_label,
      term,
      estimate,
      p_value,
      expected_sign = expected_sign,
      estimated_sign = case_when(
        estimate > 0 ~ "positivo",
        estimate < 0 ~ "negativo",
        estimate == 0 ~ "cero",
        TRUE ~ NA_character_
      ),
      statistically_significant_5pct = p_value < 0.05,
      economic_consistency = case_when(
        expected_sign == "positivo" & estimate > 0 ~ "consistente",
        expected_sign == "negativo" & estimate < 0 ~ "consistente",
        str_detect(expected_sign, "depende") ~ "requiere interpretar definicion",
        TRUE ~ "revisar signo"
      ),
      economic_reading = economic_reading
    )
}

# resumir coeficientes economicamente relevantes en una
# tabla narrativa para el cuerpo del informe.
economic_interpretation_summary <- bind_rows(
  extract_main_coefficient_interpretation(
    "imports_ecm",
    "d_ln_gdp_real",
    "elasticidad ingreso corto plazo - importaciones",
    "positivo",
    "Un aumento del PIB real argentino deberia elevar la demanda de importaciones."
  ),
  extract_main_coefficient_interpretation(
    "imports_ecm",
    "d_ln_itcrm",
    "elasticidad tipo de cambio corto plazo - importaciones",
    "depende de la convencion del ITCRM",
    "La interpretacion del signo requiere confirmar si un aumento del ITCRM implica depreciacion real."
  ),
  extract_main_coefficient_interpretation(
    "exports_ecm",
    "d_ln_pib_socios",
    "elasticidad ingreso socios corto plazo - exportaciones",
    "positivo",
    "Un aumento del PIB de socios comerciales deberia elevar la demanda externa de exportaciones argentinas."
  ),
  extract_main_coefficient_interpretation(
    "exports_ecm",
    "d_ln_itcrm",
    "elasticidad tipo de cambio corto plazo - exportaciones",
    "depende de la convencion del ITCRM",
    "La interpretacion del signo requiere confirmar si un aumento del ITCRM implica depreciacion real."
  ),
  extract_main_coefficient_interpretation(
    "exports_ecm",
    "d_ln_commodity_price_index",
    "efecto commodity corto plazo - exportaciones",
    "positivo",
    "Un aumento de precios de commodities podria elevar el valor real/exportable asociado al sector externo."
  ),
  ecm_adjustment_summary %>%
    transmute(
      model_key,
      trade_flow,
      coefficient_label = "velocidad de ajuste ECM",
      term,
      estimate,
      p_value,
      expected_sign,
      estimated_sign = case_when(
        estimate > 0 ~ "positivo",
        estimate < 0 ~ "negativo",
        estimate == 0 ~ "cero",
        TRUE ~ NA_character_
      ),
      statistically_significant_5pct = p_value < 0.05,
      economic_consistency = case_when(
        estimate < 0 & p_value < 0.05 ~ "consistente",
        estimate < 0 ~ "signo consistente pero no significativo",
        TRUE ~ "revisar signo"
      ),
      economic_reading = adjustment_interpretation
    )
)

# establecer signos esperados para contrastar resultados
# estadisticos con intuicion economica.
expected_signs_reference <- tibble(
  model_key = c(
    "imports_ecm",
    "imports_ecm",
    "imports_ecm",
    "exports_ecm",
    "exports_ecm",
    "exports_ecm",
    "exports_ecm"
  ),
  trade_flow = c(
    "imports",
    "imports",
    "imports",
    "exports",
    "exports",
    "exports",
    "exports"
  ),
  term = c(
    "d_ln_gdp_real",
    "d_ln_itcrm",
    "l1_resid_imports_eg",
    "d_ln_pib_socios",
    "d_ln_itcrm",
    "d_ln_commodity_price_index",
    "l1_resid_exports_eg"
  ),
  variable_label = c(
    "PIB real Argentina",
    "ITCRM",
    "Termino de correccion del error - importaciones",
    "PIB socios comerciales",
    "ITCRM",
    "Commodity price index",
    "Termino de correccion del error - exportaciones"
  ),
  expected_sign = c(
    "positivo",
    "negativo",
    "negativo",
    "positivo",
    "positivo",
    "positivo",
    "negativo"
  ),
  expected_sign_reason = c(
    "Mayor actividad interna aumenta la demanda de importaciones.",
    "Si el ITCRM aumenta con depreciacion real, encarece importaciones y deberia reducirlas.",
    "El ECM debe corregir desvios hacia el equilibrio de largo plazo.",
    "Mayor actividad de socios comerciales aumenta la demanda externa.",
    "Si el ITCRM aumenta con depreciacion real, mejora competitividad externa.",
    "Mayores precios de commodities pueden favorecer exportaciones asociadas.",
    "El ECM debe corregir desvios hacia el equilibrio de largo plazo."
  ),
  itcrm_definition_note = if_else(
    term == "d_ln_itcrm",
    "Se asume convencion BCRA: aumento del ITCRM = depreciacion real multilateral.",
    NA_character_
  )
)

# cruzar signos esperados con coeficientes estimados y
# marcar consistencia economica.
expected_signs_check <- expected_signs_reference %>%
  left_join(
    econometric_model_coefficients %>%
      select(model_key, trade_flow, term, estimate, p_value),
    by = c("model_key", "trade_flow", "term")
  ) %>%
  mutate(
    estimated_sign = case_when(
      estimate > 0 ~ "positivo",
      estimate < 0 ~ "negativo",
      estimate == 0 ~ "cero",
      TRUE ~ NA_character_
    ),
    statistically_significant_5pct = p_value < 0.05,
    sign_consistency = case_when(
      is.na(estimate) ~ "no estimado en especificacion final",
      expected_sign == estimated_sign ~ "consistente",
      TRUE ~ "revisar signo"
    )
  )

# preparar una tabla para comparar elasticidades del trabajo con la literatura
# sugerida por la consigna. Los valores de papers se dejan como NA para completar
# manualmente al redactar, evitando inventar elasticidades bibliograficas.
own_elasticities_for_literature <- bind_rows(
  long_run_coefficients_export %>%
    filter(term != "(Intercept)") %>%
    transmute(
      source = "Este trabajo",
      horizon = "largo plazo",
      trade_flow = model_key,
      variable = term,
      estimate = estimate,
      reference_period = paste(
        as.character(as.yearqtr(model_sample_start)),
        as.character(as.yearqtr(model_sample_end)),
        sep = "-"
      ),
      note = "coeficiente de la ecuacion de cointegracion Engle-Granger"
    ),
  economic_interpretation_summary %>%
    filter(!str_detect(coefficient_label, "velocidad")) %>%
    transmute(
      source = "Este trabajo",
      horizon = "corto plazo",
      trade_flow = trade_flow,
      variable = term,
      estimate = estimate,
      reference_period = paste(
        as.character(as.yearqtr(model_sample_start)),
        as.character(as.yearqtr(model_sample_end)),
        sep = "-"
      ),
      note = economic_reading
    )
)

# se arma la tabla literature_benchmark_template con informacion de este paso.
literature_benchmark_template <- tibble(
  source = c(
    "Berrettoni y Castresana (2008)",
    "Bus y Nicolini-Llosa (2007)",
    "Zack y Sotelsek (2016)"
  ),
  horizon = NA_character_,
  trade_flow = NA_character_,
  variable = NA_character_,
  estimate = NA_real_,
  reference_period = NA_character_,
  note = paste(
    "Completar manualmente desde el paper antes de cerrar el informe;",
    "la consigna pide comparar resultados con la literatura sugerida."
  )
)

# se unen filas para formar literature_comparison_template.
literature_comparison_template <- bind_rows(
  own_elasticities_for_literature,
  literature_benchmark_template
)

# se informa en consola el estado de la corrida.
print(lag_selection_results)
print(econometric_model_specs)
print(econometric_model_summary)
print(econometric_model_coefficients)
print(econometric_model_diagnostics)
print(exports_commodity_sensitivity_summary)
print(model_multicollinearity_vif)
print(external_regressor_correlation)
print(ecm_adjustment_summary)
print(economic_interpretation_summary)
print(expected_signs_check)
print(literature_comparison_template)

# exportar trazabilidad de PIBSOCIOS tambien como CSV
# independiente para consulta rapida.
write_csv(
  pib_socios_traceability,
  file.path(output_dir, "pib_socios_traceability.csv")
)

# se exporta esta salida para documentar los resultados.
write_csv(
  literature_comparison_template,
  file.path(output_dir, "literature_comparison_template.csv")
)

# exportar todas las salidas de modelacion en hojas
# separadas para revisar especificaciones, coeficientes y diagnosticos.
write_xlsx(
  list(
    model_sample = model_sample_definition,
    specifications = econometric_model_specs %>%
      mutate(
        contemporaneous_vars = map_chr(contemporaneous_vars, ~ paste(.x, collapse = " + ")),
        lagged_vars = map_chr(lagged_vars, ~ paste(.x, collapse = " + ")),
        regressors = map_chr(regressors, ~ paste(.x, collapse = " + "))
      ),
    lag_selection = lag_selection_results,
    model_summary = econometric_model_summary,
    coefficients_hac = econometric_model_coefficients,
    diagnostics = econometric_model_diagnostics,
    multicollinearity_vif = model_multicollinearity_vif,
    external_regressor_correlation = external_regressor_correlation,
    exports_commodity_sensitivity = exports_commodity_sensitivity_summary,
    simplification_steps = model_simplification_steps,
    long_run_coefficients = long_run_coefficients_export,
    pib_socios_traceability = pib_socios_traceability,
    structural_dummies = structural_dummies_summary,
    economic_interpretation_summary = economic_interpretation_summary,
    expected_signs_check = expected_signs_check,
    literature_comparison_template = literature_comparison_template,
    ecm_adjustment = ecm_adjustment_summary
  ),
  file.path(output_dir, "section_11_econometric_models.xlsx")
)


#********************************************************
# 12. DIAGNÓSTICOS DEL MODELO
#********************************************************
# ordenar los diagnosticos ya calculados en la seccion 11 y convertirlos en
# una lectura econometrica directa para informe y anexo.

diagnostic_tests_reference <- tibble(
  diagnostic_area = c(
    "autocorrelacion",
    "autocorrelacion",
    "heterocedasticidad",
    "normalidad",
    "estabilidad"
  ),
  test = c(
    "Breusch-Godfrey",
    "Ljung-Box",
    "Breusch-Pagan",
    "Jarque-Bera",
    "CUSUM"
  ),
  null_hypothesis = c(
    "no hay autocorrelacion serial",
    "no hay autocorrelacion serial conjunta",
    "varianza homocedastica",
    "residuos con distribucion normal",
    "parametros estables en la muestra"
  ),
  reading_rule = c(
    rep("p-value < 0.05 indica evidencia de problema diagnostico", 5)
  ),
  implication = c(
    "si se rechaza, Newey-West ayuda en inferencia, pero conviene revisar dinamica",
    "si se rechaza, puede faltar estructura dinamica o rezagos",
    "si se rechaza, los errores robustos son necesarios para inferencia",
    "si se rechaza, interpretar t-tests con cautela en muestras pequenas",
    "si se rechaza, los coeficientes promedio pueden ocultar quiebres"
  )
)

# resumir los flags de la seccion 11 en un semaforo por modelo.
model_diagnostics_summary <- econometric_model_diagnostics %>%
  mutate(
    diagnostic_flag_count = rowSums(
      across(
        c(
          autocorrelation_flag,
          heteroskedasticity_flag,
          non_normality_flag,
          instability_flag
        ),
        ~ .x != "sin evidencia al 5%"
      ),
      na.rm = TRUE
    ),
    overall_diagnostic_reading = case_when(
      diagnostic_flag_count == 0 ~ "sin alertas diagnosticas al 5%",
      diagnostic_flag_count == 1 ~ "una alerta diagnostica: interpretar con cautela",
      diagnostic_flag_count >= 2 ~ "multiples alertas: revisar especificacion",
      TRUE ~ "diagnostico no evaluable"
    ),
    report_priority = case_when(
      diagnostic_flag_count == 0 ~ "apto para tabla principal",
      diagnostic_flag_count == 1 ~ "mencionar cautela en nota",
      diagnostic_flag_count >= 2 ~ "priorizar discusion en anexo",
      TRUE ~ "revisar manualmente"
    ),
    recommended_action = case_when(
      diagnostic_flag_count == 0 ~
        "mantener especificacion e interpretar coeficientes HAC",
      autocorrelation_flag != "sin evidencia al 5%" ~
        "revisar rezagos y comparar con seleccion AIC/HQ",
      instability_flag != "sin evidencia al 5%" ~
        "evaluar dummies estructurales o submuestras",
      heteroskedasticity_flag != "sin evidencia al 5%" ~
        "mantener errores HAC y reportar robustez",
      non_normality_flag != "sin evidencia al 5%" ~
        "interpretar pruebas individuales con cautela",
      TRUE ~ "revisar diagnostico manualmente"
    )
  )

# pasar los flags a formato largo para identificar rapidamente que problema
# aparece en cada modelo.
diagnostic_flags_long <- model_diagnostics_summary %>%
  select(
    model_key,
    trade_flow,
    model_label,
    autocorrelation_flag,
    heteroskedasticity_flag,
    non_normality_flag,
    instability_flag
  ) %>%
  pivot_longer(
    cols = ends_with("_flag"),
    names_to = "diagnostic_flag",
    values_to = "flag_reading"
  ) %>%
  mutate(
    diagnostic_area = case_when(
      diagnostic_flag == "autocorrelation_flag" ~ "autocorrelacion",
      diagnostic_flag == "heteroskedasticity_flag" ~ "heterocedasticidad",
      diagnostic_flag == "non_normality_flag" ~ "normalidad",
      diagnostic_flag == "instability_flag" ~ "estabilidad",
      TRUE ~ "otro"
    ),
    issue_detected = flag_reading != "sin evidencia al 5%"
  ) %>%
  arrange(model_key, desc(issue_detected), diagnostic_area)

# vincular diagnosticos con ajuste ECM y signos esperados para una lectura
# economica final de cada especificacion.
model_diagnostic_interpretation <- model_diagnostics_summary %>%
  left_join(
    econometric_model_summary %>%
      select(
        model_key,
        n_obs,
        r_squared,
        adj_r_squared,
        aic,
        bic,
        selected_lag_bic,
        role
      ),
    by = "model_key"
  ) %>%
  left_join(
    econometric_model_specs %>%
      select(model_key, selected_by),
    by = "model_key"
  ) %>%
  left_join(
    ecm_adjustment_summary %>%
      select(
        model_key,
        ecm_estimate = estimate,
        ecm_p_value = p_value,
        dynamic_stability,
        adjustment_interpretation
      ),
    by = "model_key"
  ) %>%
  mutate(
    model_reading = case_when(
      str_detect(model_key, "_ecm$") &
        !is.na(ecm_estimate) &
        ecm_estimate < 0 &
        ecm_p_value < 0.05 &
        diagnostic_flag_count == 0 ~
        "ECM con ajuste significativo y sin alertas diagnosticas al 5%",
      str_detect(model_key, "_ecm$") &
        !is.na(ecm_estimate) &
        ecm_estimate < 0 &
        ecm_p_value < 0.05 ~
        "ECM con ajuste significativo, pero requiere cautela por diagnosticos",
      str_detect(model_key, "_ecm$") &
        (is.na(ecm_estimate) | ecm_p_value >= 0.05) ~
        "ECM sin evidencia fuerte de ajuste al equilibrio",
      diagnostic_flag_count == 0 ~
        "modelo en diferencias sin alertas diagnosticas al 5%",
      TRUE ~
        "modelo en diferencias con alertas diagnosticas a revisar"
    )
  )

# armar una guia breve para decidir que tablas usar en el informe.
diagnostics_report_guide <- tibble(
  output_sheet = c(
    "tests_reference",
    "diagnostics_summary",
    "flags_long",
    "model_interpretation"
  ),
  use_in_report = c(
    "nota metodologica",
    "anexo econometrico",
    "revision interna/anexo",
    "cuerpo del informe"
  ),
  content = c(
    "define hipotesis nula y regla de lectura de cada test",
    "resume p-values, flags y accion recomendada por modelo",
    "lista cada alerta diagnostica modelo por modelo",
    "combina diagnosticos, ajuste ECM y lectura economica"
  )
)

# se informa en consola el estado de la corrida.
print(diagnostic_tests_reference)
print(model_diagnostics_summary)
print(diagnostic_flags_long)
print(model_diagnostic_interpretation)
print(diagnostics_report_guide)

# se exporta esta salida para documentar los resultados.
write_xlsx(
  list(
    tests_reference = diagnostic_tests_reference,
    diagnostics_summary = model_diagnostics_summary,
    flags_long = diagnostic_flags_long,
    model_interpretation = model_diagnostic_interpretation,
    report_guide = diagnostics_report_guide
  ),
  file.path(output_dir, "section_12_model_diagnostics.xlsx")
)

# guardar figuras econometricas finales para complementar las tablas del informe
# con evidencia visual sobre cointegracion, ajuste ECM y residuos.
save_econometric_plot <- function(plot, filename, width = 9, height = 6) {
  ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# se construye plot_cointegration_residuals_data con las instrucciones de este minibloque.
plot_cointegration_residuals_data <- engle_granger_residuals_long %>%
  left_join(
    cointegration_specs %>%
      select(model_key, model_label),
    by = "model_key"
  )

# se construye plot_cointegration_residuals mediante un bloque de calculo extendido.
plot_cointegration_residuals <- plot_cointegration_residuals_data %>%
  ggplot(aes(x = quarter_date, y = residual)) +
  geom_hline(
    data = distinct(plot_cointegration_residuals_data, model_label),
    aes(yintercept = 0),
    color = "grey55",
    linewidth = 0.4
  ) +
  geom_line(color = "#2f5597", linewidth = 0.7) +
  facet_wrap(~ model_label, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x = NULL,
    y = "Residuo"
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_econometric_plot(
  plot_cointegration_residuals,
  "section_12_cointegration_residuals.png",
  height = 5.5
)

# se construye final_model_residuals_long con las instrucciones de este minibloque.
final_model_residuals_long <- imap_dfr(
  econometric_model_fits,
  function(fit, model_key_name) {
    if (is.null(fit)) {
      return(tibble())
    }

    # se ejecuta este bloque amplio del procedimiento.
    tibble(
      model_key = model_key_name,
      observation = seq_along(resid(fit)),
      fitted = as.numeric(fitted(fit)),
      residual = as.numeric(resid(fit))
    )
  }
) %>%
  left_join(
    econometric_model_specs %>%
      select(model_key, model_label, trade_flow, model_type),
    by = "model_key"
  )

# se construye plot_model_residuals mediante un bloque de calculo extendido.
plot_model_residuals <- final_model_residuals_long %>%
  ggplot(aes(x = observation, y = residual)) +
  geom_hline(
    data = distinct(final_model_residuals_long, model_label),
    aes(yintercept = 0),
    color = "grey55",
    linewidth = 0.4
  ) +
  geom_line(color = "#7030a0", linewidth = 0.7) +
  facet_wrap(~ model_label, scales = "free_y", ncol = 1) +
  labs(
    x = "Observacion efectiva",
    y = "Residuo"
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_econometric_plot(
  plot_model_residuals,
  "section_12_model_residuals.png",
  height = 7
)

# se construye final_model_residuals_exports con las instrucciones de este minibloque.
final_model_residuals_exports <- final_model_residuals_long %>%
  filter(trade_flow == "exports")

# se construye final_model_residuals_imports con las instrucciones de este minibloque.
final_model_residuals_imports <- final_model_residuals_long %>%
  filter(trade_flow == "imports")

# se construye plot_fitted_vs_residuals_exports mediante un bloque de calculo extendido.
plot_fitted_vs_residuals_exports <- final_model_residuals_exports %>%
  ggplot(aes(x = fitted, y = residual)) +
  geom_hline(
    data = distinct(final_model_residuals_exports, model_label),
    aes(yintercept = 0),
    color = "grey55",
    linewidth = 0.4
  ) +
  geom_point(color = "#548235", alpha = 0.75, size = 1.8) +
  facet_wrap(~ model_label, scales = "free", ncol = 1) +
  labs(
    x = "Valor ajustado",
    y = "Residuo"
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_econometric_plot(
  plot_fitted_vs_residuals_exports,
  "section_12_fitted_vs_residuals_exports.png",
  width = 7,
  height = 4.2
)

# se construye plot_fitted_vs_residuals_imports mediante un bloque de calculo extendido.
plot_fitted_vs_residuals_imports <- final_model_residuals_imports %>%
  ggplot(aes(x = fitted, y = residual)) +
  geom_hline(
    data = distinct(final_model_residuals_imports, model_label),
    aes(yintercept = 0),
    color = "grey55",
    linewidth = 0.4
  ) +
  geom_point(color = "#548235", alpha = 0.75, size = 1.8) +
  facet_wrap(~ model_label, scales = "free", ncol = 2) +
  labs(
    x = "Valor ajustado",
    y = "Residuo"
  ) +
  exploratory_theme

# se ejecuta este minibloque del procedimiento.
save_econometric_plot(
  plot_fitted_vs_residuals_imports,
  "section_12_fitted_vs_residuals_imports.png",
  width = 9,
  height = 4.8
)

# graficar la magnitud del ajuste usando exactamente los coeficientes ECM
# finales reportados en coefficients_hac y ecm_adjustment.
ecm_adjustment_plot_data <- ecm_adjustment_summary %>%
  mutate(
    adjustment_speed_pct = abs(estimate) * 100,
    bar_label = paste0(
      round(adjustment_speed_pct, 1),
      "%\n",
      "ECT = ",
      round(estimate, 3)
    ),
    model_label = fct_reorder(model_label, adjustment_speed_pct, .desc = TRUE)
  )

# se construye plot_ecm_adjustment mediante un bloque de calculo extendido.
plot_ecm_adjustment <- ecm_adjustment_plot_data %>%
  ggplot(aes(x = model_label, y = adjustment_speed_pct)) +
  geom_col(fill = "#2f5597", width = 0.55) +
  geom_text(
    aes(label = bar_label),
    vjust = -0.35,
    size = 3.4,
    lineheight = 0.95
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(
    x = NULL,
    y = "Velocidad de ajuste trimestral (%)"
  ) +
  exploratory_theme +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

# se ejecuta este minibloque del procedimiento.
save_econometric_plot(
  plot_ecm_adjustment,
  "section_12_ecm_adjustment_speed.png",
  width = 7,
  height = 4.8
)


#********************************************************
# 13. EXPORTACIÓN DE RESULTADOS
#********************************************************
# ordenar las salidas finales generadas por el script para que el informe y el
# anexo tengan un mapa claro de archivos, ubicacion y uso.

final_outputs_catalog <- tibble(
  output_group = c(
    rep("base", 5),
    rep("figura", 10),
    rep("tabla econometrica", 4),
    rep("trazabilidad", 5),
    rep("entrega", 3)
  ),
  file_name = c(
    "trade_elasticities_panel_2004_2025.dta",
    "trade_elasticities_panel_2004_2025.csv",
    "trade_elasticities_panel_transformed_2004_2025.dta",
    "trade_elasticities_panel_transformed_2004_2025.csv",
    "trade_elasticities_panel_modeling_2004_2025.dta",
    "section_08_imports_logs.png",
    "section_08_exports_logs.png",
    "section_08_trade_growth.png",
    "section_08_external_growth.png",
    "section_08_data_coverage.png",
    "section_12_cointegration_residuals.png",
    "section_12_model_residuals.png",
    "section_12_fitted_vs_residuals_exports.png",
    "section_12_fitted_vs_residuals_imports.png",
    "section_12_ecm_adjustment_speed.png",
    "section_09_stationarity_results.xlsx",
    "section_10_cointegration_results.xlsx",
    "section_11_econometric_models.xlsx",
    "section_12_model_diagnostics.xlsx",
    "pib_socios_traceability.csv",
    "literature_comparison_template.csv",
    "final_outputs_index.xlsx",
    "section_14_reproducibility_check.xlsx",
    "session_info.txt",
    "trade-elasticities-cointegration-report.pdf",
    "trade-elasticities-cointegration-report.docx",
    "trade_elasticities_cointegration.R"
  ),
  directory = c(
    rep(processed_data_dir, 5),
    rep(figures_dir, 10),
    rep(output_dir, 9),
    report_dir,
    report_dir,
    scripts_dir
  ),
  format = c(
    "dta",
    "csv",
    "dta",
    "csv",
    "dta",
    rep("png", 10),
    rep("xlsx", 4),
    "csv",
    "csv",
    "xlsx",
    "xlsx",
    "txt",
    "pdf",
    "docx",
    "R"
  ),
  report_use = c(
    "anexo de datos",
    "anexo de datos",
    "anexo de datos",
    "anexo de datos",
    "anexo de datos",
    "figura principal o anexo",
    "figura principal o anexo",
    "figura principal o anexo",
    "figura principal o anexo",
    "anexo descriptivo",
    "figura principal o anexo",
    "anexo econometrico",
    "anexo econometrico",
    "anexo econometrico",
    "figura principal o anexo",
    "anexo econometrico",
    "anexo econometrico",
    "tabla principal y anexo",
    "anexo econometrico",
    "anexo metodologico",
    "cuadro comparativo para informe",
    "control de entrega",
    "control de reproducibilidad",
    "control de reproducibilidad",
    "entrega final",
    "entrega final",
    "entrega final"
  ),
  description = c(
    "panel maestro con calendario trimestral completo",
    "copia CSV del panel maestro",
    "panel con logs, diferencias, rezagos y PIBSOCIOS",
    "copia CSV del panel transformado",
    "panel de modelacion con residuos Engle-Granger al final",
    "logs de importaciones, PIB argentino e ITCRM",
    "logs de exportaciones, PIB socios y commodities",
    "crecimiento trimestral de comercio y PIB argentino",
    "crecimiento trimestral de variables externas",
    "cobertura temporal y faltantes por fuente",
    "residuos de cointegracion Engle-Granger",
    "residuos de modelos finales",
    "valores ajustados vs residuos de exportaciones",
    "valores ajustados vs residuos de importaciones",
    "velocidad de ajuste ECM",
    "pruebas ADF, PP, KPSS y orden de integracion",
    "Engle-Granger, Gregory-Hansen y residuos de cointegracion",
    "modelos ECM/diferencias, HAC, seleccion de rezagos y robustez",
    "diagnosticos post-estimacion y lectura econometrica",
    "ponderadores originales y normalizados usados en PIBSOCIOS",
    "plantilla para comparar elasticidades con literatura sugerida",
    "indice de archivos finales generado por la seccion 13",
    "checklist final de reproducibilidad generado por la seccion 14",
    "informacion de sesion R generada por la seccion 14",
    "informe final en PDF",
    "informe final en Word",
    "script R reproducible"
  )
) %>%
  mutate(
    path = file.path(directory, file_name),
    exists = file.exists(path),
    file_size_kb = if_else(
      exists,
      round(as.numeric(file.info(path)$size) / 1024, 1),
      NA_real_
    ),
    last_modified = as.POSIXct(
      if_else(
        exists,
        as.numeric(file.info(path)$mtime),
        NA_real_
      ),
      origin = "1970-01-01",
      tz = "America/Bogota"
    ),
    delivery_status = case_when(
      file_name == "final_outputs_index.xlsx" ~
        "se genera al correr esta seccion",
      file_name %in% c(
        "section_14_reproducibility_check.xlsx",
        "session_info.txt"
      ) ~ "se genera en la seccion 14",
      file_name == "trade-elasticities-cointegration-report.docx" ~
        "pendiente: generar fuera del script",
      exists ~ "disponible",
      TRUE ~ "faltante: revisar seccion correspondiente"
    )
  )

# separar rapidamente los archivos faltantes para no llegar al cierre con
# salidas esperadas sin generar.
missing_final_outputs <- final_outputs_catalog %>%
  filter(
    !exists,
    !file_name %in% c(
      "final_outputs_index.xlsx",
      "section_14_reproducibility_check.xlsx",
      "session_info.txt",
      "trade-elasticities-cointegration-report.docx"
    )
  )

# resumir el estado de entrega por tipo de output.
final_outputs_summary <- final_outputs_catalog %>%
  count(output_group, delivery_status, name = "n_files") %>%
  arrange(output_group, delivery_status)

# se informa en consola el estado de la corrida.
print(final_outputs_catalog)
print(missing_final_outputs)
print(final_outputs_summary)

# se exporta esta salida para documentar los resultados.
write_xlsx(
  list(
    outputs_catalog = final_outputs_catalog,
    missing_outputs = missing_final_outputs,
    outputs_summary = final_outputs_summary
  ),
  file.path(output_dir, "final_outputs_index.xlsx")
)


#********************************************************
# 14. FIN DEL SCRIPT
#********************************************************
# cerrar el script con controles de reproducibilidad y notas metodologicas que
# deben quedar visibles antes de preparar la entrega final.

final_reproducibility_check <- tibble(
  check_item = c(
    "calendario trimestral 2004Q1-2025Q4 construido",
    "panel transformado con logs, diferencias, rezagos y PIBSOCIOS",
    "muestra econometrica comun documentada",
    "estacionalidad evaluada en primeras diferencias",
    "series principales clasificadas como I(1)",
    "cointegracion evaluada para importaciones y exportaciones",
    "residuos Engle-Granger incorporados al panel de modelacion",
    "modelos ECM/diferencias estimados y exportados",
    "diagnosticos post-estimacion sin alertas al 5%",
    "outputs finales esperados disponibles",
    "trazabilidad de PIBSOCIOS disponible"
  ),
  passed = c(
    all(c("2004Q1", "2025Q4") %in% trade_elasticities_panel$quarter),
    all(c(
      "ln_imports_real",
      "ln_exports_real",
      "ln_gdp_real",
      "ln_itcrm",
      "ln_pib_socios",
      "d_ln_imports_real",
      "d_ln_exports_real",
      "l1_d_ln_imports_real",
      "l1_d_ln_exports_real"
    ) %in% names(trade_elasticities_panel_transformed)),
    exists("model_sample_definition") && nrow(model_sample_definition) == 1,
    exists("seasonality_diagnostics") && nrow(seasonality_diagnostics) > 0,
    exists("stationarity_order_summary") &&
      all(stationarity_order_summary$preliminary_order == "I(1)"),
    exists("cointegration_decision_summary") &&
      all(c("imports", "exports") %in% cointegration_decision_summary$model_key),
    all(c(
      "resid_imports_eg",
      "resid_exports_eg",
      "l1_resid_imports_eg",
      "l1_resid_exports_eg"
    ) %in% names(trade_elasticities_panel_modeling_dta)),
    file.exists(file.path(output_dir, "section_11_econometric_models.xlsx")),
    exists("model_diagnostics_summary") &&
      all(model_diagnostics_summary$diagnostic_flag_count == 0),
    exists("missing_final_outputs") && nrow(missing_final_outputs) == 0,
    file.exists(file.path(output_dir, "pib_socios_traceability.csv"))
  ),
  note = c(
    "El calendario maestro cubre el horizonte completo de la consigna.",
    "La base transformada contiene las variables necesarias para estacionariedad, cointegracion y ECM.",
    "La muestra comun queda definida para secciones econometricas; las figuras preservan cobertura completa.",
    "La evaluacion usa Kruskal-Wallis por trimestre como diagnostico simple y complementario.",
    "Si algun test individual presenta evidencia mixta, revisar comentarios de la seccion 9.",
    "La decision ECM se apoya en la evidencia acumulada de Engle-Granger y Gregory-Hansen.",
    "Los residuos y sus rezagos quedan al final del .dta de modelacion.",
    "La seccion 11 concentra seleccion de rezagos, parsimonia, HAC y sensibilidad.",
    "Los tests aplicados no detectan autocorrelacion, heterocedasticidad, no normalidad ni inestabilidad al 5%.",
    "La seccion 13 no detecta archivos faltantes entre los outputs esperados.",
    "PIBSOCIOS queda documentado por ponderadores originales, normalizados, fuente y periodo."
  )
)

# registrar cautelas que deben mencionarse al redactar el informe para no
# sobreinterpretar resultados aproximados.
final_methodological_notes <- tibble(
  topic = c(
    "Engle-Granger",
    "Gregory-Hansen",
    "ITCRM",
    "Newey-West",
    "PIBSOCIOS",
    "Parsimonia",
    "Exportaciones",
    "Multicolinealidad",
    "Wickens-Breusch"
  ),
  note = c(
    "El ADF sobre residuos se compara contra la tabla critica Engle-Granger del curso; el p-value estandar queda solo como referencia aproximada.",
    "Se usa como evidencia complementaria de cointegracion con quiebre; los valores criticos son aproximados y deben validarse contra bibliografia o material del curso.",
    "La interpretacion del signo asume convencion BCRA: aumento del ITCRM implica depreciacion real multilateral.",
    "Se usa lag 4 por datos trimestrales; los errores HAC corrigen inferencia ante heterocedasticidad y autocorrelacion.",
    "La variable se construye como promedio geometrico ponderado de Brasil y Estados Unidos con ponderadores BCRA normalizados.",
    "La reduccion general-to-specific se guia por significancia individual y se acepta solo si mejora BIC.",
    "La ecuacion exportadora es parsimoniosa: aproxima demanda externa, competitividad real y commodities, sin modelar restricciones de oferta domestica.",
    "Los VIF y correlaciones externas se reportan para evaluar si PIBSOCIOS, ITCRM y commodities introducen informacion redundante.",
    "No se implementa estimacion conjunta Wickens-Breusch; los ECM se interpretan como aproximacion estandar de dos etapas."
  ),
  report_location = c(
    "metodologia/anexo",
    "metodologia/anexo",
    "datos e interpretacion de signos",
    "metodologia de estimacion",
    "datos y construccion de variables",
    "metodologia de modelos",
    "especificacion de exportaciones",
    "diagnosticos/anexo",
    "limitaciones metodologicas"
  )
)

# se arma la tabla final_script_summary con informacion de este paso.
final_script_summary <- tibble(
  script_completed_at = Sys.time(),
  project_dir = project_dir,
  output_dir = output_dir,
  processed_data_dir = processed_data_dir,
  figures_dir = figures_dir,
  all_final_checks_passed = all(final_reproducibility_check$passed),
  n_missing_outputs = nrow(missing_final_outputs),
  n_econometric_models = nrow(econometric_model_summary),
  n_model_diagnostic_alerts = sum(model_diagnostics_summary$diagnostic_flag_count)
)

# se informa en consola el estado de la corrida.
print(final_reproducibility_check)
print(final_methodological_notes)
print(final_script_summary)

# se exporta esta salida para documentar los resultados.
write_xlsx(
  list(
    global_model_config = global_model_config,
    reproducibility_check = final_reproducibility_check,
    methodological_notes = final_methodological_notes,
    script_summary = final_script_summary
  ),
  file.path(output_dir, "section_14_reproducibility_check.xlsx")
)

# se ejecuta este minibloque del procedimiento.
capture.output(
  sessionInfo(),
  file = file.path(output_dir, "session_info.txt")
)
