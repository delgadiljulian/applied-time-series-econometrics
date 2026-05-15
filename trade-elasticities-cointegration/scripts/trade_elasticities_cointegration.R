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
# INICIALIZACIÓN DEL ENTORNO
# =========================================================

rm(list = ls())
graphics.off()

# Evitar notación científica
options(scipen = 999)

# =========================================================
# CARGA DE PAQUETES
# =========================================================

# Manipulación de datos
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

# Series de tiempo
library(zoo)
library(xts)
library(tsibble)

# Econometría y cointegración
library(urca)
library(tseries)
library(dynlm)
library(lmtest)
library(sandwich)

# Visualización
library(ggplot2)
library(scales)

# Exportación
library(writexl)
library(haven)

# =========================================================
# DIRECTORIOS DE TRABAJO
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
# ARCHIVOS DE ENTRADA
# =========================================================

# ---------------------------------------------------------
# Argentina
# ---------------------------------------------------------

arg_trade_file <- file.path(
  raw_data_dir,
  "arg_trade_real_2004_2025.xlsx"
)

itcrm_file <- file.path(
  raw_data_dir,
  "arg_itcrm_daily.xlsx"
)

# ---------------------------------------------------------
# Socios comerciales
# ---------------------------------------------------------

usa_gdp_file <- file.path(
  raw_data_dir,
  "usa_real_gdp_quarterly.csv"
)

brazil_gdp_file <- file.path(
  raw_data_dir,
  "brazil_real_gdp_quarterly.csv"
)

# ---------------------------------------------------------
# Commodities
# ---------------------------------------------------------

commodity_file <- file.path(
  raw_data_dir,
  "commodity_index_monthly.xlsx"
)

# =========================================================
# IMPORTACIÓN Y LIMPIEZA DE DATOS
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

# Código aquí


# ---------------------------------------------------------
# 5.2 Tipo de Cambio Real Multilateral (ITCRM)
# ---------------------------------------------------------

# Objetivos:
# - Importar serie diaria del BCRA
# - Convertir fechas
# - Agregar frecuencia trimestral
# - Calcular promedio trimestral
# - Homogeneizar período de análisis

# Código aquí


# ---------------------------------------------------------
# 5.3 PIB real de Estados Unidos
# ---------------------------------------------------------

# Objetivos:
# - Importar datos FRED
# - Limpiar columnas
# - Convertir fechas trimestrales
# - Renombrar variable
# - Verificar frecuencia y cobertura

# Código aquí


# ---------------------------------------------------------
# 5.4 PIB real de Brasil
# ---------------------------------------------------------

# Objetivos:
# - Importar datos FRED
# - Limpiar columnas
# - Convertir fechas trimestrales
# - Renombrar variable
# - Verificar frecuencia y cobertura

# Código aquí


# ---------------------------------------------------------
# 5.5 Commodity Price Index
# ---------------------------------------------------------

# Objetivos:
# - Importar Pink Sheet del Banco Mundial
# - Seleccionar índice relevante
# - Convertir frecuencia mensual a trimestral
# - Construir índice promedio trimestral
# - Homogeneizar fechas

# Código aquí


# =========================================================
# CONSTRUCCIÓN DEL PANEL FINAL
# =========================================================

# Objetivos:
# - Unificar todas las bases
# - Realizar merges por fecha
# - Verificar consistencia temporal
# - Revisar NA's
# - Construir base maestra

# Código aquí


# =========================================================
# TRANSFORMACIONES DE VARIABLES
# =========================================================

# Objetivos:
# - Aplicar logaritmos
# - Construir diferencias
# - Crear tasas de crecimiento
# - Verificar estacionariedad preliminar

# Código aquí


# =========================================================
# VISUALIZACIÓN EXPLORATORIA
# =========================================================

# Objetivos:
# - Graficar series originales
# - Graficar log-series
# - Analizar tendencias
# - Identificar posibles quiebres estructurales

# Código aquí


# =========================================================
# ANÁLISIS DE ESTACIONARIEDAD
# =========================================================

# Objetivos:
# - ADF Test
# - Phillips-Perron
# - KPSS
# - Determinar orden de integración

# Código aquí


# =========================================================
# ANÁLISIS DE COINTEGRACIÓN
# =========================================================

# Objetivos:
# - Engle-Granger
# - Johansen
# - Evaluar relaciones de largo plazo

# Código aquí


# =========================================================
# MODELOS ECONOMÉTRICOS
# =========================================================

# Objetivos:
# - Estimar ecuación de largo plazo
# - Estimar ECM (Error Correction Model)
# - Interpretar elasticidades

# Código aquí


# =========================================================
# DIAGNÓSTICOS DEL MODELO
# =========================================================

# Objetivos:
# - Autocorrelación
# - Heterocedasticidad
# - Normalidad de residuos
# - Estabilidad estructural

# Código aquí


# =========================================================
# EXPORTACIÓN DE RESULTADOS
# =========================================================

# Objetivos:
# - Exportar tablas
# - Guardar gráficos
# - Exportar base procesada
# - Guardar outputs econométricos

# Código aquí


# =========================================================
# FIN DEL SCRIPT
# =========================================================