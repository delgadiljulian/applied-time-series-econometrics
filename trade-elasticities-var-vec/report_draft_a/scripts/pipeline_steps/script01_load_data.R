# ============================================================
# TP3 - SERIES DE TIEMPO
# Script 01: Carga inicial de datos
# ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(lubridate)
library(zoo)
library(writexl)

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

dir.create(
  here::here("data", "processed", "final_data_panel"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("data", "processed", "auxiliary_data"),
  recursive = TRUE,
  showWarnings = FALSE
)

here::here()
list.files(
  here("data", "raw"),
  full.names = FALSE
)


# ============================================================
# 1. PIB REAL DE ESTADOS UNIDOS
# ============================================================

ruta_pib_usa <- here(
  "data",
  "raw",
  "usa_real_gdp_quarterly.xlsx"
)

pib_usa_raw <- read_excel(
  path = ruta_pib_usa,
  sheet = "usa_real_gdp_quarterly.csv"
)

glimpse(pib_usa_raw)
head(pib_usa_raw)

#: Ahora limpiamos nombres y tiempo

pib_usa <- pib_usa_raw |>
  janitor::clean_names() |>
  dplyr::transmute(
    fecha = as.Date(observation_date),
    pib_usa_real = as.numeric(gdpc1)
  ) |>
  dplyr::filter(
    fecha >= as.Date("2004-01-01"),
    fecha <= as.Date("2025-12-31")
  ) |>
  dplyr::arrange(fecha) |>
  dplyr::mutate(
    anio = lubridate::year(fecha),
    trimestre = lubridate::quarter(fecha),
    periodo = zoo::as.yearqtr(fecha)
  )

#visualizar:

head(pib_usa)
tail(pib_usa)
summary(pib_usa$pib_usa_real)
nrow(pib_usa)
# ============================================================
# 2. PIB REAL DE BRASIL
# ============================================================

ruta_pib_brasil <- here(
  "data",
  "raw",
  "brazil_real_gdp_quarterly.xlsx"
)

pib_brasil_raw <- read_excel(
  path = ruta_pib_brasil,
  sheet = "brazil_real_gdp_quarterly.csv"
)

glimpse(pib_brasil_raw)
head(pib_brasil_raw)

# limpiamos:
pib_brasil <- pib_brasil_raw |>
  clean_names() |>
  transmute(
    fecha = as.Date(observation_date),
    pib_brasil_real = as.numeric(ngdprsaxdcbrq)
  ) |>
  filter(
    fecha >= as.Date("2004-01-01"),
    fecha <= as.Date("2025-12-31")
  ) |>
  arrange(fecha) |>
  mutate(
    anio = year(fecha),
    trimestre = quarter(fecha),
    periodo = as.yearqtr(fecha)
  )

#verificación:
head(pib_brasil)
tail(pib_brasil)
summary(pib_brasil$pib_brasil_real)
nrow(pib_brasil)

#verificamos fechas:

anti_join(
  pib_usa |> dplyr::select(periodo),
  pib_brasil |> dplyr::select(periodo),
  by = "periodo"
)

# ------------------------------------------------------------
# 4. Unión de las dos series
# ------------------------------------------------------------

pib_externo <- pib_usa |>
  dplyr::select(
    periodo,
    anio,
    trimestre,
    pib_usa_real
  ) |>
  full_join(
    pib_brasil |>
      dplyr::select(
        periodo,
        pib_brasil_real
      ),
    by = "periodo"
  ) |>
  arrange(periodo)

# ------------------------------------------------------------
# 5. Verificaciones
# ------------------------------------------------------------

glimpse(pib_externo)
head(pib_externo)
tail(pib_externo)

print(colSums(is.na(pib_externo)))

# ------------------------------------------------------------
# 6. Guardar resultados
# ------------------------------------------------------------

write_xlsx(
  pib_externo,
  here(
    "data",
    "processed",
    "auxiliary_data",
    "pib_usa_brasil_trimestral.xlsx"
  )
)

saveRDS(
  pib_externo,
  here(
    "data",
    "processed",
    "auxiliary_data",
    "pib_usa_brasil_trimestral.rds"
  )
)

cat(
  "\nProceso finalizado correctamente.",
  "\nObservaciones PIB USA:", nrow(pib_usa),
  "\nObservaciones PIB Brasil:", nrow(pib_brasil),
  "\nObservaciones base unida:", nrow(pib_externo),
  "\n"
)

LIMPIAR_SALIDAS <- FALSE
