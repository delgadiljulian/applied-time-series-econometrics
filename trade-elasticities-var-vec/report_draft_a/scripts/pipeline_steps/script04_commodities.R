# ============================================================
# TP3 - SERIES DE TIEMPO
# Script 04: Índice de commodities del Banco Mundial
# ============================================================

library(tidyverse)
library(readxl)
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

ruta_commodities <- here::here(
  "data",
  "raw",
  "world_bank_commodity_index.xlsx"
)

if (!file.exists(ruta_commodities)) {
  stop(
    "No se encontró el archivo: ",
    ruta_commodities
  )
}

readxl::excel_sheets(ruta_commodities)


readxl::read_excel(
  path = ruta_commodities,
  sheet = "Monthly Indices",
  range = "A1:F12",
  col_names = FALSE,
  .name_repair = "unique"
)

#2. Cargar la hoja de índices mensuales

commodities_raw <- readxl::read_excel(
  path = ruta_commodities,
  sheet = "Monthly Indices",
  skip = 9,
  col_names = FALSE,
  .name_repair = "unique"
)

dim(commodities_raw)

head(commodities_raw[, 1:5])

# 3. Seleccionar el Total Index

# columnas desde donde inicia:

commodities_mensual <- tibble::tibble(
  periodo_codigo = as.character(
    commodities_raw[[1]]
  ),
  commodity_price_index = suppressWarnings(
    as.numeric(commodities_raw[[2]])
  )
)
# eliminar filas vacísa:

commodities_mensual <- commodities_mensual |>
  filter(
    !is.na(periodo_codigo),
    stringr::str_detect(
      periodo_codigo,
      "^\\d{4}M\\d{2}$"
    )
  )

head(commodities_mensual)
tail(commodities_mensual)
nrow(commodities_mensual)

# 4. Convertir el código mensual en fecha

commodities_mensual <- commodities_mensual |>
  mutate(
    anio = as.integer(
      stringr::str_sub(
        periodo_codigo,
        1,
        4
      )
    ),

    mes = as.integer(
      stringr::str_sub(
        periodo_codigo,
        6,
        7
      )
    ),

    fecha = as.Date(
      sprintf(
        "%04d-%02d-01",
        anio,
        mes
      )
    )
  ) |>
  arrange(fecha)

#comprobar:
head(
  commodities_mensual |>
    select(
      periodo_codigo,
      anio,
      mes,
      fecha,
      commodity_price_index
    )
)
# 5. Filtrar el período 2004–2025

commodities_mensual_2004_2025 <-
  commodities_mensual |>
  filter(
    fecha >= as.Date("2004-01-01"),
    fecha <= as.Date("2025-12-31")
  ) |>
  mutate(
    trimestre = lubridate::quarter(fecha),
    periodo = zoo::as.yearqtr(fecha)
  )
# revisión:
head(commodities_mensual_2004_2025)

tail(commodities_mensual_2004_2025)

range(
  commodities_mensual_2004_2025$fecha
)

nrow(commodities_mensual_2004_2025)

# 6. Controles de la serie mensual

colSums(
  is.na(
    commodities_mensual_2004_2025 |>
      select(
        fecha,
        commodity_price_index
      )
  )
)
# 7. Convertir el índice mensual a frecuencia trimestral

commodities_trimestral <-
  commodities_mensual_2004_2025 |>
  group_by(
    periodo,
    anio,
    trimestre
  ) |>
  summarise(
    commodity_price_index = mean(
      commodity_price_index,
      na.rm = TRUE
    ),

    observaciones_mensuales = n(),

    fecha_inicial = min(fecha),

    fecha_final = max(fecha),

    .groups = "drop"
  ) |>
  arrange(periodo) |>
  mutate(
    fecha = as.Date(periodo),

    periodo_label = format(
      periodo,
      "%Y Q%q"
    ),

    ln_commodity_price_index = log(
      commodity_price_index
    )
  ) |>
  select(
    fecha,
    periodo,
    periodo_label,
    anio,
    trimestre,
    commodity_price_index,
    ln_commodity_price_index,
    observaciones_mensuales,
    fecha_inicial,
    fecha_final
  )

View(commodities_trimestral)

head(commodities_trimestral, 8)

tail(commodities_trimestral, 8)

nrow(commodities_trimestral)

#10. Resumen estadístico

commodities_trimestral |>
  summarise(
    fecha_inicial = min(fecha),

    fecha_final = max(fecha),

    observaciones = n(),

    indice_minimo = min(
      commodity_price_index
    ),

    indice_promedio = mean(
      commodity_price_index
    ),

    indice_maximo = max(
      commodity_price_index
    )
  )

# graficar

grafico_commodities <- ggplot(
  commodities_trimestral,
  aes(
    x = fecha,
    y = commodity_price_index
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  labs(
    title =
      "Índice de precios de commodities",

    subtitle =
      "Total Index del Banco Mundial, promedio trimestral; 2010 = 100",

    x = NULL,

    y = "Índice",

    caption =
      "Fuente: Banco Mundial, Pink Sheet"
  ) +
  theme_minimal()

grafico_commodities

# guardar:

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "commodity_price_index_trimestral.png"
  ),

  plot = grafico_commodities,

  width = 9,

  height = 5,

  dpi = 300
)

#guardar  serie rds:

saveRDS(
  commodities_trimestral,

  here::here(
    "data",
    "processed",
    "auxiliary_data",
    "world_bank_commodity_index_quarterly_2004_2025.rds"
  )
)

# guardar excel:

commodities_excel <-
  commodities_trimestral |>
  mutate(
    periodo = periodo_label
  ) |>
  select(
    -periodo_label
  )

writexl::write_xlsx(
  commodities_excel,

  path = here::here(
    "data",
    "processed",
    "auxiliary_data",
    "world_bank_commodity_index_quarterly_2004_2025.xlsx"
  )
)

saveRDS(
  commodities_mensual_2004_2025,

  here::here(
    "data",
    "processed",
    "auxiliary_data",
    "world_bank_commodity_index_monthly_2004_2025.rds"
  )
)

# final:

cat(
  "\nÍndice de commodities procesado correctamente.",

  "\nObservaciones mensuales:",
  nrow(
    commodities_mensual_2004_2025
  ),

  "\nObservaciones trimestrales:",
  nrow(
    commodities_trimestral
  ),

  "\nPeríodo inicial:",
  format(
    min(
      commodities_trimestral$periodo
    ),
    "%Y Q%q"
  ),

  "\nPeríodo final:",
  format(
    max(
      commodities_trimestral$periodo
    ),
    "%Y Q%q"
  ),

  "\nFaltantes:",
  sum(
    is.na(
      commodities_trimestral$
        commodity_price_index
    )
  ),

  "\n"
)
