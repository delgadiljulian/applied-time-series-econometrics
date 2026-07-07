# ============================================================
# TP3 - SERIES DE TIEMPO
# Script 03: PIB, importaciones y exportaciones de Argentina
# ============================================================

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

ruta_comercio_arg <- here::here(
  "data",
  "raw",
  "arg_trade_data_2004_2025.xlsx"
)

if (!file.exists(ruta_comercio_arg)) {
  stop(
    "No se encontró el archivo: ",
    ruta_comercio_arg
  )
}

readxl::excel_sheets(ruta_comercio_arg)

# lectura de datos sin encabezaados:

datos_arg_raw <- readxl::read_excel(
  path = ruta_comercio_arg,
  sheet = "cuadro 1",
  col_names = FALSE,
  .name_repair = "unique"
)

tibble::tibble(
  etiqueta = datos_arg_raw[[1]]
) |>
  dplyr::filter(!is.na(etiqueta))

etiquetas_filas <- datos_arg_raw[[1]] |>
  as.character() |>
  stringr::str_squish()



# 4. Identificar años y trimestres

anios_raw <- datos_arg_raw[4, -1] |>
  unlist(use.names = FALSE) |>
  as.character() |>
  readr::parse_number()

anios <- zoo::na.locf(
  anios_raw,
  na.rm = FALSE
)

etiquetas_trimestre <- datos_arg_raw[5, -1] |>
  unlist(use.names = FALSE) |>
  as.character()

# columnas trimestrales:

columnas_trimestrales <- which(
  stringr::str_detect(
    stringr::str_to_lower(
      tidyr::replace_na(
        etiquetas_trimestre,
        ""
      )
    ),
    "trimestre"
  )
)

trimestres <- suppressWarnings(
  readr::parse_number(etiquetas_trimestre)
) |>
  as.integer()

length(columnas_trimestrales)

# 5. Crear una función para extraer cada serie

etiquetas_filas <- datos_arg_raw[[1]] |>
  as.character() |>
  stringr::str_squish()

extraer_serie_argentina <- function(
    etiqueta_fila,
    nombre_variable
) {

  fila <- which(
    etiquetas_filas == etiqueta_fila
  )

  if (length(fila) == 0) {
    stop(
      "No se encontró la fila: ",
      etiqueta_fila
    )
  }

  if (length(fila) > 1) {
    stop(
      "Se encontró más de una fila para: ",
      etiqueta_fila
    )
  }

  valores <- datos_arg_raw[fila, -1] |>
    unlist(use.names = FALSE) |>
    as.character() |>
    as.numeric()

  tibble(
    anio = as.integer(
      anios[columnas_trimestrales]
    ),
    trimestre = as.integer(
      trimestres[columnas_trimestrales]
    ),
    !!nombre_variable :=
      valores[columnas_trimestrales]
  )
}

# 6. Extraer el PIB real

pib_argentina <- extraer_serie_argentina(
  etiqueta_fila = "Producto Interno Bruto",
  nombre_variable = "pib_real"
)

head(pib_argentina)

tail(pib_argentina)

nrow(pib_argentina)

# 7. Extraer las importaciones reales

importaciones_argentina <- extraer_serie_argentina(
  etiqueta_fila =
    "Importaciones FOB (bienes y servicios reales)",
  nombre_variable =
    "importaciones_reales"
)

head(importaciones_argentina)

tail(importaciones_argentina)

nrow(importaciones_argentina)

# 8. Extraer las exportaciones reales

exportaciones_argentina <- extraer_serie_argentina(
  etiqueta_fila =
    "Exportaciones FOB (bienes y servicios reales)",
  nombre_variable =
    "exportaciones_reales"
)

head(exportaciones_argentina)

tail(exportaciones_argentina)

nrow(exportaciones_argentina)

# 9. Unir las tres series

datos_argentina <- pib_argentina |>
  dplyr::full_join(
    importaciones_argentina,
    by = c(
      "anio",
      "trimestre"
    )
  ) |>
  dplyr::full_join(
    exportaciones_argentina,
    by = c(
      "anio",
      "trimestre"
    )
  ) |>
  dplyr::arrange(
    anio,
    trimestre
  )

# fecha trimestral.

datos_argentina <- datos_argentina |>
  dplyr::mutate(
    mes_inicial = 1L + 3L * (trimestre - 1L),

    fecha = as.Date(
      sprintf(
        "%04d-%02d-01",
        anio,
        mes_inicial
      )
    ),

    periodo = zoo::as.yearqtr(fecha),

    periodo_label = format(
      periodo,
      "%Y Q%q"
    )
  ) |>
  dplyr::select(
    fecha,
    periodo,
    periodo_label,
    anio,
    trimestre,
    pib_real,
    importaciones_reales,
    exportaciones_reales
  )

View(datos_argentina)

head(datos_argentina, 8)

tail(datos_argentina, 8)

# 10. Crear los logaritmos naturales

datos_argentina <- datos_argentina |>
  dplyr::mutate(
    ln_pib_real =
      log(pib_real),

    ln_importaciones_reales =
      log(importaciones_reales),

    ln_exportaciones_reales =
      log(exportaciones_reales)
  )

#11. controles de datos:

duplicados_argentina <- datos_argentina |>
  dplyr::count(
    anio,
    trimestre
  ) |>
  dplyr::filter(n > 1)

duplicados_argentina

# faltantes:

resumen_faltantes_argentina <- datos_argentina |>
  dplyr::summarise(
    dplyr::across(
      c(
        pib_real,
        importaciones_reales,
        exportaciones_reales
      ),
      ~ sum(is.na(.x))
    )
  )

resumen_faltantes_argentina

#valores no positivos:

datos_argentina |>
  dplyr::filter(
    pib_real <= 0 |
      importaciones_reales <= 0 |
      exportaciones_reales <= 0
  )

# 13. Graficar las series argentinas

datos_argentina_long <- datos_argentina |>
  dplyr::select(
    fecha,
    `PIB real` = pib_real,
    `Importaciones reales` =
      importaciones_reales,
    `Exportaciones reales` =
      exportaciones_reales
  ) |>
  tidyr::pivot_longer(
    cols = -fecha,
    names_to = "serie",
    values_to = "valor"
  )
# gráfico:

grafico_datos_argentina <- ggplot(
  datos_argentina_long,
  aes(
    x = fecha,
    y = valor
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ serie,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    title =
      "PIB y comercio exterior real de Argentina",
    subtitle =
      "Valores trimestrales a precios constantes de 2004",
    x = NULL,
    y = NULL,
    caption =
      "Fuente: INDEC"
  ) +
  theme_minimal()

grafico_datos_argentina

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "series_reales_argentina.png"
  ),
  plot = grafico_datos_argentina,
  width = 9,
  height = 8,
  dpi = 300
)

# 14. Guardar la base procesada
saveRDS(
  datos_argentina,
  here::here(
    "data",
    "processed",
    "auxiliary_data",
    "argentina_quarterly_data_2004_2025.rds"
  )
)
# formato excel:

datos_argentina_excel <- datos_argentina |>
  dplyr::mutate(
    periodo = periodo_label
  ) |>
  dplyr::select(
    -periodo_label
  )

writexl::write_xlsx(
  datos_argentina_excel,
  path = here::here(
    "data",
    "processed",
    "auxiliary_data",
    "argentina_quarterly_data_2004_2025.xlsx"
  )
)

cat(
  "\nDatos argentinos procesados correctamente.",
  "\nObservaciones:", nrow(datos_argentina),
  "\nPeríodo inicial:",
  format(
    min(datos_argentina$periodo),
    "%Y Q%q"
  ),
  "\nPeríodo final:",
  format(
    max(datos_argentina$periodo),
    "%Y Q%q"
  ),
  "\nFaltantes PIB:",
  sum(is.na(datos_argentina$pib_real)),
  "\nFaltantes importaciones:",
  sum(
    is.na(
      datos_argentina$importaciones_reales
    )
  ),
  "\nFaltantes exportaciones:",
  sum(
    is.na(
      datos_argentina$exportaciones_reales
    )
  ),
  "\n"
)
